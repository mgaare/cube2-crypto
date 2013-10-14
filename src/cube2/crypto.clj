(ns cube2.crypto
  (:require [crypto.random]
            [cube2.jacobian]))

(defn pow [base exp]
  (let [seq (repeat exp base)]
    (reduce * seq)))

;; Base params for NIST 192-P (aka ANSI 192v1, etc etc etc) ;;

(def ecc-params {:P 16rfffffffffffffffffffffffffffffffeffffffffffffffff
                 :B 16r64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
                 :base {
                        :x 16r188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
                        :y 16r07192b95ffc8da78631011ed6b24cdd573f977a11e794811
                        :z 1}
                 :origin {:x 1 :y 1 :z 0}
                 :bit-size 192})

(defn dec->hex [dec]
  (format "%x" (biginteger dec)))

(defn hex->dec [hex]
  (let [s (map #(Integer/parseInt (str %) 16) (flatten (map char hex)))]
    (reduce
     (fn [d x] (+ (* 16 d) x))
     (bigint 0)
     s)))

(defn extended-euclidean-gcd
  "algorithm copied from
   http://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm"
  [a b]
  (if (= a 0)
    [b 0 1]
    (let [[g y x] (extended-euclidean-gcd (mod b a) a)]
      [g (- x (* y (bigint (/ b a)))) y])))

(defn modular-inverse
  "algorithm copied from same source as euclidean gcd"
  [a m]
  (let [[g x y] (extended-euclidean-gcd a m)]
    (when (= 1 g)
      (mod x m))))

(defn with-modulus [m f]
  (fn [& args]
     (mod (apply f args) m)))

(defn with-ecc-params [params]
  (let [m (params :P)]
    (fn [f]
      (with-modulus m f))))

(let [params ecc-params
      params-f (with-ecc-params params)]
  (def m+ (params-f +))
  (def m*2 (fn [x] (m+ x x)))
  (def m- (params-f -))
  (def m* (params-f *))
  (def m**2 (fn [x] (m* x x)))
  (def m** (params-f pow)))

(defn get-random []
  (let [bit-size (ecc-params :bit-size)
        bytes (/ bit-size 8)]
    (hex->dec (crypto.random/hex bytes))))

(defn big-m**
  "faster algo for big exponents"
  ([x exp]
      (let [low-bit (= 1 (mod exp 2))
            carry (if low-bit x 1)]
        (big-m** x (bigint (/ exp 2)) carry)))
  ([x exp carry]
     (if (= 0 exp)
       carry
       (let [x' (m**2 x)
             low-bit (= 1 (mod exp 2))
             carry' (if low-bit (m* carry x') carry)]
         (big-m** x' (bigint (/ exp 2)) carry')))))


;; http://en.wikipedia.org/wiki/Jacobian_curve
;; http://hyperelliptic.org/EFD/g1p/auto-jquartic-xyz.html
(defn jacobian-add [{x1 :x y1 :y z1 :z :as p} {x2 :x y2 :y z2 :z :as q}]
  (if (nil? z2)
    p
    (let [a2 (m**2 x2)
          c2 (m**2 z2)
          d2 (m+ a2 c2)
          b2 (m- (m**2 (m+ x2 z2)) d2)
          e2 (m+ b2 y2)
          a1 (m**2 x1)
          c1 (m**2 z1)
          d1 (m+ a1 c1)
          b1 (m- (m**2 (m+ x1 z1)) d1)
          e1 (m+ b1 y1)
          a1a2 (m* a1 a2)
          b1b2 (m* b1 b2)
          c1c2 (m* c1 c2)
          y1y2 (m* y1 y2 )
          f (m+ c1c2 a1a2)
          g (m*2 b1b2)
          x3 (m- (m* e1 e2) b1b2 y1y2)
          y3 (m+ (m* f (m+ (m* 4 y1y2) g)) (m* (m- (m* d1 d2) f) g))
          z3 (m*2 (m- c1c2 a1a2))]
      {:x x3 :y y3 :z z3})))

(defn jacobian-add' [{x1 :x y1 :y z1 :z :as p} {x2 :x y2 :y z2 :z :as q}]
  (if (nil? z2)
    p
    (let [a (m* x2 (m**2 z1))
          b (m* y2 (m** z1 3))
          c (m- a x1)
          d (m- b y1)
          x3 (m- (m**2 d) (m+ (m** c 3) (m* 2 x1 (m**2 c))))
          y3 (m- (m* d (m- (m* x1 (m**2 c)) x3)) (m* y1 (m** c 3)))
          z3 (m* z1 c)]
      {:x x3 :y y3 :z z3})))

(defn legendre
  "eihrul function that checks... something to do with congruence with P for
   purposes of square roots"
  [x P]
  (let [P-sub1-div2 (/ (- P 1) 2)
        res (big-m** x P-sub1-div2)]
    (cond (= 0 res) 0
          (= 1 res) 1
          true -1)))

(defn legendre-sqrt
  "another eihrul function for calculating square root (I guess) for initializing
   y from some x"
  [x P]
  (let [check (legendre x P)]
    (cond (= 0 check) 0
           (= -1 check) nil
           (= 1 check) (let [P-add1-div4 (/ (inc P) 4)]
                         (big-m** x P-add1-div4)))))

(defn jacobian-double
  "Borrowing Jacobian Projective coordinate in field Fp algorithm from:
   http://www.dkrypt.com/home/ecc"
  [{x1 :x y1 :y z1 :z}]
  (let [a (m* (m* 4 x1) (m**2 y1))
        b (m* 8 (m** y1 4))
        c (m* (m* 3 (m- x1 (m**2 z1))) (m+ x1 (m**2 z1)))
        d (m+ (m* a -2) (m**2 c))
        x3 d
        y3 (m- (m* c (m- a d)) b)
        z3 (m* (m*2 y1) z1)]
    {:x x3 :y y3 :z z3}))


(defn jacobian-mult
  "p is the point on the curve, n is the scalar multiple.
   Borrowing recursive algorithm from here:
   http://en.wikipedia.org/wiki/Elliptic_curve_point_multiplication"
  [p n]
  (if (< n 1)
    0
    (if (= 1 (mod n 2))
      (jacobian-add p (jacobian-mult p (dec n)))
      (jacobian-mult (jacobian-double p) (/ n 2)))))

(defn jacobian-normalize
  "Scales the coords so that z is 1. Uses eihrul's algorithm"
  [{x1 :x y1 :y z1 :z}]
  (let [a (modular-inverse z1 (ecc-params :P))
        b (m**2 a)
        x3 (m* x1 b)
        y3 (m* y1 a b)
        z3 1]
    {:x x3 :y y3 :z z3}))

(defn jacobian-from-x
  "equivalent to the parse function in eihrul ecjacobian"
  [x]
  (let [y2 (+ (- (m** x 3) (m* x 3)) (ecc-params :B))
        y (legendre-sqrt y2 (ecc-params :P))]
    {:x x :y y :z 1}))

(defn generate-private-key []
  (get-random))

(defn get-public-key [private-key]
  (let [base (ecc-params :base)
        key (if (string? private-key)
              (hex->dec private-key)
              private-key)]
    (-> base
        (jacobian-mult key)
        jacobian-normalize
        :x)))

(defn crypt-message
  [message jacobian]
  (-> jacobian
      (jacobian-mult message)
      :x))

(defn generate-challenge
  [message pubkey]
  (let [jacobian-pubkey (jacobian-from-x pubkey)
        jacobian-base (ecc-params :base)
        message (get-random)]
    {:message message
     :challenge (crypt-message message jacobian-base)
     :answer (crypt-message message jacobian-pubkey)}))
