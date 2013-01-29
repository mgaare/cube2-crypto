(ns cube2-crypto.core
  (:import [java.security SecureRandom]))


(defn pow [base exp]
  (let [seq (repeat exp base)]
    (reduce * seq)))

;; Base params for NIST 192-P (aka ANSI 192v1, etc etc etc) ;;

(def ecc-params {:P 16rfffffffffffffffffffffffffffffffeffffffffffffffff
                 :B 16r64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
                 :x 16r188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
                 :y 16r07192b95ffc8da78631011ed6b24cdd573f977a11e794811
                 :origin {:x 1 :y 1 :z 0}})

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
  (def m*2 (fn [x] (modular-add x x)))
  (def m- (params-f -))
  (def m* (params-f *))
  (def m**2 (fn [x] (modular-mult x x)))
  (def m** (params-f pow)))

;; http://en.wikipedia.org/wiki/Jacobian_curve
;; http://hyperelliptic.org/EFD/g1p/auto-jquartic-xyz.html
(defn jacobian-add [{:keys [x1 y1 z1]} {:keys [x2 y2 z2]}]
  (let [a2 (m**2 x2)
        c2 (m**2 z2)
        d2 (m+ a2 c2)
        b2 (m- (modular-square (modular-add x2 z2)) d2)
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
    {:x x3 :y y3 :z z3}))


(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
