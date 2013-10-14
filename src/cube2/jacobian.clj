(ns cube2.jacobian)

(defprotocol Modular
  "Defines protocol implementing modular arithmetic."
  (m+ [x y])
  (m*2 [x])
  (m- [x y])
  (m* [x y])
  (mdiv2 [x])
  (mdouble [x])
  (msquare [x])
  (mpow [x p])
  (msqrt [x])
  (minverse [x]))

(defprotocol Jacobian
  "Defines protocol implementing Jacobian curve operations."

  (jacobian-add [p q])

  (jacobian-double [p])

  (jacobian-multiply [p n])

  (jacobian-normalize [p]))

(defrecord GField
  [n P]

  Modular

  (m+ [this y]
    (assoc this :n
           (mod (+ n y) P)))

  (m*2 [this]
    (assoc this :n
           (m+ n n)))

  (m- [this y]
    (assoc this :n
           (mod (- n y) P)))

  (m* [this y]
    (assoc this :n
           (mod (* n y) P)))

  (mdiv2 [this]
    (if (zero? (mod n 2))
      (assoc :this n (/ n 2))
      (assoc :this n (-> n (m+ P) (/ 2)))))

  (mdouble [this]
    (assoc this :n
           (m+ n n)))

  (msquare [this]
    (assoc this :n
           (m* n n)))

  (mpow [this exp]
    (loop [x this exp exp carry (GField. 1 P)]
      (if (= 0 exp)
        (assoc this :n carry)
        (let [low-bit (= 1 (mod exp 2))]
          (recur (msquare x)
                 (bigint (/ exp 2))
                 (if low-bit (m* carry x) carry))))))

  ;; This is the legendre/legendre-sqrt function from sauer source
  (msqrt [this]
    (let [check (mpow this (-> P (- 1) (/ 2)))]
      (cond (= 0 check) 0
            (= 1 check) (mpow this (-> P inc (/ 4)))
            :else nil)))

  ;; let java do the work for us
  (minverse [this]
    ;; make sure we're coerced to biginteger
    (let [x (biginteger n)
          m (biginteger P)]
      (assoc this :n
             (.modInverse x m)))))

(defrecord JacobianPoint
  [x y z P B]

  Jacobian

  ;; Basically copied from Morgan Borman's python implementation, but skipping
  ;; a few senseless "optimizations" carried over from sauer source
  (jacobian-add [this q]
    (let [{qx :x qy :y qz :z} q]
      (cond
       (zero? qz) this
       (zero? z) q
       :else
       (let [[a b c d e f] (repeat 0)
             a (msquare z)
             b (-> qy (m* a) (m* z))
             a (m* a qx)
             ;; here is where we skip one unnecessary optimization
             e (msquare qz)
             f (-> y (m* e) (m* qz))
             e (m* e x)
             c (m+ e a)
             d (m+ f b)
             a (m- e a)
             b (m- f b)]
         (if (zero? a)
           (if (zero? b)
             (jacobian-double this)
             (assoc this :x 1 :y 1 :z 0))
           ;; another silly optimization skipped
           (let [z' (m* z qz)
                 z' (m* z' a)
                 e (msquare a)
                 f (m* c e)
                 x' (- (msquare b) f)
                 e (-> e (m* a) (m* d))
                 y' (-> f (m- x) (m- x) (m* b) (m- e))
                 y' (mdiv2 y')]
             (assoc this :x x' :y y' :z z')))))))

  ;; Algorithm taken from http://dkrypt.com/home/ecc
  (jacobian-double [this]
    (let [a (-> (msquare y) (m* (m* x 4)))
          b (-> (mpow y 4) (m* 8))
          c (m* (->> (msquare z) (m- x) (m* 3)) (-> (msquare z) (m+ x)))
          d (m+ (m* a -2) (msquare c))
          x' d
          y' (-> (m- a d) (m* c) (m- b))
          z' (-> (mdouble y') (m* z))]
      (assoc this :x x' :y y' :z z')))

  ;; borrowing recursive algorithm from:
  ;; http://en.wikipedia.org/wiki/Elliptic_curve_point_multiplication
  (jacobian-multiply [this n]
    (if (< n 1)
      0
      (if (= 1 (mod n 2))
        (jacobian-add this (jacobian-multiply this (dec n)))
        (jacobian-multiply (jacobian-double this) (/ n 2)))))

  ;; scales coords to z = 1. shamelessly stolen from eihrul.
  (jacobian-normalize [this]
    (let [a (minverse z)
          b (msquare a)
          x' (m* x b)
          y' (-> y (m* a) (m* b))
          z' 1]
      (assoc this :x x' :y y' :z z'))))
