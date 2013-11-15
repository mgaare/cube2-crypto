(ns sauerworld.cube2.jacobian
  (:require [sauerworld.cube2.gfield :refer :all]
            [sauerworld.cube2.conversion :refer :all]))

(defprotocol Jacobian
  "Defines protocol implementing Jacobian curve operations."

  (jacobian-add [p q])

  (jacobian-double [p])

  (jacobian-multiply [p n])

  (jacobian-normalize [p])

  (jacobian-negative? [p])

  (jacobian-get-x [p]))

(defrecord JacobianPoint
  [x y z]

  Object
  (toString [this]
    (.toString x))

  BigIntAble
  (to-bigint [this]

    )

  Jacobian

  ;; Basically copied from Morgan Borman's python implementation, but skipping
  ;; a few senseless "optimizations" carried over from sauer source
  (jacobian-add [this q]
    (let [{qx :x qy :y qz :z} q]
      (cond
       (nil? qz) this
       (zero? (to-bigint z)) q
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
         (if (zero? (to-bigint a))
           (if (zero? (to-bigint b))
             (jacobian-double this)
             (assoc this :x 1 :y 1 :z 0))
           ;; another silly optimization skipped
           (let [z' (m* z qz)
                 z' (m* z' a)
                 e (msquare a)
                 f (m* c e)
                 x' (m- (msquare b) f)
                 e (-> e (m* a) (m* d))
                 y' (-> f (m- x') (m- x') (m* b) (m- e))
                 y' (mdiv2 y')]
             (assoc this :x x' :y y' :z z')))))))

  ;; Algorithm taken from http://dkrypt.com/home/ecc
  (jacobian-double [this]
    (let [a (-> (msquare y) (m* (m* x 4)))
          b (-> (mpow y 4) (m* 8))
          c (m* (-> (m- x (msquare z)) (m* 3)) (-> (msquare z) (m+ x)))
          d (m+ (m* a -2) (msquare c))
          x' d
          y' (-> (m- a d) (m* c) (m- b))
          z' (-> (mdouble y) (m* z))]
      (assoc this :x x' :y y' :z z')))

  ;; borrowing recursive algorithm from:
  ;; http://en.wikipedia.org/wiki/Elliptic_curve_point_multiplication
  (jacobian-multiply [this n]
    (let [n (to-bigint n)]
      (if (< n 1)
        0
        (if (= 1 (mod n 2))
          (jacobian-add this (jacobian-multiply this (dec n)))
          (jacobian-multiply (jacobian-double this) (/ n 2))))))

  ;; scales coords to z = 1. shamelessly stolen from eihrul.
  (jacobian-normalize [this]
    (let [a (minverse z)
          b (msquare a)
          x' (m* x b)
          y' (-> y (m* a) (m* b))
          z' 1]
      (assoc this :x x' :y y' :z z')))

  (jacobian-negative? [this]
    (-> y to-bigint (mod 2) zero? not))

  (jacobian-get-x [this]
    (-> x to-bigint)))

(defn make-jacobian
  ([x {:keys [P B] :as ecc-params}]
     (let [x (to-bigint x)
           negative? (< x 0)
           x (if negative? (* -1 x) x)
           gx (to-gfield x P)
           gy (m+ (m- (mpow gx 3) (m* gx 3)) B)
           gy (msqrt gy)
           gy (if negative? (* -1 gy) gy)]
       (make-jacobian gx gy 1 ecc-params)))
  ([x y z {:keys [P B] :as ecc-params}]
     (let [[x y z] (map #(-> % to-bigint (to-gfield P)) [x y z])]
       (JacobianPoint. x y z))))
