(ns sauerworld.cube2.jacobian
  (:require [sauerworld.cube2.gfield :refer :all]
            [sauerworld.cube2.unbox :refer (unbox)]))

(defprotocol Jacobian
  "Defines protocol implementing Jacobian curve operations."

  (jacobian-add [p q])

  (jacobian-double [p])

  (jacobian-multiply [p n])

  (jacobian-normalize [p]))

(defrecord JacobianPoint
  [x y z]

  Jacobian

  ;; Basically copied from Morgan Borman's python implementation, but skipping
  ;; a few senseless "optimizations" carried over from sauer source
  (jacobian-add [this q]
    (let [{qx :x qy :y qz :z} q]
      (cond
       (nil? qz) this
       (zero? (unbox z)) q
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
         (if (zero? (unbox a))
           (if (zero? (unbox b))
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

(defn make-jacobian
  [x y z p]
  (let [[x y z] (map #(-> % unbox (make-gfield p)) [x y z])]
    (JacobianPoint. x y z)))
