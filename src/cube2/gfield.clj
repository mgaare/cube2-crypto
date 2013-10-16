(ns cube2.gfield
  (:require [cube2.unbox :refer :all]))

(defprotocol Modular
  "Defines protocol implementing modular arithmetic."
  (m+ [x y])
  (m- [x y])
  (m* [x y])
  (mdiv2 [x])
  (mdouble [x])
  (msquare [x])
  (mpow [x p])
  (msqrt [x])
  (minverse [x]))

(defrecord GField
  [n P]

  Unboxable
  (unbox [this]
    n)

  Modular

  (m+ [this y]
    (let [y (unbox y)]
      (assoc this :n
             (mod (+ n y) P))))

  (m- [this y]
    (let [y (unbox y)]
      (assoc this :n
             (mod (- n y) P))))

  (m* [this y]
    (let [y (unbox y)]
      (assoc this :n
             (mod (* n y) P))))

  (mdiv2 [this]
    (if (zero? (mod n 2))
      (assoc this :n (/ n 2))
      (assoc this :n (-> n (+ P) (/ 2)))))

  (mdouble [this]
    (m+ this this))

  (msquare [this]
    (m* this this))

  (mpow [this exp]
    (loop [x this exp exp carry (GField. 1 P)]
      (if (= 0 exp)
        carry
        (let [low-bit (= 1 (mod exp 2))]
          (recur (msquare x)
                 (bigint (/ exp 2))
                 (if low-bit (m* carry x) carry))))))

  ;; This is the legendre/legendre-sqrt function from sauer source
  (msqrt [this]
    (let [check (mpow this (-> P (- 1) (/ 2)))]
      (cond (= 0 (unbox check)) 0
            (= 1 (unbox check)) (mpow this (-> P inc (/ 4)))
            :else nil)))

  ;; let java do the work for us
  (minverse [this]
    ;; make sure we're coerced to biginteger
    (let [x (biginteger n)
          m (biginteger P)]
      (assoc this :n
             (bigint (.modInverse x m))))))

(defn make-gfield
  [n p]
  (GField. n p))
