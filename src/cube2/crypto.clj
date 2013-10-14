(ns cube2.crypto
  (:require [crypto.random]
            [cube2.jacobian :refer :all]
            [cube2.gfield :refer :all]))

;; Base params for NIST 192-P (aka ANSI 192v1, etc etc etc) ;;

(def P 16rfffffffffffffffffffffffffffffffeffffffffffffffff)

(def ecc-params
  {:P P
   :B (make-gfield
       16r64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
       P)
   :base (make-jacobian
          16r188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
          16r07192b95ffc8da78631011ed6b24cdd573f977a11e794811
          1
          P)
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

(defn get-random []
  (let [bit-size (ecc-params :bit-size)
        bytes (/ bit-size 8)]
    (hex->dec (crypto.random/hex bytes))))

(defn jacobian-from-x
  "equivalent to the parse function in eihrul ecjacobian"
  [x]
  (let [x (make-gfield x P)
        y2 (m+ (m- (mpow x 3) (m* x 3)) (ecc-params :B))
        y (msqrt y2)]
    (make-jacobian x y 1 P)))

(defn generate-private-key []
  (get-random))

(defn get-public-key [private-key]
  (let [base (ecc-params :base)
        key (if (string? private-key)
              (hex->dec private-key)
              private-key)]
    (-> base
        (jacobian-multiply key)
        jacobian-normalize
        :x)))

(defn crypt-message
  [message jacobian]
  (-> jacobian
      (jacobian-multiply message)
      :x))

(defn generate-challenge
  [message pubkey]
  (let [jacobian-pubkey (jacobian-from-x pubkey)
        jacobian-base (ecc-params :base)
        message (get-random)]
    {:message message
     :challenge (crypt-message message jacobian-base)
     :answer (crypt-message message jacobian-pubkey)}))
