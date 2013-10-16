(ns cube2.crypto
  (:require [crypto.random]
            [cube2.jacobian :refer :all]
            [cube2.gfield :refer :all]
            [cube2.unbox :refer (unbox)]))

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
  (if (number? hex)
    hex
    (let [s (map #(Integer/parseInt (str %) 16) (flatten (map char hex)))]
      (reduce
       (fn [d x] (+ (* 16 d) x))
       (bigint 0)
       s))))

(defn get-random []
  (let [bit-size (ecc-params :bit-size)
        bytes (/ bit-size 8)]
    (hex->dec (crypto.random/hex bytes))))

(defn jacobian-from-x
  [x P]
  (let [x (make-gfield (hex->dec x) P)
        y (m+ (m- (mpow x 3) (m* x 3)) (ecc-params :B))
        y (msqrt y)]
    (make-jacobian x y 1 P)))

(defn parse-public-key
  [key]
  (let [sign (first key)
        negative? (= \- sign)
        key-x (->> key rest (apply str) hex->dec)
        {y :y :as jacobian} (jacobian-from-x key-x P)]
    (if (= negative?
           (not (zero? (mod (unbox y) 2))))
      jacobian
      (assoc jacobian :y (make-gfield (* y -1) P)))))

(defn parse-private-key
  [key]
  (-> key hex->dec (make-gfield P)))

(defn generate-private-key []
  (get-random))

(defn get-public-key [private-key]
  (let [base (ecc-params :base)
        key (hex->dec private-key)
        public-jacobian (-> base
                            (jacobian-multiply key)
                            jacobian-normalize)
        sign (if (-> public-jacobian :y unbox (mod 2) zero?)
               "+" "-")]
    (->> public-jacobian :x unbox dec->hex (str sign))))

(defn crypt-message
  [jacobian message]
  (-> jacobian
      (jacobian-multiply message)
      jacobian-normalize
      :x
      unbox))

(defn generate-challenge
  [pubkey]
  (let [jacobian-pubkey (parse-public-key pubkey)
        jacobian-base (ecc-params :base)
        message (get-random)]
    {:message message
     :challenge (crypt-message jacobian-base message)
     :answer (crypt-message jacobian-pubkey message)}))

(defn generate-answer
  [privkey challenge]
  (let [key (hex->dec privkey)
        c-jacobian (jacobian-from-x challenge P)]
    (-> c-jacobian
        (jacobian-multiply key)
        jacobian-normalize
        :x
        unbox)))
