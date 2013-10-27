(ns sauerworld.cube2.crypto
  (:require [crypto.random]
            [sauerworld.cube2.jacobian :refer :all]
            [sauerworld.cube2.gfield :refer :all]
            [sauerworld.cube2.unbox :refer (unbox)]))

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
  (if (string? dec)
    dec
    (let [pos? (pos? dec)
          hexstr (format "%x" (biginteger dec))]
      (if pos?
        (str "+" hexstr)
        hexstr))))

(defn hex->dec [hex]
  (if (number? hex)
    hex
    (let [signed? (or (= \- (first hex)) (= \+ (first hex)))
          multiplier (if (= \- (first hex)) -1 1)
          hex-part (if signed? (rest hex) hex)
          s (map #(Integer/parseInt (str %) 16) (flatten (map char hex-part)))]
      (* multiplier
         (reduce
          (fn [d x] (+ (* 16 d) x))
          (bigint 0)
          s)))))

(defn get-random []
  (let [bit-size (ecc-params :bit-size)
        bytes (/ bit-size 8)]
    (crypto.random/hex bytes)))

(defn jacobian-from-x
  [x P]
  (let [x (make-gfield (hex->dec x) P)
        y (m+ (m- (mpow x 3) (m* x 3)) (ecc-params :B))
        y (msqrt y)]
    (make-jacobian x y 1 P)))

(defn get-sign
  [jac]
  (let [pos? (-> jac :y unbox (mod 2) zero?)]
    (if pos? 1 -1)))

(defn parse-public-key
  [key]
  (let [key-x (hex->dec key)
        negative? (< key-x 0)
        x (if negative? (* key-x -1) key-x)
        {y :y :as jacobian} (jacobian-from-x x P)]
    (if negative?
      (assoc jacobian :y (make-gfield (* (unbox y) -1) (:P y)))
      jacobian)))

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
        sign (get-sign public-jacobian)]
    (->> public-jacobian
         :x
         unbox
         (* sign)
         dec->hex)))

(defn crypt-message
  [jacobian message]
  (let [message (hex->dec message)
        crypted-j
        (-> jacobian
            (jacobian-multiply message)
            jacobian-normalize)
        sign (get-sign crypted-j)]
    (-> crypted-j
        :x
        unbox
        (* sign))))

(defn generate-challenge
  [pubkey]
  (let [jacobian-pubkey (parse-public-key pubkey)
        jacobian-base (ecc-params :base)
        message (get-random)]
    {:message (dec->hex message)
     :challenge (dec->hex (crypt-message jacobian-base message))
     :answer (dec->hex (crypt-message jacobian-pubkey message))}))

(defn generate-answer
  [privkey challenge]
  (let [key (hex->dec privkey)
        c-jacobian (parse-public-key challenge)]
    (-> c-jacobian
        (jacobian-multiply key)
        jacobian-normalize
        :x
        unbox
        dec->hex)))
