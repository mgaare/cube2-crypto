(ns sauerworld.cube2.crypto
  (:require [crypto.random]
            [sauerworld.cube2.conversion :refer :all]
            [sauerworld.cube2.jacobian :refer :all]
            [sauerworld.cube2.gfield :refer :all]))

;; Base params for NIST 192-P (aka ANSI 192v1, etc etc etc) ;;

(def P 16rfffffffffffffffffffffffffffffffeffffffffffffffff)

(defn assoc-with
  [m k f]
  (assoc m k (f m)))

(defn make-ecc-params
  [{:keys [P B base-x base-y bit-size] :as params}]
  (-> params
      (assoc :P (to-bigint P))
      (assoc :bit-size bit-size)
      (assoc-with :B (fn [m]
                       (to-gfield B (:P m))))
      (assoc-with :base (fn [m]
                          (make-jacobian base-x base-y 1 m)))))

(def ecc-params
  (make-ecc-params {:P "fffffffffffffffffffffffffffffffeffffffffffffffff"
                    :B "64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1"
                    :base-x "188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012"
                    :base-y "07192b95ffc8da78631011ed6b24cdd573f977a11e794811"
                    :origin {:x 1 :y 1 :z 0}
                    :bit-size 192}))

(comment
  {:P P
   :B (to-gfield
       16r64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
       P)
   :base (make-jacobian
          16r188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
          16r07192b95ffc8da78631011ed6b24cdd573f977a11e794811
          1
          P)
   :origin {:x 1 :y 1 :z 0}
   :bit-size 192})

(defn get-random [bits]
  (let [bytes (int (/ bits 8))]
    (crypto.random/hex bytes)))

(defn generate-private-key [bits]
  (get-random bits))

(defn crypt-message
  [jacobian message & {signed :signed :or {:signed false}}]
  (let [crypted-j
        (-> jacobian
            (jacobian-multiply message)
            jacobian-normalize)
        cypher-text (str crypted-j)]
    (if signed
      (let [sign (if (jacobian-negative? crypted-j)
                   "-"
                   "+")]
        (str sign cypher-text))
      cypher-text)))

(defn get-public-key*
  [private-key {base :base :as ecc-p}]
  (let [public-jacobian (-> base
                            (jacobian-multiply private-key)
                            jacobian-normalize)
        negative? (jacobian-negative? public-jacobian)
        sign (if negative? "-" "+")]
    (str sign public-jacobian)))

(defn generate-challenge*
  [pubkey {:keys [base bit-size] :as ecc-p}]
  (let [jacobian-pubkey (make-jacobian pubkey ecc-p)
        message (get-random bit-size)]
    {:message (dec->hex message)
     :challenge (crypt-message base message :signed true)
     :answer (crypt-message jacobian-pubkey message)}))

(defn generate-answer*
  [privkey challenge {:as ecc-p}]
  (let [c-jacobian (make-jacobian challenge ecc-p)]
    (-> c-jacobian
        (jacobian-multiply privkey)
        jacobian-normalize
        str)))

(defn with-ecc-params
  [ecc-params f]
  (fn [& params]
    ;; if the function is called with other ecc-params, we want to drop them
    ;; in favor of the ones set in with-ecc-params
    (let [drop-last? (and (map? (last params))
                          ((some-fn :P :B :base) (last params)))
          params (if drop-last?
                   (drop-last params)
                   params)
          params-w-ecc (conj (vec params) ecc-params)]
      (apply f (conj (vec params) ecc-params)))))

(def get-public-key
  (with-ecc-params ecc-params get-public-key*))

(def generate-challenge
  (with-ecc-params ecc-params generate-challenge*))

(def generate-answer
  (with-ecc-params ecc-params generate-answer*))
