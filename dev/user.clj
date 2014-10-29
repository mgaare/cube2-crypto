(ns user
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer (pprint)]
            [clojure.tools.namespace.repl :refer (refresh refresh-all)]))

(def test-key "f373de2d49584e7a16166e76b1bb925f24f0130c63ac9332")


;; Notes:
;; Cube2 auth process:
;; generate-challenge
;; - random 192 bit message created
;; - challenge is messsage multiplied by the curve base point, normalized, checked for positivity, hex
;; - answer is the message multiplied by the pubkey (turned into point), normalized, unchecked, hex
;;
;; generate-answer
;; - turn the challenge into point
;; - multiply by privkey
;; - hex str, unchecked
;;
;; Methods:
;; - Positivity check:
;;   An EC Point is positive if Y is even
;;
;; - How cube2 turns int into ec point
;;   - int checked for negativity, then abs, becomes x
;;   - y is x multipled by a bunch of stuff, added to B (ecc param), sqrt, multiplied by -1 if int was neg
;;   - z is 1
