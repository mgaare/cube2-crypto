(ns sauerworld.cube2.unbox)

(defprotocol Unboxable
  (unbox [x]))

(extend-protocol Unboxable
  java.math.BigInteger
  (unbox [x]
    x)
  clojure.lang.BigInt
  (unbox [x]
    x)
  java.lang.Long
  (unbox [x]
    x)
  java.lang.Integer
  (unbox [x]
    x))
