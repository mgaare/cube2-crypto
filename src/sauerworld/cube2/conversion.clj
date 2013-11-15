(ns sauerworld.cube2.conversion)

(defn dec->hex [dec]
  (if (string? dec)
    dec
    (format "%x" (biginteger dec))))

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

(defprotocol BigIntAble
  (to-bigint [x]))

(extend-protocol BigIntAble
  String
  (to-bigint [s]
    (hex->dec s))
  java.math.BigInteger
  (to-bigint [x]
    bigint)
  clojure.lang.BigInt
  (to-bigint [x]
    x)
  java.lang.Long
  (to-bigint [x]
    bigint x)
  java.lang.Integer
  (to-bigint [x]
    bigint x))
