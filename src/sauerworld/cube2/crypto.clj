(ns sauerworld.cube2.crypto
  (:require [crypto.random :as rand])
  (:import java.security.KeyFactory ;; check
           java.security.Security ;; check
           org.bouncycastle.jce.ECNamedCurveTable ;; check
           org.bouncycastle.jce.spec.ECPrivateKeySpec ;; check
           org.bouncycastle.jce.spec.ECPublicKeySpec ;; check
           org.bouncycastle.crypto.generators.ECKeyPairGenerator
           org.bouncycastle.math.ec.ECFieldElement$Fp
           org.bouncycastle.math.ec.ECPoint
           org.bouncycastle.math.ec.ECPoint$Fp
           org.bouncycastle.jce.provider.BouncyCastleProvider))

;;; Utility

(defprotocol IBigInteger

  "Coercion to java.math.BigInteger. For hex strings it is sign-aware."
  (to-biginteger [x] "Coerce to BigInteger"))

(extend-protocol IBigInteger
  String
  (to-biginteger [s]
    (let [sign (first s)]
      (if (contains? #{\- \+} sign)
        (let [negative? (= \- sign)]
          (cond-> (to-biginteger (.substring s 1))
                  negative? .negate))
        (BigInteger. s 16))))
  ECFieldElement$Fp
  (to-biginteger [fe]
    (.toBigInteger fe))
  ECPoint$Fp
  (to-biginteger [p]
    (to-biginteger (.getX p)))
  Long
  (to-biginteger [l]
    (biginteger l))
  BigInteger
  (to-biginteger [x]
    x))


(defn random-message
  "Generates a random message whose size matches the Curve's field size."
  [curve]
  (-> curve
      (.getCurve)
      (.getFieldSize)
      (/ 8)
      (rand/hex)
      to-biginteger))

;;; Cryptsystem setup

;; Add BouncyCastle as a provider, so KeyFactory works
(Security/addProvider (new BouncyCastleProvider))

;; Cube2auth's ECC
(def curve (ECNamedCurveTable/getParameterSpec "prime192v1"))


;;; Key-related

(defn new-private-keyspec
  "Convenience wrapper over ECPrivateKeySpec."
  [key-secret curve]
  (let [key-secret (to-biginteger key-secret)]
    (ECPrivateKeySpec. key-secret curve)))

(defn keyspec->private-key
  "Convenience wrapper over .generatePrivate"
  [keyspec ^KeyFactory factory]
  (.generatePrivate factory keyspec))

(defn new-private-key
  "Creates a new private key from pre-generated key material."
  ([key-secret curve]
     (new-private-key key-secret curve (KeyFactory/getInstance "EC" "BC")))
  ([key-secret curve factory]
     (-> key-secret
         (new-private-keyspec curve)
         (keyspec->private-key factory))))

(defn priv->pub
  "Converts a private key to a public key."
  [privkey curve]
  (-> (.getG curve)
      (.multiply (to-biginteger privkey))
      (ECPublicKeySpec. curve)))


;;; Cube2 crypto functions

(defn negative-point?
  "Checks whether a given ECPoint is \"negative\". Specifically:
  If Y is even, then it's positive.

  * Another non-standard operation in ECC."
  [^ECPoint p]
  (-> (.getY p)
      to-biginteger
      odd?))

(defn point->string
  "Converts point into a hex-string. If truthy signed? param is passed, adds
  sign based on negative-point? test."
  ([^ECPoint point]
     (point->string point false))
  ([^ECPoint point signed?]
     (-> (.getX point)
         (to-biginteger)
         (.toString 16)
         (cond->> signed?
                  (str (if (negative-point? point)
                         "-"
                         "+"))))))

(defn plot-point
  "Plots a point on the curve, given the x coordinate. Uses the formula for
   the elliptic curve, which is:
      y^2 = x^3 + ax + b

   Prime-based elliptic curves use a = -3, and provide a per-curve b param.

   * It is extremely non-standard in ECC to do this operation, but cube2 auth
     relies on it extensively."
  [x curve-param]
  (let [x (to-biginteger x)
        curve (.getCurve curve-param)
        b (.getB curve)
        negative? (< x 0)
        x (cond-> x negative? .negate)
        x-field (.fromBigInteger curve x)
        y-field (-> x-field
                    (.multiply x-field)
                    (.multiply x-field)
                    (.subtract (-> (.fromBigInteger curve (biginteger 3))
                                   (.multiply x-field)))
                    (.add b)
                    (.sqrt)
                    (cond-> negative? .negate))]
    (.createPoint curve (.toBigInteger x-field) (.toBigInteger y-field))))

(defn- generate-challenge
  "Generates message, challenge and answer, given a pubkey, curve and optional
   message. If no message is passed, generates a random one."
  ([pubkey curve message]
     (let [challenge (-> (.getG curve)
                         (.multiply message)
                         (.normalize))
           pubkey-point (plot-point (to-biginteger pubkey) curve)]
       {:message message
        :challenge challenge
        :answer (-> pubkey-point
                    (.multiply message)
                    (.normalize))})))

(defn- generate-answer
  "Generates the answer to a cube2auth challenge, using a given privkey."
  [privkey curve challenge]
  (let [challenge-point (plot-point (to-biginteger challenge) curve)]
    (-> challenge-point
        (.multiply (to-biginteger privkey))
        (.normalize))))

(defn get-pubkey
  "Returns a pubkey from a given privkey as a properly cube2-formatted string."
  [privkey]
  (-> (priv->pub privkey curve)
      (.getQ)
      (point->string :signed)))

(defn challenge
  "Generates a cube2 auth challenge, from a given pubkey and optional message.
   Returns a map with :challenge and :answer as properly cube2-formatted strings."
  ([pubkey]
     (challenge pubkey (random-message curve)))
  ([pubkey message]
     (let [{:keys [challenge answer]} (generate-challenge pubkey curve message)]
       {:challenge (point->string challenge :signed)
        :answer (point->string answer)})))

(defn answer
  "Generates a cube2 auth answer, from a given privkey and challenge. Returns
   properly cube2-formatted answer string."
  [privkey challenge]
  (-> (generate-answer privkey curve challenge)
      (point->string)))
