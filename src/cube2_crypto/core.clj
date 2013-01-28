(ns cube2-crypto.core
  (:import [java.security SecureRandom]))

(defn extended-euclidean-gcd
  "algorithm copied from
   http://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm"
  [a b]
  (if (= a 0)
    [b 0 1]
    (let [[g y x] (extended-euclidean-gcd (mod b a) a)]
      [g (- x (* y (bigint (/ b a)))) y])))

(defn modular-inverse
  "algorithm copied from same source as euclidean gcd"
  [a m]
  (let [[g x y] (extended-euclidean-gcd a m)]
    (when (= 1 g)
      (mod x m))))

(defn with-modulus [m f]
  (fn [& args]
     (mod (apply f args) m)))



(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
