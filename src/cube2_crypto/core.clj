(ns cube2-crypto.core
  (:import [java.security SecureRandom]))


(defn pow [base exp]
  (let [seq (repeat exp base)]
    (reduce * seq)))

;; Base params for NIST 192-P (aka ANSI 192v1, etc etc etc) ;;

(def ecc-params {:P 16rfffffffffffffffffffffffffffffffeffffffffffffffff
                 :B 16r64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
                 :x 16r188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
                 :y 16r07192b95ffc8da78631011ed6b24cdd573f977a11e794811
                 :origin {:x 1 :y 1 :z 0}})

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

(defn with-ecc-params [params]
  (let [m (params :P)]
    (fn [f]
      (with-modulus m f))))

(let [params ecc-params
      params-f (with-ecc-params params)]
  (def modular-add (params-f +))
  (def modular-double (fn [x] (modular-add x x)))
  (def modular-sub (params-f -))
  (def modular-mult (params-f *))
  (def modular-square (fn [x] (modular-mult x x)))
  (def modular-pow (params-f pow)))

(defn jacobian-double [{:keys [x y z]}]
  (when (not (zero? z))
    (if (zero? y)
      (ecc-params :origin)
      (let [a (modular-square y)
            b (modular-double a)
            c (modular-square z)
            d (modular-sub x c)
            c' (modular-add c x)
            d' (modular-mult d c')
            
            ])
      )
    )
  ) ;; TODO ok what we're gonna do here is map this out a little
;; better from the source


(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
