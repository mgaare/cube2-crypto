(ns user
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer (pprint)]
            [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [sauerworld.cube2.crypto :as crypto]
            [sauerworld.cube2.gfield :as gf]
            [sauerworld.cube2.jacobian :as j]))

(def test-key "f373de2d49584e7a16166e76b1bb925f24f0130c63ac9332")
