# cube2-crypto

A pure clojure implementation of the crypto functions used for [Cube 2: Sauerbraten](http://www.sauerbraten.org)'s auth. 

Morgan Borman has a build of the C++ Sauerbraten library with some useful utility functions here: [cube2crypto](https://github.com/MorganBorman/cube2crypto)

## Usage

```clojure
(ns auth
  (:require [cube2.crypto as crypto]))

;; create private key - 192 bits is the only supported number in sauer
(def private-key crypto/generate-private-key 192)

;; generate public key
(def public-key (crypto/get-public-key private-key))
```

## License

Copyright © 2013 Michael Gaare

Distributed under the Eclipse Public License, the same as Clojure.
