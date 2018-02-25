# cube2.crypto

cube2.crypto is a small Clojure library for interoperating with [Cube 2: Sauerbraten](http://www.sauerbraten.org)'s Auth functionality, based on Elliptic Curve Cryptography.

Morgan Borman has a build of the C++ Sauerbraten library with some useful utility functions here: [cube2crypto](https://github.com/MorganBorman/cube2crypto)

## Installation

Add the following dependency to your `project.clj` file:

    [sauerworld/cube2.crypto "1.0.0"]

## Usage

```clojure
(ns auth
  (:require [sauerworld.cube2.crypto as crypto]))

;; create private key
(def private-key (crypto/make-privkey)

;; generate public key
(def public-key (crypto/get-pubkey private-key))

;; generate an auth challenge
(def chal (crypto/challenge public-key))

;; generate an answer to the challenge
(def answer (crypto/answer private-key (:challenge chal)))

;; they should match
(= answer (:answer chal))
```

## License

Copyright © 2014 Michael Gaare

Distributed under the Eclipse Public License, the same as Clojure.
