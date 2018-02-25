(ns sauerworld.cube2.crypto-test
  (:require [clojure.test :refer :all]
            [sauerworld.cube2.crypto :refer :all]))

(def authkeys [{:private "f373de2d49584e7a16166e76b1bb925f24f0130c63ac9332"
                :public "+2c1fb1dd4f2a7b9d81320497c64983e92cda412ed50f33aa"}
               {:private "0978245f7f243e61fca787f53bf9c82eabf87e2eeffbbe77"
                :public "-afe5929327bd76371626cce7585006067603daf76f09c27e"}
               {:private "935f7b951c132951527ab541ffc5b8bff258c1e88414ab2a"
                :public "-d954ee56eddf2b71e206e67d48aaf4afe1cc70f8ca9d1058"}
               {:private "f6295aa51aca7f511c441e754830cf0d951a2078cbf881d9"
                :public "-454c98466c45fce242724e6e989bdd9f841304a1fcba4787"}
               {:private "e9ee7bf32f60110b2a0355ccbf120404307de5ee72a41417"
                :public "+15fda493cb1095ca40f652b0d208769bd42b9e234e48d1a8"}
               {:private "8ef7537b1e631ca7c30a4fe8f70d61b7f2589c9ba1f97b0f"
                :public "+643d99cb21178557f4e965eb6dc1ec1e4f57b3b05375fafb"}])

(deftest privkey-conversion
  (testing "converting privkey to pubkey should have correct ouptut"
    (doall
     (map #(is (= (:public %) (get-pubkey (:private %)))) authkeys))))

(deftest challenge-and-answer
  (testing "answer should match expected answer"
    (doall
     (map (fn [k]
            (let [{c :challenge a :answer} (challenge (:public k))]
              (is (= a (answer (:private k) c)))))
          authkeys))))

(deftest new-key
  (testing "challenge and answer should match with new key"
    (let [privkey (make-privkey)
          pubkey (get-pubkey privkey)
          c (challenge pubkey)]
      (is (= (:answer c) (answer privkey (:challenge c)))))))
