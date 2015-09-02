(ns com.manigfeald.vbuffer-test
  (:require [clojure.test :refer :all]
            [com.manigfeald.vbuffer :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(def value-buffer-gen (gen/fmap value-buffer gen/bytes))

(defspec t-buffer-equality
  1e4
  (prop/for-all [a value-buffer-gen
                 b value-buffer-gen]
                (if (= (vec (.-b a))
                       (vec (.-b b)))
                  (and (= a b)
                       (= (hash a) (hash b)))
                  (not= a b))))

(defspec t-buffer-sets
  1e4
  (prop/for-all [a (gen/list value-buffer-gen)]
                (= (count (set (map #(vec (.-b %)) a)))
                   (count (set a)))))
