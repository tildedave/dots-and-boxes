(ns dots-and-boxes.core-test
  (:require [clojure.test :refer :all]
            [dots-and-boxes.core :refer :all]))

(deftest test-create-board
    (testing "Testing create-board functionality"
        (is (= (count (:board-array (create-board 3 3))) 81))
        (is (= (board-to-string (create-board 3 3)) "\n\n\n*-*-*\n- - -\n*-*-*\n- - -\n*-*-*\n\n"))))

(deftest test-boxes-for-idx
    ; Need to figure out how to really compare these
    (testing "Testing boxes for index"
        (is (= (seq (boxes-for-idx (create-board 3 3) 21)) (seq (set [21 39 31 29]))))
        (is (= (seq (boxes-for-idx (create-board 3 3) 31)) (seq [(set [31 23 33 41]) (set [31 21 29 39])])))))
