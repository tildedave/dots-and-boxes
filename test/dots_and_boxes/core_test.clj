(ns dots-and-boxes.core-test
  (:require [clojure.test :refer :all]
            [dots-and-boxes.core :refer :all]))

(deftest test-create-board
    (testing "Testing create-board functionality"
        (is (= (count (:board-array (create-board 3 3))) 63))
        (is (= (board-to-string (create-board 3 3)) "\n\n\n*-*-*\n- - -\n*-*-*\n- - -\n*-*-*\n\n"))))
