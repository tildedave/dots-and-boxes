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
        (is (= (vec (boxes-for-idx (create-board 3 3) 21)) [#{21 39 31 29}]))
        (is (= (seq (boxes-for-idx (create-board 3 3) 31)) [#{31 21 29 39} #{31 23 33 41}]))))

(deftest test-make-move-handles-completed-box
    (testing "Make move handles completed box"
        (true? (:player-one-to-move
            (make-move
                (make-move
                    (make-move
                        (make-move
                            (make-move
                                (make-move
                                    (make-move
                                        (create-board 3 3)
                                        1 21) 2 59) 1 29)  2 57) 1 31) 2 47) 1 39)))))

