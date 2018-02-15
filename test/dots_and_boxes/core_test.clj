(ns dots-and-boxes.core-test
  (:require [clojure.test :refer :all]
    [dots-and-boxes.core :refer :all]))

(deftest test-create-board
    (testing "Testing create-board functionality"
        (is (= (count (:board-array (create-board 3 3))) 81))
        (is (= (:available-moves (create-board 3 3)) #{59 39 21 31 33 41 29 51 23 47 57 49}))))

(deftest test-boxes-for-idx
    (testing "Testing boxes for index"
        (is (= (vec (boxes-for-idx (create-board 3 3) 21)) [#{21 39 31 29}]))
        (is (= (vec (boxes-for-idx (create-board 3 3) 31)) [#{31 21 29 39} #{31 23 33 41}]))))

(defn apply-move-list [board move-list]
    (if (empty? move-list) board (recur (apply make-move board (first move-list)) (rest move-list))))

(deftest test-make-move
    (testing "make move removes existing move"
        (is (true? (contains? (:available-moves (create-board 3 3)) 21))
            "create-board showed 21 as an available move")
        (is (false? (contains? (:available-moves (make-move (create-board 3 3) 1 21)) 21))
            "making a move at spot 21 removed it from being an available move")
        (is (true? (contains? (:available-moves (make-move (create-board 3 3) 1 21)) 29))
            "making a move at spot 21 didn't clobber other available moves"))
    (testing "make move handles completed box"
        (let [completed-moves [[1 21] [2 59] [1 29] [2 57] [1 31] [2 47] [1 39]]
              board-with-completed-box (apply-move-list (create-board 3 3) completed-moves)]
            (is (true? (:player-one-to-move board-with-completed-box)) "player one got a bonus move after completing a box")
            (is (= (:score board-with-completed-box) (hash-map 1 1 2 0)) "player one got credit for completing a box"))))
