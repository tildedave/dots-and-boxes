(ns dots-and-boxes.core
  (:require [clojure.set :refer :all])
  (:gen-class))

; Dots and boxes is played on N x N grid
; We represent 'out of bounds' with dummy rows that contain "X" items
;
; A 3x3 grid will be internally represented as:
; XXXXXXXXX   0  1  2  3  4  5  6  7  8
; XXXXXXXXX   9 10 11 12 13 14 15 16 17
; XX*-*-*XX  18 19 20 21 22 23 24 25 26
; XX- - -XX  27 28 29 30 31 32 33 34 35
; XX*-*-*XX  36 37 38 39 40 41 42 43 44
; XX- - -XX  45 46 47 48 49 50 51 52 53
; XX*-*-*XX  54 55 56 57 58 59 60 61 62
; XXXXXXXXX  63 64 65 66 67 68 69 70 71
; XXXXXXXXX  72 73 74 75 76 77 78 79 80
;
; 21's "pairs" in box life are 21, 29, 31, 39
; 31 is in two boxes: [31 21 29 39] and [31 23 33 41]

(defrecord Board [height width board-array available-moves player-to-move score])

(defn length-row [width] (+ (* 2 width) 3))

(defn create-dummy-row [width]
    (repeat (length-row width) "X"))

(defn create-horizontal-row [width]
    (let [len (length-row width)]
        (map
            #(cond
            (<= %1 1) "X"
            (>= %1 (- len 2)) "X"
            (zero? (mod %1 2)) "*"
            :else "-")
            (range len))))

(defn create-vertical-row [width]
    (let [len (length-row width)]
        (map
            #(cond
            (<= %1 1) "X"
            (>= %1 (- len 2)) "X"
            (zero? (mod %1 2)) "-"
            :else " ")
            (range len))))

(defn create-board [height width]
    (let [arr
        (vec
            (concat
            (create-dummy-row width)
            (create-dummy-row width)
            (apply concat (repeat (- height 1)
                (concat
                    (create-horizontal-row width)
                    (create-vertical-row width))))
            (create-horizontal-row width)
            (create-dummy-row width)
            (create-dummy-row width)))
        moves (set (keep-indexed #(if (= (nth arr %1) "-") %2) (range (count arr))))]
    (->Board height width arr moves 1 (hash-map 1 0 2 0))))

(defn board-range [board]
    (range (count (:board-array board))))


; Board formatting functions follow

(defn- horizontal-row-to-string [board y spacing-between-stars]
    (let [width (:width board)]
        (str
            (apply str
                (map
                    (fn [x]
                        ; horizontal row
                        (let [idx (+ 1 (* (inc x) 2) (* (length-row width) (+ 2 (* y 2))))
                              board-contents (nth (:board-array board) idx)]
                            (str
                                "* "
                                (apply str (repeat (- (/ spacing-between-stars 2) 1) "-"))
                                (if (= board-contents "-")
                                    (format "  %3d   " idx)
                                    (format "%3d (%d) " idx board-contents))
                                (apply str (repeat (- (/ spacing-between-stars 2) 1) "-"))
                                " ")))
                    (range (dec width))))
            "*\n")))

(defn- vertical-filler-row-to-string [board spacing-between-stars]
    (let [width (:width board)]
        (str
            (apply str
                (map
                    (fn [x]
                        ; vertical row
                        (str
                            "|"
                            (apply str (repeat (+ 8 spacing-between-stars) " "))))
                    (range (dec width))))
            "|\n")))

(defn- vertical-number-display [board x y]
    (let [width (:width board)
          idx (+ (* (inc x) 2) (* (length-row width) (+ 2 (inc (* y 2)))))
          board-contents (nth (:board-array board) idx)]
        (if (= board-contents "-")
            (format "%-3d    " idx)
            (format "%-3d (%d)" idx board-contents))))

(defn- vertical-number-row-to-string [board y spacing-between-stars]
    (let [width (:width board)]
        (str
            (apply str
                (map
                    (fn [x]
                        ; vertical row
                        (str
                            (vertical-number-display board x y)
                            (apply str (repeat (+ 2 spacing-between-stars) " "))))
                    (range (dec width))))
            (vertical-number-display board (dec width) y)
            "\n")))

(defn board-to-string [board]
    (let [height (:height board)
          width (:width board)
          column-width 100
          spacing-between-stars (- (/ column-width (:width board)) 2)]
          (str
              (apply str
                  (map
                    (fn [y]
                        (str
                            (horizontal-row-to-string board y spacing-between-stars)
                            (apply str
                                ; magic number to make the REPL look better
                                ; this won't work for different font sizes
                                ; +1 because I need a midpoint to put the # at, so it needs to be odd
                                (let [magic-threshold (inc (quot spacing-between-stars 4))]
                                    (map
                                        (fn [visual-y]
                                        ; vertical rows
                                        ; let's say spacing-between-stars is 5
                                            (cond
                                                (= visual-y (quot (dec magic-threshold) 2))
                                                    (vertical-number-row-to-string board y spacing-between-stars)
                                                :else (vertical-filler-row-to-string board spacing-between-stars)))
                                        (range magic-threshold))))
                            ))
                    (range (dec height))))
                (horizontal-row-to-string board (dec height) spacing-between-stars)
              )))

(defn is-horizontal-idx [board idx]
    ; Is the index on the horizontal row?
    (let [len (length-row (:width board))]
        (< (mod idx (* 2 len)) len)))

; Board partners:
; * horizontal indices go up 2 rows and down 2 rows, then down 1 row
; * vertical indices go up/down 1 row and left/right,

(defn is-legit-box [board box]
    ; This test relies on the 'X' padding in the board representation
    (every?
        (fn [idx]
            (and
                (not= (nth (:board-array board) idx) "X")
                (not= (nth (:board-array board) idx) " ")))
        box))

(defn horizontal-box-partners [board idx]
    ; Returns a sequence of sets of box partners for a given index
    (let [len (length-row (:width board))
          up-box (set [idx (+ idx (- len) (- len)) (+ idx (- len) 1) (+ idx (- len) -1)])
          down-box (set [idx (+ idx len len) (+ idx len 1) (+ idx len -1)])]
          (filter (partial is-legit-box board) [up-box down-box])))

(defn vertical-box-partners [board idx]
    ; Returns a sequence of sets of box partners for a given index
    (let [len (length-row (:width board))
          left-box (set [idx (+ idx (- len) -1) (+ idx -2) (+ idx len -1)])
          right-box (set [idx (+ idx (- len) 1) (+ idx +2) (+ idx len 1)])]
          (filter (partial is-legit-box board) [left-box right-box])))

(defn boxes-for-idx [board idx]
    (if (is-horizontal-idx board idx)
        (horizontal-box-partners board idx)
        (vertical-box-partners board idx)))

(defn fill-line [board player idx]
    ; Update the state of the board (everything)
    (update-in
        (update-in board [:board-array] (fn [arr] (assoc arr idx player)))
        [:available-moves]
        (fn [moves] (difference moves #{idx}))))

(defn has-completed-box [board player box]
    ; This test relies on us filling in the player number in available indices
    (every? (fn [idx] (= (nth (:board-array board) idx) player)) box))

(defn other-player [player]
    (if (= player 1) 2 1))

(defn make-move [board player idx]
    ; 1) fill in the line for the player
    ; 2) determine if this means the box is now filled by that player
    ;    ...if so, don't toggle whose turn to move it is
    ;
    (cond
        (not= (nth (:board-array board) idx) "-")
            (throw (AssertionError. (str "Board was not empty at spot " idx ": " (nth (:board-array board) idx))))
        (not= (:player-to-move board) player)
            (throw (AssertionError. (str "Incorrect player attempted to make move: " player " (expected " (:player-to-move board) ")")))
        :else
            (let [new-board (fill-line board player idx)
                  completed-boxes (filter (partial has-completed-box new-board player) (boxes-for-idx new-board idx))]
                  (if (> (count completed-boxes) 0)
                    (update-in new-board [:score]
                        (fn [score] (update score (:player-to-move new-board) (partial + (count completed-boxes)))))
                    (update-in new-board [:player-to-move] (fn [player-to-move] (other-player player-to-move)))))))

(defn choose-move-random-strategy [board]
    (rand-nth (seq (:available-moves board))))

(defn is-game-complete [board]
    ; TODO: be smart about 'no more boxes can be completed' to prune tree depth
    (= (count (:available-moves board)) 0))

(defn game-outcome [board player]
    (let [score (:score board)
          player-score (get score player)
          other-player-score (get score (other-player player))]
          (compare player-score other-player-score)))

(defrecord CandidateMove [kind score move nodes depth])

(defn choose-move-minimax [board player depth]
    (cond
        (is-game-complete board)
        (->CandidateMove 'completed-game
            (case (game-outcome board player)
                ; stupid way to incentivize "running up the score" in a won position
                ; if the outcome is 1000 (I win) they should go for more points
                -1  (- 1000 (get (:score board) player))
                1   (+ 1000 (get (:score board) player))
                0   0)
            nil
            1
            0)
        (= depth 0)
            (let [score (:score board)]
                (->CandidateMove 'incomplete-game (- (get score player) (get score (other-player player))) nil 1 0))
        :else
            (reduce
                (fn [best-so-far result-move]
                    (update-in
                        (if (> (:score result-move) (:score best-so-far))
                            result-move
                            best-so-far)
                        [:nodes]
                        (fn [nodes]
                            (+ (:nodes best-so-far) (:nodes result-move)))))
                (->CandidateMove 'dummy-value -100000 nil 0 0)
                (map
                    (fn [move]
                        (let [new-board (make-move board player move)
                              new-to-move (:player-to-move new-board)
                              is-different-player (= (:player-to-move board) new-to-move)]
                            (update-in
                                (update-in
                                    (assoc
                                        (choose-move-minimax new-board new-to-move
                                            (if is-different-player depth (dec depth)))
                                        :move
                                        move)
                                    [:depth]
                                    inc)
                                [:score]
                                (fn [score] (if is-different-player (- score) score)))))
                    (:available-moves board)))))

(defn -main
  [& args]
  (println "You are player one")
  (loop [board (create-board 3 3)]
    (println "Board is now:")
    (println (board-to-string board))
    (cond
        (is-game-complete board)
            (do
                (println "Game is done!")
                (println (board-to-string board))
                (println (:score board))
                (println
                    (case (game-outcome board 1)
                        -1 "I win!"
                        1 "You win!"
                        0 "We tied!")))
        (= (:player-to-move board) 1)
            (do
                (println "Enter your move:")
                (let [idx (Integer/parseInt (read-line))]
                    (recur (make-move board 1 idx))))
        :else
            (do
                (println "My move")
                (println "Searching at depth 5...")
                (let [move (choose-move-minimax board 2 5)]
                    (println (str "I choose move: " (:move move) " (evaluated " (:nodes move) " nodes to depth " (:depth move) ", score is " (:score move) ")"))
                    (recur (make-move board 2 (:move move))))))))
