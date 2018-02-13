(ns dots-and-boxes.core
  (:gen-class))

; Dots and boxes is played on N x N grid
; 0 1 2 3 4
;
; x -- x -- x -- x -- x
; |    |    |    |    |
; x -- x -- x -- x -- x
; |    |    |    |    |
; x -- x -- x -- x -- x
; |    |    |    |    |
; x -- x -- x -- x -- x
;
; 3x3 grid:
;
; 0   1  2  3  4
; 5   6  7  8  9
; 10 11 12 13 14
; 15 16 17 18 19
; 20 21 22 23 24
;
; 0, 10, 20, 2, 4, are dots.
; Vertical pairs for line indices are +/- (2 * width - 1) * 2
; Horizontal pairs are +/- (2 * width - 1) + 1 and (2 * width - 1) - 1

(defrecord Board [height width board-array line-map])

(defn length-row [width] (+ (* 2 width) 1))

(defn create-dummy-row [width]
    (repeat (length-row width) "X"))

(defn create-horizontal-row [width]
    (let [len (length-row width)]
        (map
            #(cond
            (zero? %1) "X"
            (= %1 (- len 1)) "X"
            (zero? (mod %1 2)) "-"
            :else "*")
            (range len))))

(defn create-vertical-row [width]
    (let [len (length-row width)]
        (map
            #(cond
            (zero? %1) "X"
            (= %1 (- len 1)) "X"
            (zero? (mod %1 2)) " "
            :else "-")
            (range len))))

(defn create-board [height width]
    (let [arr
        (concat
            (create-dummy-row width)
            (create-dummy-row width)
            (apply concat (repeat (- height 1)
                (concat
                    (create-horizontal-row width)
                    (create-vertical-row width))))
            (create-horizontal-row width)
            (create-dummy-row width)
            (create-dummy-row width))]
    (->Board height width arr {})))

(defn board-range [board]
    (range (count (:board-array board))))

(defn board-to-string [board]
    (apply str (map-indexed (fn [idx itm]
        (cond
            (zero? (mod idx (length-row (:width board)))) "\n"
            (= itm "X") ""
            :else itm)) (:board-array board))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
