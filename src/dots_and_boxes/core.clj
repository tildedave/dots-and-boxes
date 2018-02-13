(ns dots-and-boxes.core
  (:gen-class))

; Dots and boxes is played on N x N grid
; We represent 'out of bounds' with dummy rows that contain "X" items
; For example a 3x3 grid will be internally represented as:
; XXXXXXX   0  1  2  3  4  5  6
; XXXXXXX   7  8  9 10 11 12 13
; X*-*-*X  14 15 16 17 18 19 20
; X- - -X  21 22 23 24 25 26 27
; X*-*-*X  28 29 30 31 32 33 34
; X- - -X  35 36 37 38 39 40 41
; X*-*-*X  42 43 44 45 46 47 48
; XXXXXXX  49 50 51 52 53 54 55
; XXXXXXX  56 57 58 59 60 61 62
;
; 16's "pairs" in box life are 22, 24, 30
; 24 is in two boxes: [16 22 24 30] and [18 24 26 32]

; FIXME: board-array is not yet an array
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
    ; TODO: includes extraneous newlines because of printing the dummy rows
    (apply str (map-indexed (fn [idx itm]
        (cond
            (zero? (mod idx (length-row (:width board)))) "\n"
            (= itm "X") ""
            :else itm)) (:board-array board))))

(defn is-horizontal-idx [board idx]
    ; Is the index on the horizontal row?
    (let [len (length-row (:width board))]
        (< (mod idx (* 2 len)) len)))

; (defn vertical-partners-idx [board idx]
;     (let [len (length-row (:width board))]
;         [(+ idx (* 2 len))
;          (- idx (* 2 len))]))

; (defn vertical-partners-idx [board idx]
;     (let [len (length-row (:width board))]
;         [(+ idx (* 2 len))
;          (- idx (* 2 len))]))

; (defn line-pairs [board idx]
;     ; Vertical pairs for line indices are +/- (2 * width - 1) * 2
;     ; Horizontal pairs are +/- (2 * width - 1) + 1 and (2 * width - 1) - 1
;     (vertical-partner)
;     )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
