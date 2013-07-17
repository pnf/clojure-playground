; list of winning positions
(def wp 
  [2r111000000 2r000111000 2r000000111 2r100100100 2r010010010 2r001001001 2r100010001 2r001010100])
;; Is a particular bitmask winning?
(defn winner? [p] 
  (some (fn [c] (= (bit-and c p) c)) wp))
;; list of possible moves, filtering out ones already made
(defn moves [p] 
  (filter pos? (map #(bit-and (bit-not p) (bit-shift-left 1%)) (range 9))))
;; A board position comprises a bitarray for each player and an indicaiton of whose turn it is.
;; A board position won by player 1 has a score of 1; a board position won by player 2 has a score of -1.
;; Other board positions are scored by a minimaxy tuple:
;;    1) The best that player 1 can hope to attain (1 being good for him) if he makes ideal moves
;;    2) The best that player 2 can hope to attain (-1 being good for him) if he makes ideal moves
;;    3) An average score, assuming random moves
;; Since tic-tac-toe is a "solved" game, we introduce (3) as a tie-breaker, since otherwise the game will always be a draw.
;; Define an array of utilities that differe depending on whose turn it is.
                                        ;          scoring by tuple                                    picking move1                     move2
(def for1 ; i.e. when it's X's turn
  [(fn [t] (+(*(second t) 4) (*(first t) 2) (t 2) ))  ; score from X's perspective
   max-key                                            ; X's score goal
   (fn [p1 m] (bit-or p1 m))                          ; how to apply a move to X's position
   (fn [p2 m] p2)])                                   ; how (not) to apply it to O's
(def for2 ; i.e. when it's Y's turn
  [(fn [t] (+(*(first t) 4)(*(second t) 2)(t 2) ))
   min-key
   (fn [p1 m] p1)
   (fn [p2 m] (bit-or p2 m))])
(def forplayer [nil for1 for2])

(def score (memoize
   (fn [p1 p2 turn]
     (cond
       (winner? p1) [1 1 1]
       (winner? p2) [-1 -1 -1]
       :else
       (let [mm (moves (bit-or p1 p2)) ]
         (if (empty? mm)
           [0 0 0] ; a draw
           (cond ; nshortcut minimax if there's an avalable win on this mvoe
            (and (= turn 1) (some #(winner? (bit-or p1 %)) mm)) [ 1  1  1]
            (and (= turn 2) (some #(winner? (bit-or p2 %)) mm)) [-1 -1 -1]
            :else
            (let [[fscore fpick p1f p2f] (forplayer turn)
                  scorevectors  (map #(score (p1f p1 %) (p2f p2 %) (- 3 turn)) mm)
                  av             (float (/ (reduce + (map #(% 2) scorevectors))
                                           (count scorevectors)))
                  best           (apply fpick fscore scorevectors)]
              [(best 0) (best 1) av]))
           ))))))

;; List of all possible moves for the player, along with their score tuples
(defn scoredmoves [p1 p2 turn] 
  (let [[fscore fpick p1f p2f] (forplayer turn)]
    (map (fn [x] [x (score (p1f p1 x) (p2f p2 x) (- 3 turn)) ] )
         (moves (bit-or p1 p2)) ) ) )

;; Best of those moves, along with resultant board positions
(defn move [p1 p2 turn]
  (let [ms (scoredmoves p1 p2 turn)]
    (if (= 0 (count ms)) []
        (let [[fscore fpick p1f p2f] (forplayer turn)
              [themove vectorscore]  (apply fpick #(fscore (second %)) ms)
              p1m                    (p1f p1 themove)
              p2m                    (p2f p2 themove)
              p1s                    (Integer/toString p1m 2)
              p2s                    (Integer/toString p2m 2)]
          ; return tuple of possibly interesting data
          [themove p1m p2m (- 3 turn) p1s p2s vectorscore ]))))

(defn pretty [p1 p2] 
  (map (fn [p] (cond (pos? (bit-and p1 p)) "X"
                     (pos? (bit-and p2 p)) "O"
                     :else                 "_"))
       (reverse (map #(bit-shift-left 1 %) (range 9)))))

(defn board [p1 p2] 
  (println (clojure.string/join "\n" (re-seq #"..." (apply str (pretty p1 p2))))))
