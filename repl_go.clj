;; REPL Go
;; 
;; Models a basic game of go, playable via the REPL.
;;
;; Example (output omitted):
;;
;;     (use 'repl-go)
;;     (start-game)
;;     (play-move :d4)          ; Black move
;;     (play-move :c4)          ; White move
;;     (play-move :c5)          ; Black move
;;     ...
;;
;; Copyright (c) 2009 Justin Kramer <jkkramer@gmail.com>
;; Licensed under WTFPL, http://en.wikipedia.org/wiki/WTFPL

(ns repl-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defn to-int
  "Converts arg to an integer, with an optional fallback value"
  ([x] (to-int x nil))
  ([x errval] (try (Integer. x) (catch Exception _ errval))))

(defn tally
  "Returns a map of values to the number of times each value
   appears in the given sequence. Optionally starts with the given
   tally map."
  ([s]
     (tally s {}))
  ([s start]
     (reduce (fn [m val] (assoc m val (inc (get m val 0)))) start s)))

(defn pad
  "Left-pad a string"
  ([s len]
     (pad s len \space))
  ([s len pad]
     (str s (apply str (repeat (- len (.length s)) pad)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Board handling

(defn empty-board
  "Generates an empty board of the given size"
  [size]
  (vec (repeat (* size size) :empty)))

(def board-size
  (memoize
   (fn [board]
     (int (Math/sqrt (count board))))))

(defn in-bounds?
  "Whether the given coord falls within the board size"
  [board [x y]]
  (let [size (board-size board)]
    (and (>= x 0) (< x size) (>= y 0) (< y size))))

(defn coord->index
  "Return the index of a given coordinate within a board vector"
  [board [x y]]
  (let [size (board-size board)]
    (when (in-bounds? board [x y])
      (+ (* y size) x))))

(defn stone-at
  "Return the stone at the given board coordinate, or nil"
  [board coord]
  (when-let [idx (coord->index board coord)]
    (board idx)))

(defn add-stone
  "Return a board with a stone (:b, :w, :empty) added at a coordinate"
  [board stone coord]
  (when-let [idx (coord->index board coord)]
    (assoc board idx stone)))

(defn add-stones
  "Add multiple stones of the same type"
  [board stone coords]
  (reduce #(add-stone %1 stone %2) board coords))

(def stone->char {:b \X :w \O :empty \.})

(def x-axis-labels
     ;; infinite sequence
     (filter #(not= % \I)
             (for [i (iterate inc 0)] (char (+ (int \A) i)))))

(defn render-board
  "Render an ASCII text board suitable for printing"
  [board]
  (let [size (board-size board)]
    (apply str
           \space \space \space
           (apply str (interpose \space (take size x-axis-labels)))
           \newline
           (for [y (range size) x (range size)]
             (str (when (= x 0) (pad (str (inc y) \space) 3))
                  (stone->char (stone-at board [x y]))
                  \space
                  (when (= x (dec size)) \newline))))))

(defn label->coord
  "Converts a coord label (A5) to a coord ([0 4])"
  [label]
  (let [label (if (keyword? label) (.substring (str label) 1) (str label))
        x (- (int (.charAt (.toUpperCase label) 0)) (int \A))
        y (dec (to-int (.substring label 1) -1))]
    [x y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game state

(defstruct game-struct :board :turn :move-number :b-captures :w-captures)

(defn blank-game
  "Generate a blank game map"
  [size]
  (struct game-struct (empty-board size) :b 0 0 0))

(def stone->label {:b "Black" :w "White" :empty "Empty"})

(defn render-game
  "Render a game's board and various states as text"
  [g]
  (str "Move " (:move-number g) \newline
       (stone->label :b) " captures: " (:b-captures g) \newline
       (stone->label :w) " captures: " (:w-captures g) \newline
       (stone->label (:turn g)) " to play" \newline
       (render-board (:board g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule enforcement

(def opposite-color {:b :w :w :b})

(defn coords-around
  "Returns a vector of the coords surrounding the given coord (left, right,
   top, bottom). Note that some may be out of bounds."
  [[x y]]
  [[(- x 1) y] [(+ x 1) y] [x (- y 1)] [x (+ y 1)]])

(defn stone-libs
  "Returns the number of liberties of the stone at the given coord,
   independent of any connected stones"
  [board coord]
  (count (filter #(= :empty %)
                 (map #(stone-at board %)
                      (coords-around coord)))))

(defn group-at
  "Returns a set of the coords of all stones connected to the
   stone at the given coord."
  [board start-coord]
  (with-local-vars [group-coords #{}]
    (let [group-color (stone-at board start-coord)
          group-at* (fn group-at* [coord]
                      (when (and (= group-color (stone-at board coord))
                                 (not (contains? @group-coords coord)))
                        (var-set group-coords (conj @group-coords coord))
                        (dorun (map group-at* (coords-around coord)))))]
      (when (and group-color (not= :empty group-color))
        (group-at* start-coord)
        @group-coords))))

(defn group-libs
  "Returns the number of liberties of the given group of coords"
  [board group-coords]
  (when group-coords
    (reduce + (map #(stone-libs board %) group-coords))))

(defn capture-stones
  "Capture any opponent stones with zero liberties surrounding the
   given coord (presumed to be the last point played). Also captures
   the played stone if suiciding. Returns a vector of the 'new' board,
   count of captured white stones, and count of captured black stones"
  [board coord]
  (let [opp-color (opposite-color (stone-at board coord))
        opp-coords (filter #(= opp-color (stone-at board %))
                           (coords-around coord))
        opp-groups (map #(group-at board %) opp-coords)
        cap-groups (filter #(= 0 (group-libs board %)) opp-groups)
        cap-coords (if (< 0 (count cap-groups))
                     (reduce concat cap-groups)
                     (let [self-group (group-at board coord)]
                       (when (= 0 (group-libs board self-group))
                         self-group)))  ;; suicide
        cap-stones (map #(stone-at board %) cap-coords)
        cap-tally (tally cap-stones {:b 0 :w 0})
        new-board (add-stones board :empty cap-coords)]
    [new-board (:w cap-tally) (:b cap-tally)]))

(defn try-move
  "Tries playing a move to see if it's allowed. Returns a 'new' game state
   when everything is okay, or nil."
  [game-states coord]
  (let [g (peek game-states)
        curb (:board g)]
    (when (and (in-bounds? curb coord)
               (= :empty (stone-at curb coord)))
      (let [oldb (:board (peek (pop game-states)))
            uncapb (add-stone curb (:turn g) coord)
            [newb, bcaps, wcaps] (capture-stones uncapb coord)]
        (when-not (= newb oldb) ;; ko?
          (struct-map game-struct
            :board newb
            :turn (opposite-color (:turn g))
            :move-number (inc (:move-number g))
            :b-captures (+ bcaps (:b-captures g))
            :w-captures (+ wcaps (:w-captures g))))))))

(defn try-pass
  "Returns a new game state representing the game after a pass"
  [game-states]
  (let [g (peek game-states)]
    (assoc g
        :turn (opposite-color (:turn g))
        :move-number (inc (:move-number g)))))

(defn score
  []
  ;; TODO?
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game state succession, game playing, output
;;
;; The current game state has been left intentionally implicit in the
;; following functions, to make it easy to play via the REPL.
;;
;; An alternate strategy would be to have a create-game function, which
;; would return a game-states atom, which could be passed to the various
;; game-playing functions explicitly -- i.e. allow many games at a time.

;; Last item = current game state
(def game-states (atom []))

(defn print-game
  "Prints current game state"
  []
  (println (render-game (peek @game-states))))

(defn start-game
  "Reset the game to a clean slate, setting the board to the given size"
  ([]
     (start-game 9))
  ([size]
     (swap! game-states (fn [_] [(blank-game size)]))
     (print-game)))

(defn play-move
  "If the given move is allowed, make it real"
  [label]
  (if-let [newg (try-move @game-states (label->coord label))]
    (do
      (swap! game-states conj newg)
      (print-game))
    (println "You can't play there!")))

(defn pass
  []
  (swap! game-states conj (try-pass @game-states))
  (print-game))

(defn undo-move
  "Revert to the previous game state"
  []
  (if (> (count @game-states) 1)
    (do
      (swap! game-states pop)
      (print-game))
    (println "Nothing to undo!")))
