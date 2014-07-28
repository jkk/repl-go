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
;;     (print-score)
;;
;; Copyright (c) 2009 Justin Kramer <jkkramer@gmail.com>
;; Licensed under WTFPL, http://en.wikipedia.org/wiki/WTFPL

(ns repl-go
  (:use [clojure.set :only [difference]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defn to-int
  "Converts arg to an integer, with an optional fallback value"
  [x & [errval]] (try (Integer. x) (catch Exception _ errval)))

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
  [size [x y]]
  (and (>= x 0) (< x size) (>= y 0) (< y size)))

(defn all-coords
  "Return the set of all coordinates for a board of the given size"
  [size]
  (set (for [x (range size) y (range size)] [x y])))

(defn coord->index
  "Return the index of a given coordinate within a board vector"
  [size [x y]]
  (when (in-bounds? size [x y])
    (+ (* y size) x)))

(defn stone-at
  "Return the stone at the given board coordinate, or nil"
  [board coord]
  (when-let [idx (coord->index (board-size board) coord)]
    (board idx)))

(defn add-stone
  "Return a board with a stone (:b, :w, :empty) added at a coordinate"
  [board stone coord]
  (when-let [idx (coord->index (board-size board) coord)]
    (assoc board idx stone)))

(defn add-stones
  "Add multiple stones of the same type"
  [board stone coords]
  (reduce #(add-stone %1 stone %2) board coords))

(def stone->char {:b \X :bt \x :w \O :wt \o :empty \.})
(def stone->territory {:b :bt :w :wt})

(defn int->letter
  "0 => A, 1 => B, etc."
  [x]
  (char (+ (int \A) x)))

(def x-axis-labels
     ;; infinite sequence
     (filter (partial not= \I)
             (map int->letter (iterate inc 0))))

(defn render-board
  "Render an ASCII text board suitable for printing"
  [board]
  (let [size (board-size board)]
    (apply str
           \space \space \space
           (apply str (interpose \space (take size x-axis-labels)))
           \newline
           (for [y (range size) x (range size)]
             (str (when (= x 0) (str (format "%1$2d" (inc y)) \space))
                  (stone->char (stone-at board [x y]))
                  \space
                  (when (= x (dec size)) \newline))))))

(defn label->coord
  "Converts a coord label (A5) to a coord ([0 4])"
  [label]
  (let [label (if (keyword? label) (.substring (str label) 1) (str label))
        charval (int (.charAt (.toUpperCase label) 0))
        x (- (if (> charval (int \I)) (dec charval) charval) (int \A))
        y (dec (to-int (.substring label 1) -1))]
    [x y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule enforcement

(def opposite-color {:b :w :w :b})

(defn neighbors
  "Returns a vector of the coords surrounding the given coord (left, right,
   top, bottom). Note that some may be out of bounds."
  [[x y]]
  [[(- x 1) y] [(+ x 1) y] [x (- y 1)] [x (+ y 1)]])

(defn stone-libs
  "Returns the number of liberties of the stone at the given coord,
   independent of any connected stones"
  [board coord]
  (count (filter #(= :empty (stone-at board %))
                 (neighbors coord))))

(defn group-at
  "Returns a set of the coords of all stones connected to the
   stone at the given coord. Can also be used to find contiguous
   areas of :empty coords."
  [board start-coord]
  (when-let [group-color (stone-at board start-coord)]
    (loop [group #{}
           pending #{start-coord}]
      (if-let [coord (first pending)]
        (recur (conj group coord)
               (reduce conj
                       (disj pending coord)
                       (filter #(and (= group-color (stone-at board %))
                                     (not (contains? pending %))
                                     (not (contains? group %)))
                               (neighbors coord))))
        group))))

(defn group-libs
  "Returns the number of liberties of the given group of coords"
  [board group-coords]
  (when group-coords
    (reduce + (map (partial stone-libs board) group-coords))))

(defn capture-stones
  "Capture any opponent stones with zero liberties surrounding the
   given coord (presumed to be the last point played). Also captures
   the played stone if suiciding. Returns a vector of the 'new' board,
   count of captured white stones, and count of captured black stones"
  [board coord]
  (let [opp-color (opposite-color (stone-at board coord))
        cap-groups (->> (neighbors coord)
                        (filter #(= opp-color (stone-at board %)))
                        (map #(group-at board %))
                        (filter #(= 0 (group-libs board %))))
        cap-coords (if (< 0 (count cap-groups))
                     (reduce concat cap-groups)
                     (let [self-group (group-at board coord)]
                       (when (= 0 (group-libs board self-group))
                         self-group))) ;; suicide
        cap-tally (frequencies (map #(stone-at board %) cap-coords))
        new-board (add-stones board :empty cap-coords)]
    [new-board (:w cap-tally 0) (:b cap-tally 0)]))

(defn try-move
  "Tries playing a move to see if it's allowed. When everything's okay,
   returns a vector of 'new' board, B captures, and W captures. Otherwise,
   nil."
  [board stone coord & [old-board]]
  (when (and (in-bounds? (board-size board) coord)
             (= :empty (stone-at board coord)))
    (let [raw-board (add-stone board stone coord)
          [new-board, bcaps, wcaps] (capture-stones raw-board coord)]
      (when-not (= new-board old-board) ;; ko?
        [new-board bcaps wcaps]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scoring

(defn all-groups
  "Return a vector of all groups on the board"
  [board]
  (loop [remaining-coords (all-coords (board-size board))
         groups []]
    (if (empty? remaining-coords)
      groups
      (let [next-group (group-at board (first remaining-coords))]
        (recur (difference remaining-coords next-group)
               (conj groups next-group))))))

(defn all-same?
  "Return nil if both colors are found in the coords, or :empty if
   neither is found. Otherwise, the color occupying all the existent,
   non-empty coords."
  [board coords]
  (let [occupied (filter #{:b :w} (map (partial stone-at board) coords))]
    (cond
      (empty? occupied) :empty
      (apply = occupied) (first occupied))))

(defn surrounded-by
  "Return the sole color surrounding a group if such a color exists.
   Otherwise, nil."
  [board group]
  (let [found (remove #(= :empty %)
                      (map #(all-same? board (neighbors %)) group))]
    (when (and (seq found) (first found) (apply = found))
      (first found))))

(defn group-owner
  "If the group is composed of stones, return their color. Otherwise,
   the color of the stones surrounding it, or nil if there is no single
   owner."
  [board group]
  (let [stone (stone-at board (first group))]
    (if (not= :empty stone)
      stone
      (surrounded-by board group))))

(defn score
  "Return a map of the score for the board"
  [board]
  (reduce
    (fn [scores group]
      (if-let [owner (group-owner board group)]
        (assoc scores owner (+ (scores owner) (count group)))
        scores))
    {:b 0 :w 0} (all-groups board)))

(defn determine-territories
  "Return a map from coord to stone, indicating ownership of :empty space
   as territory for a player"
  [board]
  (reduce
    (fn [owned-coords group]
      (or (when (= :empty (stone-at board (first group)))
            (when-let [owner (group-owner board group)]
              (apply (partial assoc owned-coords)
                     (interleave group (repeat owner)))))
          owned-coords))
    {} (all-groups board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game state

(defstruct game-struct :board :turn :movenum :bcaps :wcaps)

(defn blank-game
  "Generate a blank game state"
  [size]
  (struct game-struct (empty-board size) :b 0 0 0))

(defn next-game-state
  "Generate the next game state based on the given state and optional
   map of game state key/vals"
  [g & [kvs]]
  (merge (assoc g
           :turn (opposite-color (:turn g))
           :movenum (inc (:movenum g)))
         kvs))

(def stone->label {:b "Black" :w "White" :empty "Empty"})

(defn render-game
  "Render a game's board and various states as text"
  [g]
  (str "Move " (:movenum g) \newline
       (stone->label :b) " captures: " (:bcaps g) \newline
       (stone->label :w) " captures: " (:wcaps g) \newline
       (stone->label (:turn g)) " to play" \newline
       (render-board (:board g))))

(defn render-score
  "Render a game's board with filled in territories and score as text"
  [g]
  (let [board (:board g)
        score (score board)
        size (board-size board)
        territory-owners (determine-territories board)]
    (str (stone->label :b) ": " (:b score) \newline
         (stone->label :w) ": " (:w score) \newline
         (render-board (vec (for [y (range size) x (range size)]
                              (if-let [owner (territory-owners [x y])]
                                (stone->territory owner)
                                (stone-at board [x y]))))))))

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

(defn print-score
  "Prints the current score"
  []
  (println (render-score (peek @game-states))))

(defn start-game
  "Reset the game to a clean slate, setting the board to the given size"
  [& [size]]
  (swap! game-states (fn [_] [(blank-game (or size 9))]))
  (print-game))

(defn play-move
  "If the given move is allowed, make it real"
  [label]
  (let [g (peek @game-states)
        board (:board g)
        stone (:turn g)
        coord (label->coord label)
        oldb (:board (peek (pop @game-states)))]
    (if-let [[newb bcaps wcaps] (try-move board stone coord oldb)]
      (let [newg (next-game-state g {:board newb
                                     :bcaps (+ bcaps (:bcaps g))
                                     :wcaps (+ wcaps (:wcaps g))})]
        (swap! game-states conj newg)
        (print-game))
      (println "You can't play there!"))))

(defn pass
  "Skip a turn"
  []
  (swap! game-states conj (next-game-state (peek @game-states)))
  (print-game))

(defn undo-move
  "Revert to the previous game state"
  []
  (if (> (count @game-states) 1)
    (do
      (swap! game-states pop)
      (print-game))
    (println "Nothing to undo!")))
