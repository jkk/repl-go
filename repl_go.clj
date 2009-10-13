;; REPL Go
;; 
;; Models a basic game of go, playable via the REPL.
;;
;; Example (output omitted):
;;
;;     (use 'repl-go)
;;     (start-game)
;;     (play-move [4 4])          ; Black move
;;     (play-move [3 4])          ; White move
;;     (play-move [3 5])          ; Black move
;;     ...
;;
;; Copyright (c) 2009 Justin Kramer <jkkramer@gmail.com>
;; Licensed under WTFPL, http://en.wikipedia.org/wiki/WTFPL

(ns repl-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Board handling

(def size 9) ;; TODO: determine on-the-fly

(def empty-board (vec (repeat (* size size) :empty)))

(defn in-bounds?
  "Whether the given coord falls within the board size"
  [[x y]]
  (and (>= x 0) (< x size) (>= y 0) (< y size)))

(defn coord->index
  "Return the index of a given coordinate within a board vector"
  [[x y]]
  (when (in-bounds? [x y])
    (+ (* y size) x)))

(defn stone-at
  "Return the stone at the given board coordinate, or nil"
  [board coord]
  (when-let [idx (coord->index coord)]
    (board idx)))

(defn add-stones
  "Return a board with stones (:b, :w, :empty) added at coordinates.
   Example:
       (add-stones board :w [4 4] :b [6 3])"
  ([board]
     board)
  ([board stone coord]
     (when-let [idx (coord->index coord)]
       (assoc board idx stone)))
  ([board stone coord & scs]
     (when-let [idx (coord->index coord)]
       (let [ret (assoc board idx stone)]
         (if scs
           (recur ret (first scs) (second scs) (nnext scs))
           ret)))))

(def stone->char {:b \X :w \O :empty \.})

(defn render-board
  "Render an ASCII text board suitable for printing"
  ;; TODO: print row/col labels
  [board]
  (apply str
         (for [y (range size) x (range size)]
           (str (stone->char (stone-at board [x y]))
                \space
                (when (= x (dec size)) \newline)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game state

(defstruct game-struct :board :turn :move-number :b-captures :w-captures)

(def blank-game (struct game-struct empty-board :b 0 0 0))

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
  (apply + (map #(stone-libs board %) group-coords)))

(defn capture-stones
  "Capture any stones with zero liberties surrounding the given
   coord (presumed to be the last point played). Returns a vector
   of the 'new' board, count of captured black stones, and count of
   captured white stones"
  [board coord]
  (let [groups (map #(group-at board %) (coords-around coord))
        cap-groups (filter #(= 0 (group-libs board %)) groups)
        cap-coords (reduce concat cap-groups)
        cap-stones (map #(stone-at board %) cap-coords)
        ;; TODO: cap-tally
        bcaps (count (filter #(= :w %) cap-stones))
        wcaps (count (filter #(= :b %) cap-stones))
        stones-coords (interleave (repeat :empty) cap-coords)
        new-board (apply add-stones (concat [board] stones-coords))]
    [new-board bcaps wcaps]))

(defn try-move
  "Tries playing a move to see if it's allowed. Returns a 'new' game state
   when everything is okay, or nil."
  [game-states coord]
  (let [g (peek game-states)]
    (when (and (in-bounds? coord)
               (= :empty (stone-at (:board g) coord)))
      (let [curb (:board g)
            oldb (:board (peek (pop game-states)))
            uncapb (add-stones curb (:turn g) coord)
            [newb, bcaps, wcaps] (capture-stones uncapb coord)]
        (when-not (= newb oldb) ;; ko?
          (struct-map game-struct
            :board newb
            :turn (opposite-color (:turn g))
            :move-number (inc (:move-number g))
            :b-captures (+ bcaps (:b-captures g))
            :w-captures (+ wcaps (:w-captures g))))))))

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
  "Reset the game to a clean slate"
  []
  (swap! game-states (fn [_] [blank-game]))
  (print-game))

(defn play-move
  "If the given move is allowed, make it real"
  ;; TODO: handle pass, take a string like "E5" 
  [coord]
  (if-let [newg (try-move @game-states coord)]
    (swap! game-states conj newg)
    (println "You can't play there!"))
  (print-game))

(defn undo-move
  "Revert to the previous game state"
  []
  (if (> (count @game-states) 1)
    (swap! game-states pop)
    (println "Nothing to undo!"))
  (print-game))
