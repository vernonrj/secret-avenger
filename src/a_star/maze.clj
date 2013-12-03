(ns a-star.maze)

(use 'a-star.point)
(use 'a-star.astarp)
(use 'a-star.search)

(declare generate-maze)
(declare find-with-coords)


(defprotocol MazeProtocol
  "Maze generation protocol"
  (touched-node?
    [this node-key]
    "return true if node has been touched during maze generation")
  (possible-connections
    [this node-key]
    "return a list of keys that could connect to node-key")
  (connect-nodes
    [this node-key-from node-key-to]
    "connect node-key-from to node-key-to")
  (set-node-visited
    [this node-key]
    "set node-key as visited"))


;; Maze node - each "node" in a MazeMap is this
(defrecord MazeNode [maze-key   ;; Unique key to address this node
                     visited    ;; Whether or not this node has been 
                                ;; touched during maze generation
                     coord      ;; Node's coordinates as a Point
                     links      ;; Nodes that are connected to this node
                     prev       ;; Previous node (used in A*)
                     status     ;; open/closed/blocked (used in A*)
                     ])

;; Maze class supporting the A* protocol
(defrecord MazeMap [node-map    ;; HashMap of MazeNodes
                    start-key   ;; key pointing to beginning of maze
                    end-key     ;; key pointing to end of maze
                    ]
  AStarProtocol
  (open? [this node-key] (= :open (-> this :node-map node-key :status)))
  (closed? [this node-key] (= :closed (-> this :node-map node-key :status)))
  (visited? [this node-key] (or (open? this node-key) (closed? this node-key)))
  (blocked? [this node-key] (= :blocked (-> this :node-map node-key :status)))
  (open [this node-key prev-key]
    (let [node (-> this :node-map node-key)]
      (MazeMap.
        (assoc
          (:node-map this)
          node-key
          (MazeNode. (:maze-key node)
                     (:visited node)
                     (:coord node)
                     (:links node)
                     prev-key
                     :open))
        (:start-key this)
        (:end-key this))))
  (close [this node-key]
    (let [node (-> this :node-map node-key)]
      (MazeMap.
        (assoc
          (:node-map this)
          node-key
          (MazeNode. (:maze-key node)
                     (:visited node)
                     (:coord node)
                     (:links node)
                     (:prev node)
                     :closed))
        (:start-key this)
        (:end-key this))))
  (block [this node-key]
    (let [node (-> this :node-map node-key)]
      (MazeMap.
        (assoc
          (:node-map this)
          node-key
          (MazeNode. (:maze-key node)
                     (:visited node)
                     (:coord node)
                     (:links node)
                     (:prev node)
                     :blocked))
        (:start-key this)
        (:end-key this))))
  (get-prev-key [this node-key]
    (-> this :node-map node-key :prev))
  (distance [this key1 key2]
    (let [node1 (-> this :node-map key1 :coord)
          node2 (-> this :node-map key2 :coord)]
      (Math/sqrt (+ (Math/pow (- (:x node2) (:x node1)) 2)
                    (Math/pow (- (:y node2) (:y node1)) 2)))))
  (past-path-cost [this node-key]
    (loop [node-key node-key cost 0]
      (if (nil? node-key)
        cost
        (let [prev-key (get-prev-key this node-key)]
          (if (nil? prev-key)
            cost
            (recur prev-key
                   (+ cost (distance this node-key prev-key))))))))
  (future-path-cost [this node-key]
    (distance this node-key (:end-key this)))
  (connected-nodes [this node-key] (-> this :node-map node-key :links))
  (node-keys [this] (-> this :node-map keys))
  MazeProtocol
  (touched-node? [this node-key] (true? (-> this :node-map node-key :visited)))
  (possible-connections
    [this node-key]
    (let [node (-> this :node-map node-key)
          coord (:coord node)
          x (-> coord :x)
          y (-> coord :y)
          points [(->Point (dec x) y)
                  (->Point (inc x) y)
                  (->Point x (dec y))
                  (->Point x (inc y))]
          nodes (map #(find-with-coords this %) points)]
      (set (remove nil? nodes))))
  (connect-nodes [this node-key-from node-key-to]
    (let [from-node (-> this :node-map node-key-from)
          to-node (-> this :node-map node-key-to)
          connected-from (MazeMap.
                           (assoc
                             (:node-map this)
                             node-key-from
                             (MazeNode.
                               (:maze-key from-node)
                               (:visited from-node)
                               (:coord from-node)
                               (conj (:links from-node) node-key-to)
                               (:prev from-node)
                               (:status from-node)))
                           (:start-key this)
                           (:end-key this))
          connected-to (assoc connected-from :node-map
                              (assoc
                                (:node-map connected-from)
                                node-key-to
                                (assoc to-node :links
                                       (conj (:links to-node)
                                             node-key-from))))]
      connected-to))
  (set-node-visited
    [this node-key]
    (let [node (-> this :node-map node-key)]
      (assoc this :node-map
             (assoc (:node-map this) node-key
                    (assoc node :visited true))))))



(defn find-with-coords
  "Find a node from MazeMap with coords matching those in point"
  [this point]
  (first (filter
           #(= point (-> this :node-map % :coord))
           (-> this :node-map keys))))


(defn initialize-maze
  [dimension]
  (let [keysize (count (str dimension))
        start-key (keyword (apply str (repeat (* 2 keysize) "0")))
        end-key (keyword (to-str-pad keysize (dec dimension) (dec dimension)))
        nodes (for [x (range dimension)
                    y (range dimension)]
                (let [point (->Point x y)]
                  (MazeNode.
                    (keyword (to-str-pad keysize x y))
                    false
                    point
                    '()
                    nil
                    :empty)))]
    (let [maze-initial (MazeMap.
                         (reduce
                           (fn [a b] (assoc a (:maze-key b) b))
                           {}
                           nodes)
                         start-key
                         end-key)]
      ;; join all adjacent keys
      (generate-maze maze-initial))))

(defn generate-maze
  "Generate an initialized maze"
  ([maze-map] (generate-maze maze-map (:end-key maze-map)))
  ([maze-map current-key] 
   (if (every? #(touched-node? maze-map %) (-> maze-map :node-map keys))
     maze-map
     (let [this (set-node-visited maze-map current-key)
           connections (possible-connections this current-key)
           unvisited (remove #(touched-node? this %) connections)]
       (if (empty? unvisited)
         this
         (let [next-key (rand-nth unvisited)]
           (recur
             (generate-maze
               (connect-nodes this current-key next-key)
               next-key)
             current-key)))))))


(defn print-maze-horiz
  "Print a horizontal line of a maze"
  ([maze-map y] (print-maze-horiz maze-map y '()))
  ([maze-map y walkback]
   (letfn [(node-char [maze-map node-key walkback]
             (cond
               (= (:start-key maze-map) node-key) "S"
               (= (:end-key maze-map) node-key) "E"
               (some #{node-key} walkback) "#"
               :else " "))
           (horiz-char [maze-map node-key x y walkback]
             (let [right (find-with-coords maze-map (->Point (inc x) y))]
               (cond
                 (not (some #{right} (connected-nodes maze-map node-key))) "|"
                 (not (every?
                        (fn [x] (some #{x} walkback))
                        [node-key right])) " "
                 (= node-key (get-prev-key maze-map right)) ">"
                 (= (get-prev-key maze-map node-key) right) "<"
                 :else " ")))
           (print-vert-links [maze-map x y]
             (let [node-key (find-with-coords maze-map (->Point x y))]
               (if (nil? node-key)
                 nil
                 (let [up (find-with-coords maze-map (->Point x (dec y)))]
                   ;(println x y up)
                   (cond
                     (not
                       (some #{up}
                             (connected-nodes maze-map node-key))) (print "--")
                     (not (every?
                            (fn [x] (some #{x} walkback))
                            [node-key up])) (print " -")
                     (= node-key (get-prev-key maze-map up)) (print "^-")
                     (= (get-prev-key maze-map node-key) up) (print "v-")
                     :else (print " -"))
                   (recur maze-map (inc x) y)))))]
     (print "|")
     (print-vert-links maze-map 0 y)
     (println "")
     (print "|")
     (loop [x 0]
       (let [node-key (find-with-coords maze-map (->Point x y))]
         (if (nil? node-key)
           nil
           (do 
             (print (str (node-char maze-map node-key walkback)
                         (horiz-char maze-map node-key x y walkback)))
             (recur (inc x)))))))))



(defn print-maze
  "Pretty-print a maze"
  [maze-map]
  (let [walkback (walk-path maze-map (:end-key maze-map))]
    (loop [y 0]
      (if (nil? (find-with-coords maze-map (->Point 0 y)))
        nil
        (do
          (print-maze-horiz maze-map y walkback)
          (println "")
          (recur (inc y)))))))

