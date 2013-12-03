(ns a-star.nodemap)

(use 'a-star.point)
(use 'a-star.astarp)
(use 'a-star.search)

(declare point-distance)
(declare build-field)
(declare find-node-with-coord)


(defrecord Node [prev-key status coord links])


(defn single-open?
  "Return true if a node is open"
  [this]
  (= :open (:status this)))

(defn single-closed?
  "Return true if a node is closed"
  [this]
  (= :closed (:status this)))

(defn single-visited?
  "Return true if a node has been visited"
  [this]
  (or (single-open? this) (single-closed? this)))

(defn single-blocked?
  "Return true if a node has been blocked"
  [this]
  (= :blocked (:status this)))

(defn single-open
  "Open a node"
  [this prev-key]
  (Node. prev-key :open (:coord this) (:links this)))

(defn single-close
  "Close a node"
  [this]
  (Node. (:prev-key this) :closed (:coord this) (:links this)))

(defn single-block
  "Block a node"
  [this]
  (Node. (:prev-key this) :blocked (:coord this) (:links this)))

(defn single-back
  "Go to a previous node"
  [this]
  (:prev-key this))





;; Field implementation supporting A* protocol
(defrecord NodeMap [node-map    ;; Map of nodes
                    start-key   ;; Starting key
                    end-key     ;; Ending key
                    ]
  AStarProtocol
  (open? [this node-key] (-> this :node-map node-key single-open?))
  (closed? [this node-key] (-> this :node-map node-key single-closed?))
  (visited? [this node-key] (-> this :node-map node-key single-visited?))
  (blocked? [this node-key] (-> this :node-map node-key single-blocked?))
  (open
    [this node-key prev-key]
    (NodeMap. 
      (assoc
        (:node-map this)
        node-key
        (single-open (-> (:node-map this) node-key) prev-key))
      (:start-key this)
      (:end-key this)))
  (close
    [this node-key]
    (NodeMap.
      (assoc
        (:node-map this)
        node-key
        (-> (:node-map this) node-key single-close))
      (:start-key this)
      (:end-key this)))
  (block
    [this node-key]
      (NodeMap.
        (assoc
          (:node-map this)
          node-key
          (-> (:node-map this) node-key single-block))
        (:start-key this)
        (:end-key this)))
  (get-prev-key
    [this node-key]
    (let [node (node-key (:node-map this))]
      (if (nil? node)
        nil
        (single-back node))))
  (distance
    [this key1 key2]
    (point-distance (-> this :node-map key1 :coord)
                    (-> this :node-map key2 :coord)))
  (past-path-cost
    [this node-key]
    (loop [node-key node-key cost 0]
      (if (nil? node-key)
        cost
        (let [prev-node-key (-> this :node-map node-key single-back)]
          (if (nil? prev-node-key)
            cost
            (recur prev-node-key
                   (+ cost (distance this node-key prev-node-key))))))))
  (future-path-cost [this node-key] (distance this node-key (:end-key this)))
  (connected-nodes [this node-key] (-> this :node-map node-key :links))
  (node-keys [this] (-> this :node-map keys)))




(defn create-node
  "create a Node record with possibly empty node"
  ([prev-key status coord links] (Node. prev-key status coord links))
  ([coord links] (Node. nil :empty coord links)))




;; Point list instantiations

(defn make-simple-node-map
  "Make a simple template node map"
  []
  (let [nodes (build-field 5 :00 :22)
        block-list [:11 :12 :13 :33 :32 :31 :21]]
    (reduce (fn [a b] (block a b)) nodes block-list)))


(defn build-field
  "Build a field of size dimension, setting start key and end key"
  [dimension start-key end-key]
  (letfn [(add-adj [x_c y_c all-keys keysize]
            (let [ad (for [x [(dec x_c) x_c (inc x_c)]
                           y [(dec y_c) y_c (inc y_c)]]
                       (keyword (to-str-pad keysize x y)))]
                  (filter (fn [x] (some #{x} all-keys)) ad)))
          (to-keyword
            [point keysize]
            (keyword (to-str-pad keysize (:x point) (:y point))))]
  (let [keylist (for [x1 (range dimension) x2 (range dimension)]
                  (->Point x1 x2))
        keysize (count (str (dec dimension)))
        all-keys (map #(to-keyword % keysize) keylist)
        dests (map (fn [point] [(to-keyword point keysize)
                                (create-node
                                  point
                                  (add-adj
                                    (:x point)
                                    (:y point)
                                    all-keys
                                    keysize))])
                   keylist)]
    (NodeMap.
      (reduce (fn [a b] (assoc a (first b) (second b))) {} dests)
      start-key
      end-key))))


;; Printing

(defn print-field
  "Print a field"
  ([node-map] (print-field node-map (:start-key node-map) (:end-key node-map)))
  ([node-map start-key end-key]
   (let [xvals (map #(-> node-map :node-map % :coord :x) (node-keys node-map))
         yvals (map #(-> node-map :node-map % :coord :y) (node-keys node-map))
         ystart (apply min yvals)
         yend (apply max yvals)
         xstart (apply min xvals)
         xend (apply max xvals)
         winning-path (walk-path node-map start-key end-key)]
     (loop [x xstart y ystart]
       (if (> y yend)
         nil
         (let [node-key (find-node-with-coord node-map x y)
               x1 (if (>= x xend) 0 (inc x))
               y1 (if (>= x xend) (inc y) y)]
           (do
             (cond (nil? node-key) (print "~")
                   (= start-key node-key) (print "S")
                   (= end-key node-key) (print "E")
                   (not (nil? (some #{node-key} winning-path))) (print "#")
                   (blocked? node-map node-key) (print "|")
                   (open? node-map node-key) (print "-")
                   (closed? node-map node-key) (print "x")
                   :else (print "~"))
             (if (zero? x1) (println))
             (recur x1 y1))))))))


(defn find-node-with-coord [node-map x y]
  (let [matches (filter
                  (fn [n] (and (= (-> node-map :node-map n :coord :x) x)
                               (= (-> node-map :node-map n :coord :y) y)))
                  (node-keys node-map))]
    (if-not (empty? matches)
      (first matches)
      nil)))


(defn point-distance [point1 point2]
  (let [xdist (- (:x point2) (:x point1))
        ydist (- (:y point2) (:y point1))]
    (Math/sqrt (+ (Math/pow xdist 2) (Math/pow ydist 2)))))


