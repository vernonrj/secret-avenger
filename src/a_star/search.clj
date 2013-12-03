(ns a-star.search)

(use 'a-star.astarp)

(declare walk-path)
(declare better-path-cost-found? find-lowest-cost)
(declare estimated-path-cost)
(declare find-all-open open-adjacent find-valid-nodes)
(declare find-unvisited-adjacent)


(defn walk-path
  "returns a list of keys representing the current path"
  ([node-map node-key] (walk-path node-map nil node-key))
  ([node-map start-key node-key]
   (loop [node-key node-key node-list []]
     (cond
       (nil? node-key) node-list
       (= start-key node-key) (concat [node-key] node-list)
       :else (let [prev-node-key (get-prev-key node-map node-key)]
               (recur prev-node-key (concat [node-key] node-list)))))))


(defn better-path-cost-found?
  "check to see if setting node-key's prev-key to node-prev
  gives a better path cost than node-key's current node-prev"
  [node-map node-key node-prev]
  (let [current-cost (past-path-cost node-map node-key)
        base-cost (past-path-cost node-map (get-prev-key node-map node-key))
        new-cost (+ (past-path-cost node-map node-prev)
                    (distance node-map node-key node-prev))]
    (< new-cost current-cost)))


(defn estimated-path-cost
  "return the knowledge-plus-heuristic cost of path"
  [node-map node-key]
  (+ (past-path-cost node-map node-key)
     (future-path-cost node-map node-key)))


(defn find-lowest-cost
  "find the lowest estimated cost among the open list,
  returns as a tuple of (key cost)"
  [node-map end-key]
  (let [node-costs (map (fn [x] [x (estimated-path-cost
                                     node-map
                                     x)])
                        (find-all-open node-map))]
    (if (empty? node-costs)
      0
      (first (apply min-key second node-costs)))))


(defn find-all-open
  "Find all open nodes"
  [node-map]
  (filter #(open? node-map %) (node-keys node-map)))


(defn open-adjacent
  "Open any unopened nodes adjacent to node, and close current node"
  [node-map node-key]
  (let [valid-moves (find-valid-nodes node-map node-key)
        newly-opened (reduce (fn [a b] (open a b node-key))
                             node-map
                             valid-moves)]
    (close newly-opened node-key)))


(defn find-valid-nodes
  "find adjacent nodes that are either unopened or have a better path value
  from node-key"
  [node-map node-key]
  (letfn [(valid? [node-map chk-key]
            (cond (closed? node-map chk-key) false
                  (blocked? node-map chk-key) false
                  (not (visited? node-map chk-key)) true
                  (= node-key chk-key) false
                  (better-path-cost-found? node-map chk-key node-key) true
                  :else false))]
    (let [adjacent (connected-nodes node-map node-key)]
      (filter #(valid? node-map %) adjacent))))


