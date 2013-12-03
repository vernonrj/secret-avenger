(ns a-star.core
  (:gen-class))

;; Includes

(use 'a-star.astarp)
(use 'a-star.search)
(use 'a-star.nodemap)
(use 'a-star.maze)

;; Forward declarations

(declare a-star)
(declare generate-path)



(defn a-star
  "A* search implementation.
  node-map must support the astarp A* protocol"
  [node-map]
  (let [start-key (:start-key node-map)
        end-key (:end-key node-map)]
    (loop [chosen-node start-key
           node-map (open node-map start-key nil)]
      (cond
        (closed? node-map end-key) node-map
        :else (let [next-stats (open-adjacent node-map chosen-node)]
                (if (not-any? #(open? node-map %) (node-keys next-stats))
                  node-map
                  (recur
                    (find-lowest-cost next-stats end-key)
                    next-stats)))))))



(defn generate-path
  "Use the A* algorithm to create a short path from start to dest"
  [node-map]
  (walk-path (a-star node-map)))




(defn -main
  "solves a maze of arbitrary size"
  ([] (-main 10))
  ([size] (print-maze (a-star (initialize-maze (Long. size))))))

