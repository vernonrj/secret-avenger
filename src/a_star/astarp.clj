(ns a-star.astarp)

(defprotocol AStarProtocol
  "A protocol for getting status from a node"
  (open?
    [this node-key]
    "return true if node associated with node-key is open")
  (closed?
    [this node-key]
    "return true if node associated with node-key is closed")
  (visited?
    [this node-key]
    "return true if node associated with node-key has been visited")
  (blocked?
    [this node-key]
    "return true if node associated with node-key is blocked")
  (open
    [this node-key prev-key]
    "open node associated with node-key and set its ancestor a prev-key")
  (close
    [this node-key]
    "close any number of keys in node-map")
  (block
    [this node-key]
    "block any number of keys in node-map so that a path will not use
    any key in node-keys")
  (get-prev-key
    [this node-key]
    "return the ancestor of a node associated with node-key")
  (distance
    [this key1 key2]
    "return the distance between two keys")
  (past-path-cost
    [this node-key]
    "return the past path cost, denoted as g(x)")
  (future-path-cost
    [this node-key]
    "return the future path cost, denoted as h(x)")
  (connected-nodes
    [this node-key]
    "return all nodes that are connected to node indexed by node-key")
  (node-keys
    [this]
    "return a sequence of keys, each of which map to one node"))


(defn to-str-pad
  "Convert numbers to string, ensuring each number is at least padlen
  chars by padding with 0's before each number."
  [padlen & numbers]
  (loop [numbers numbers s ""]
    (if (empty? numbers)
      s
      (let [n (str (first numbers))
            padsize (- padlen (count n))
            padchars (repeatedly padsize #(str 0))]
        (recur (next numbers)
               (apply str (concat [s] padchars [n])))))))

