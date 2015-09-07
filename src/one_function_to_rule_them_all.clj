(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [acc e] (if (empty? acc)
                          (conj acc e)
                          (conj acc x e)))
            [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc e] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc e] (conj acc e)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [mm e] (if (empty? mm)
                       [e e]
                       (let [min-v (first mm)
                             max-v (second mm)]
                         (cond
                           (< e min-v) [e max-v]
                           (> e max-v) [min-v e]
                           :else [min-v max-v]))))
          [] a-seq))

(defn insert [sorted-seq n]
    (loop [loop-seq sorted-seq
           new-seq []]
      (let [current-e (first loop-seq)]
        (cond
          (empty? loop-seq) (conj new-seq n)
          (< n current-e) (concat (conj new-seq n) loop-seq)
          :else (recur (rest loop-seq) (conj new-seq current-e))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [val] true))
  ([x] x)
  ([x y] (fn [val] (and (x val) (y val) y)))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])