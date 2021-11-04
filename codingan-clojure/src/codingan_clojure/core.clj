(ns codingan-clojure.core
  (:gen-class))

(defn count' [xs]
  (if (empty? xs)
    0
    (+ 1 (count' (rest xs)))))

(defn reverse' [xs]
  (if (empty? xs)
    []
    (cons (last xs) (reverse' (drop-last xs)))))

(defn fibo [x]
  (if (= x 3)
    [1 1 2]
    (conj (fibo (- x 1)) (+ (last (fibo (- x 1))) (second (reverse (fibo (- x 1))))))))

(defn flatten' [xs]
  (if (coll? xs)
    (mapcat flatten' xs)
    [xs]))

(defn cap? [xs]
  (->> xs
       (filter (set (map char (range 65 91))))
       (apply str)))

(defn com-seq [xs]
  ; p30 compress sequence
  (->> [xs]
       (partition-by identity)
       (map set)
       (apply concat)))

(defn range' [a b]
  (take (- b a) (iterate inc a)))

(defn interleave' [[x & xs :as allx] [y & ys :as ally]]
  (if (or (empty? allx) (empty? ally))
    []
    (concat [x y] (interleave' xs ys))))

(defn fac [x]
  (if (= x 1)
    1
    (* x (fac (dec x)))))

(defn rev-inter
  [xs x]
  (->> xs
       (group-by #(rem % x))
       vals))

(defn rotate' [x xs]
  (let [n (mod x (count xs))]
    n
    (concat (drop n xs) (take n xs))))

(defn long-inc-sub-seq [xs]
  (->> xs
       (partition 2 1)
       (partition-by #(= (+ (first %) 1) (second %)))
       (map flatten)
       (filter #(= (+ (first %) 1) (second %)))
       (group-by count)
       (sort-by key)
       last
       last
       flatten
       dedupe))

(defn partition' [x xs]
  (if (> x (count xs))
    '()
    (cons (take x xs) (partition' x (drop x xs)))))

(defn freq
  [xs]
  (->> xs
       (group-by identity)
       vals
       (#(zipmap (map first %) (map count %)))))