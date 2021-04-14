(ns solutions
  (:require [clojure.set]))

;4clojure solutions

;53 Longest Increasing Sub-Seq

(def l-i-helper (fn [col n]
                  (case (first col)
                    nil (list (list n))
                    (let [r (rest col)
                          f (first col)
                          x (first f)]
                      (if (= (inc x) n)
                        (conj r (conj f n))
                        (conj col (list n)))))))

(def longest-increasing (fn [col]
                          (let [l (apply max-key count (reduce l-i-helper '() col))]
                            (if (> (count l) 2)
                              (vec (reverse l))
                              []))))

;54 Partition a sequence
(def my-partition (fn [n col]
                    (loop [col col
                           new-col []]
                      (if (<= n (count col))
                        (recur (drop n col) (conj new-col (take n col)))
                        (seq new-col)))))

;adereth's solution
(defn ptn [n s]
  (let [next-chunk (take n s)]
    (if (= (count next-chunk) n)
      (cons next-chunk (ptn n (drop n s))))))

;58 Function composition
(def cmp (fn [& fns]
           (fn [& args]
             (let [fns (reverse fns)
                   first-function (first fns)
                   first-return (apply first-function args)]
               (reduce #(%2 %1) first-return (rest fns))))))

;59 Juxtaposition
(def jxt (fn [& fs] (fn [& args]
                       (let [p (fn [f] (apply f args))]
                          (map p fs)))))

;60 Sequence Reductions
(def my-reductions
  (fn
    ([f [x & xs]]
     (my-reductions f x xs))
    ([f n [x & xs]]
     (if x
       (lazy-seq (cons n (my-reductions f (f n x) xs)))
       (cons n nil)))))

;61 Map Construction
(def zm (fn [col1 col2] (reduce #(let [[k v] %2]
                               (assoc %1 k v)) {} (map #(vector %1 %2) col1 col2))))

;62 Re-implement Iterate
(def iter (fn [f x] (lazy-seq (cons x (iter f (f x))))))

;63 Group a Sequence
(def grp (fn [f col]
           (reduce (fn [m [k v]]
             (if-let [old (get m k)]
                (assoc-in m [k (count old)] v)
                (assoc m k [v]))) {} (for [x col :let [fx (f x)]] [fx x]))))

;austintaylor's solution
(def grp2 (fn [f s]
  (reduce (fn [m a]
    (let [x (f a)]
      (assoc m x (conj (get m x []) a)))) {} s)))

;daowen's solution
(def grp3 (fn [f vs]
  (reduce #(merge-with into % {(f %2) [%2]}) {} vs)))

;65 Seqs testing
(def test-seq (fn [s]
                (let [new-s (conj (empty s) [:a 1] [:a 1] [:b 2])]
                  (if (= (count new-s) 2)
                    (if (get new-s :a)
                      :map
                      :set)
                    (if (= (first new-s) [:b 2])
                      :list
                      :vector)))))

;66 Greatest common divisor
(def gcd (fn [a b]
           (if (zero? b)
             a
             (gcd b (mod a b)))))

;67 Prime numbers
(def ps (fn [n]
          (loop [primes [2]
                 col (iterate #(+ 2 %) 3)]
            (if (= (count primes) n)
              primes
              (let [p (first col)]
                (recur (conj primes p) (remove #(zero? (mod % p)) col)))))))

;daowen's solution
(def ps2 (fn [n]
  (loop [ps [2], x 3]
    (if (= n (count ps)) ps
      (let [p (loop [i x]
                (if (some #(= 0 (mod i %)) ps)
                  (recur (+ 2 i))
                  i))]
        (recur (conj ps p) (+ 2 p)))))))

;Sieve
(defn sieve
  [[p & rst]]
  ;; make sure the stack size is sufficiently large!
  (lazy-seq (cons p (sieve (remove #(= 0 (mod % p)) rst)))))

(def primes (sieve (iterate inc 2)))

;69 Merge with a function
(def mw (fn [f m & ms]
          (reduce (fn [m1 m2] (reduce #(let [[k v] %2]
                                         (if-let [i (get % k)]
                                           (assoc % k (f i v))
                                           (assoc % k v))) m1 m2)) m ms)))

;70 Word Sorting
(def word-sort (fn [s]
  (sort-by #(.toLowerCase %) (re-seq #"\w+" s))))

;73 Analyze Tic Tac Toe
(def ttt (fn [board]
           (let [board (reduce #(into % %2) board)]
             (loop [symbols [:x :o]]
               (let [el (first symbols)
                     els (map first (filter #(= (second %) el)
                                  (map-indexed vector board)))]
                 (when el
                   (if (and (= (count els) 3)
                            (= (- (last els) (second els)) (- (second els) (first els))))
                     el
                     (recur (next symbols)))))))))

;74 Filter perfect squares (from String)
(def filter-squares (fn [s]
                       (apply str (interpose "," (filter #(some #{%} '(1 4 9 16 25 36 49))
                               (map read-string (re-seq #"\d+" s)))))))

;75 Euler's Totient Function
(def coprime? (fn [x y]
                (= (gcd x y) 1)))

(def totient (fn [x]
               (let [gcd (fn gcd [a b]
                           (if (zero? b)
                             a
                             (gcd b (mod a b))))
                     coprime? (fn [x y]
                                (= 1 (gcd x y)))]
                 (count (reduce #(if (coprime? %2 x) (conj % %2) %) [] (range 1 (inc x)))))))

;77 Anagram Finder
(def find-anagram (fn [words]
                    (loop [s #{} wrds words]
                      (if (> (count wrds) 1)
                        (let [anagrams (filter #(=
                                                 (into #{} (seq (first wrds)))
                                                 (into #{} (seq %))) wrds)]
                          (if (> (count anagrams) 1)
                            (recur (conj s (into #{} anagrams)) (remove #(some #{%} anagrams) wrds))
                            (recur s (next wrds))))
                        s))))

;daowen's solution:
(def find-anagram2 #(->> % (group-by sort) vals (filter second) (map set) set))

;78 Reimplement Trampoline
(declare triple sub-two stop?)

(defn triple [x]
  #(sub-two (* 3 x)))

(defn sub-two [x]
  #(stop? (- x 2)))

(defn stop? [x]
  (if (> x 50) x #(triple x)))

(def tramp (fn
             ([f]
              (let [g (f)]
                (if (fn? g)
                  (tramp g)
                  g)))
             ([f & xs]
                (let [g (apply f xs)]
                  (if (fn? g)
                    (tramp g)
                    g)))))

;80 Perfect Numbers
(def perfect? (fn [n]
  (loop [i 2 d [1]]
    (if (> i (/ n 2))
      (= (apply + d) n)
      (if (integer? (/ n i))
        (recur (inc i) (conj d i))
        (recur (inc i) d))))))

;daowen's solution:
(def perfect2? (fn [n]
  (->> (range 1 n) (filter #(zero? (mod n %))) (reduce +) (= n))))

;81 Set Intersection
(def intersect (fn [s1 s2]
                 (reduce #(if (some #{%2} s1)
                            (conj %1 %2)
                            %1) #{} s2)))

;daowen's solution
(def intersect-2 (comp set filter))

;83 A Half-Truth
(def half-truth (fn [& bs]
                  (and (not (every? identity bs))
                       (not (every? not bs)))))

;daowen's solution:
(def half-truth2 #(boolean (and (some true? %&) (some false? %&))))

;88 Symetric Difference
(def symmetric-difference (fn [s1 s2]
                           (let [all (into (into [] s1) s2)]
                             (reduce #(if (and (some #{%2} s1) (some #{%2} s2))
                                       %1
                                        (conj %1 %2))
                                    #{} all))))
;daowen's solution
(def symmetric-difference-2 #(clojure.set/difference (clojure.set/union % %2) (clojure.set/intersection % %2)))

;90 Cartesian Product
(def cartesian-product #(set (for [x %1
                                   y %2]
                               [x y])))



;95 To Tree, or not to Tree
(def _tree? (fn tree? [t]
              (or (nil? t)
                  (and (coll? t)
                       (= (count t) 3)
                       (let [[v l r] t]
                         (and
                           (tree? l)
                           (tree? r)))))))

;97 Pascal's Triangle
(def pascals-triangle (fn
                        ([n] (if (= n 1) [1] (pascals-triangle n [1])))
                        ([n col] (if (= (count col) n)
                                   col
                                   (pascals-triangle n (map #(apply + %) (partition 2 1 (concat [0] col [0]))))))))

;daowen's solution
(def pascals-triangle-2 (fn pascal [n]
                          (loop [i 1, row [1]]
                            (if (= n i) row
                              (recur (inc i) (concat [1] (map #(apply + %) (partition 2 1 row)) [1]))))))

;99 Product Digits
(def product-digits (fn [x y]
                      (loop [p (* x y)
                             q 10
                             r '()
                             l [0]]
                          (let [s (rem p q)
                                t (last l)
                                u (/ q 10)]
                            (if (= p s)
                              (conj r (/ (- s t) u))
                              (recur p (* 10 q) (conj r (/ (- s t) u)) (conj l s)))))))

;100 Least Common Multiple
(def lcm (fn [& xs]
           (let [gcd (fn [a b]
                       (if (zero? b)
                         a
                         (recur b (mod a b))))
                 q (reduce gcd xs)]
             (/ (apply * xs) q))))

;102 intoCamelCase
(def camel-case #(let [xs (clojure.string/split % #"-")]
                   (str (first xs)
                        (apply str (map clojure.string/capitalize (rest xs))))))

;aceeca1's solution:
(def camel-case-2 #(clojure.string/replace % #"-[a-z]" (comp clojure.string/upper-case last)))

;107 Simple closures
(def closure #(fn [x] (apply * (repeat % x))))

;118 Re-implement Map
(def my-map (fn my-map [f col]
              (let [x (first col)]
                (if x
                  (lazy-seq (cons (f x) (my-map f (rest col))))
                  (lazy-seq)))))

;120 Sum of square of digits
(def sum-of-square-of-digits (fn [col]
                               (let [get-digits (fn [x] (map #(- (int %) (int \0)) (str x)))]
                                 (count (filter (fn [n] (let [ds (get-digits n)] (< n (reduce + (map #(* % %) ds))))) col)))))

;122 Read a binary number
(def read-binary (fn [s] (apply + (map * (iterate #(* 2 %) 1) (reverse (map #(- (int %) (int \0)) (seq s)))))))

;126 Through the Looking Class
(def x java.lang.Class)

;128 Recognize Playing Cards
(def card (fn [s] 
            (assoc {}
                   :suit ({\D :diamond
                           \H :heart
                           \C :club
                           \S :spade} (first s))
                   :rank ({\T 8
                           \J 9
                           \Q 10
                           \K 11
                           \A 12
                           \2 0
                           \3 1
                           \4 2
                           \5 3
                           \6 4
                           \7 5
                           \8 6
                           \9 7} (last s)))))

;austintaylor's solution:
(def card-2 (fn [[s r]]
              { :suit ({\D :diamond \H :heart \C :club \S :spade} s)
               :rank (.indexOf (seq "23456789TJQKA") r)}))

;135 Infix Calculator
(def infix (fn [& args]
             (loop [xs args v 0 o +]
               (let [x (first xs)]
                 (if x
                   (if (= (class x) Long)
                     (recur (rest xs) (o v x) nil)
                     (recur (rest xs) v x))
                   v)))))

;daowen's solution:
(def infix-2 (fn infix-eval [&[x op y & t]]
         (if op (recur (cons (op x y) t)) x)))

;143 Dot product
(def dot-product #(apply + (map * % %2)))

;157 Indexing Sequences
(def index-seq #(partition 2 (interleave % (range))))

;daowen's solution:
(def index-seq-2 #(map-indexed (fn [i v] [v i]) %))

;156 Map Defaults
(def map-defaults #(reduce (fn [m k] (assoc m k %)) {} %2))

;166 Comparisons
(def comparisons #(let [v (%1 %2 %3)
                        w (%1 %3 %2)]
                    (if v
                      :lt
                      (if w
                        :gt
                        :eq))))

;daowen's solution:
(fn cmp [lt x y]
  (cond
    (lt x y) :lt
    (lt y x) :gt
    :else :eq))

;;; 96 Beauty is Symmetry
(defn inverse-tree [[v l r]]
  (if (nil? v) nil
      [v (inverse-tree r) (inverse-tree l)]))

;;; Pascal's Trapezoid

;;; daowen's solution
(def pascal-trapezoid (fn [row]
                        (lazy-seq
                         (let [head (first row)
                               tail (map #(apply +' %) (partition-all 2 1 row))
                               next (cons head tail)]
                           (cons row (pascal-trapezoid next))))))

(def p-trapezoid (fn [v]
                   (let [next-row (fn [v]
                                    (mapv #(apply +' %)
                                          (partition 2
                                                     (into [0]
                                                           cat [(interleave v v) [0]]))))]
                     (iterate next-row v))))

;;; #146 Trees into tables

(defn tree2table [tree]
  (for [[k v] tree
        :when (= (class v) clojure.lang.PersistentArrayMap)
        :let [k :map]]
    [k v]))


(def pairwise-disjoint?
  (fn [sets]
    (not (false? (reduce (fn [all new-set]
                           (if all
                             (let [intersection (clojure.set/intersection all new-set)]
                               (if (empty? intersection)
                                 (clojure.set/union all new-set)
                                 false))
                             false)) #{} sets)))))

;;; daowen
(def pairwise-disjoint2? #(every?
                           empty?
                           (for [x % y % :when (not= x y)]
                             (clojure.set/intersection x y))))

(def balanced-number? (fn [n]
  (let [n-str (str n)
        d (int (/ (count n-str) 2))
        s (seq n-str)
        i (map #(Character/digit % 10) s)
        left-half-sum (apply + (take d i))
        right-half-sum (apply + (take-last d i))]
    (= left-half-sum right-half-sum))))

;; (defn check-brackets [s]
;;  (let [valid-brackets (fn [bracket-seq open-bracket]


(def brackets-sain?
  (fn [bracket-string]
    (let [bracket-pairs {"}" "{"
                         "]" "["
                         ")" "("}
          open-brackets (fn [open-brackets next-bracket]
                          (if (some #{next-bracket} (keys bracket-pairs))
                            ;; we have a closing bracket
                            (if (= (bracket-pairs next-bracket)
                                   (first open-brackets))
                              ;; we just closed a bracket
                              (rest open-brackets)
                              false)
                            (if (some #{next-bracket} (vals bracket-pairs))
                              (conj open-brackets next-bracket)
                              false)))]
      (empty? (loop [open-bs '() bracket-seq (re-seq #"[\{\}\(\)\[\]]" bracket-string)]
                (if-let [next-bracket (first bracket-seq)]
                  (if-let [next-open-brackets (open-brackets open-bs next-bracket)]
                    (recur next-open-brackets (rest bracket-seq))
                    bracket-seq)
                  open-bs))))))

;; daowen's solution:
(fn check-brackets [in]
  (let [openers (apply sorted-set "{[(")
        closers (apply sorted-set "}])")
        pairs   (zipmap openers closers)]
    (loop [[c & cs] in, stack []]
      (cond
        (nil? c) (empty? stack)
        (openers c) (recur cs (conj stack (pairs c)))
        (closers c) (if (= c (peek stack))
                      (recur cs (pop stack)))
        :else (recur cs stack)))))

