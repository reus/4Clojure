(ns answers)

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

;Seqs testing
(def test-seq (fn [s]
                (let [new-s (conj (empty s) [:a 1] [:a 1] [:b 2])]
                  (if (= (count new-s) 2)
                    (if (get new-s :a)
                      :map
                      :set)
                    (if (= (first new-s) [:b 2])
                      :list
                      :vector)))))

;Greatest common divisor
(def gcd (fn [a b]
           (if (zero? b)
             a
             (gcd b (mod a b)))))

;Prime numbers
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

;156 Map Defaults
(def map-defaults #(reduce (fn [m k] (assoc m k %)) {} %2))

;83 A Half-Truth
(def half-truth (fn [& bs]
                  (and (not (every? identity bs))
                       (not (every? not bs)))))

;daowen's solution:
(def half-truth2 #(boolean (and (some true? %&) (some false? %&))))

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

;81 Set Intersection
(def intersect (fn [s1 s2]
                 (reduce #(if (some #{%2} s1)
                            (conj %1 %2)
                            %1) #{} s2)))

;daowen's solution
(def intersect-2 (comp set filter))
