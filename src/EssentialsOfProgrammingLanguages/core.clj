(ns EssentialsOfProgrammingLanguages.core)

(defn timetester [lst f]
  (dotimes [_ 5] (time (f lst))))

;1.2.1 list-lengh

(defn list-length [lst]
  (if (empty? lst) 0 (+ 1 (list-length (rest lst)))))

;my
(defn clj-list-length [lst]
  (loop [xs lst n 0]
    (if (empty? xs)
      n
      (recur (rest xs) (inc n)))))

;1.2.2 nth
(defn clj-nth [lst n]
  (if (zero? n)
    (first lst)
    (recur (next lst) (dec n))))

(defn scheme-nth [lst n]
  (if (empty? lst)
    (throw "to short")
    (if (zero? n)
      (first lst)
      (recur (rest lst) (dec n)))))

;1.2.3

(defn scheme-remove-first [syb lst]
  (if (empty? lst)
    '()
    (if (= (first lst) syb)
      (rest lst)
      (cons (first lst) (scheme-remove-first syb (rest lst))))))

(defn clj-remove-first [item coll]
  (let [[pre post] (split-with #(not= % item) coll)]
    (concat pre (rest post))))

;bonus
(defn clj-remove-all [item lst]
  (filter (comp not #(= item %)) lst))

;1.2.4
;dont understand that! Don't know how to implement in clojure will be
;remet later in the book

(defn occurs-free? [var exp]
  (cond
   ((symbol? exp) (= var exp))
   ((= (first exp) 'fn)
    (and
     (not (= var (first (second exp))))
     (occurs-free? var (nth exp 2))))
   :else
    (or
     (occurs-free? var (first exp))
     (occurs-free? var (second exp)))))

;1.2.5 subst
(declare subst-in-s-exp subst)
(defn subst [new old slist]
  (if (empty? slist)
    '()
    (lazy-seq (cons
     (subst-in-s-exp new old (first slist))
     (subst new old (rest slist))))))

(defn subst-in-s-exp [new old sexp]
  (if (symbol? sexp)
    (if (= sexp old) new sexp)
    (subst new old sexp)))

;1.3 Auxiliary Procedures and context arguments

(defn scheme-number-elements-form [coll n]
  (if (seq coll)
    (cons (list n (first coll))
          (scheme-number-elements-form (rest coll) (inc n)))
    '()))
(defn number-elements [coll]
  (scheme-number-elements-form coll 0))

;clj
(defn clj-number-elements-form
  ([coll] (clj-number-elements-form coll 0))
  ([coll n] (map (fn [a b] [a b]) (iterate inc n) coll)))

;scheme sum 1
(defn scheme-list-sum [loi]
  (if (seq loi)
    (+ (first loi)
       (scheme-list-sum (rest loi)))
    0))

;scheme sum 2
(defn partial-vector-sum [v n]
  (if (zero? n)
    (nth v 0)
    (+ (nth v n)
       (partial-vector-sum v (dec n)))))

(defn vector-sum [v]
  (let [n (count v)]
    (if (zero? n)
      0
      (partial-vector-sum v (dec n)))))
;clj idiomatic (dosn't make the point of this chapter)
(defn sum-of-vec [v]
  (reduce + v))


;1.4 

;1.15
(defn duple [mal x]
  (loop [lst '() mal mal]
    (if (zero? mal) lst (recur (conj lst x) (dec mal)))))

;1.16
(defn invert [coll]
  (loop [coll coll newcoll []]
    (if (seq coll)
      (recur (vec (rest coll))
             (conj newcoll (vec (reverse (first coll)))))
      newcoll)))
;1.17
(defn down [coll]
  (loop [newlist [] n 0]
    (if (= (count coll) n)
      newlist
      (recur (conj newlist (list (nth coll n))) (inc n)))))

;1.18

(defn clj-swaper [lst c1 c2]
  (loop [coll lst output []]
    (if (seq coll)
      (recur (rest coll)
             (conj output (if (= c1 (first coll))
                            c2
                            (first coll))))
      output)))


;1.19

(defn clj-coll-set [coll index x]
  (loop [coll coll output []  i 0]
    (if (seq coll)
      (recur (rest coll) (conj output (if (= i index) x (first coll))) (inc i))
      output)))

;1.2

(defn count-occurrences [item coll]
  (cond
     (empty? coll) 0
     (seq? (first coll)) (+ (count-occurrences item (first coll))
                            (count-occurrences item (rest coll)))
     (= item (first coll)) (inc (count-occurrences item (rest coll)))
     :else (+ 0 (count-occurrences item (rest coll)))))

(defn count-occurrences-tail-call [item coll]
  (count (filter (partial = item) (flatten coll))))

;;1.21

(defn product [sos1 sos2]
  (mapcat (fn [item1]
           (map (fn [item2]
                  [item1 item2]) sos2))  sos1))
;;1.22

(defn clj-filter-in [pred coll]
  (loop [coll coll output []]
    (if (seq coll)
      (if (pred (first coll))
        (recur (rest coll) (conj output (first coll)))
        (recur (rest coll) output))
      output)))

;;1.23

(defn coll-index [pred coll]
  (count (take-while #(not (number? %)) coll)))

;;1.24

(defn -every? [pred coll]
  (loop [coll coll]
    (if (empty? coll)
      true
      (if (pred (first coll))
        (recur (rest coll))
        false))))

;;1.25

(def -exists? (complement -every?))

;;1.26

(defn up [coll]
  (println coll)
  (cond
   (empty? coll) coll
   (seq? (first coll)) (concat (first coll) (up (rest coll)))
   :else (conj (up (rest coll)) (first coll))))

;;1.27

(defn -flatten [coll]
  (cond
   (empty? coll) coll
   (seq? (first coll)) (concat (flatten (first coll)) (flatten (rest coll)))
   :else (conj (rest coll) (first coll))))

;;1.28 merge

(defn -merge [coll1 coll2]
  (sort (concat coll1 coll2)))

;1.31

(def tree1 '(bar 1 (foo 1 2)))
(def tree2
     '(baz
      (bar 1 (foo 1 2))
      (biz 4 5)))



;2 Data Abstruction

2.1 

(def base 16)

(defn zero []
  ())

(defn is-zero? [n]
  (empty? n))

(defn predecessor [n]
  (let [newnum (dec (first n))]
         (if (<= newnum 0)
           (rest n)
           (cons newnum (rest n)))))

(defn cessor [base n f]
  (if (seq n)
    (loop [lst n output '()]
      (if (seq lst)
        (let [newnum (f (first lst))]
         (if (< newnum base)
           (concat output  (cons newnum (rest lst)))
           (recur (rest lst) (conj output (first lst)))))
        output))))

(defn succssor [n]
  (if (= n (cessor base n inc))
    (cons 1 n)))

(defn getnum [n base]
  (apply + (map (fn [hoch num] (* num (apply * (repeat hoch base))))  n (iterate inc 0))))

