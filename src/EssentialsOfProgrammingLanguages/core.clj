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


;exersise inlining

(comment (defn inlining-subst [new old slist]
  (let [inline-partial (partial inlining-subst new old)
        subs-exp (fn  [sexp]
                   (if (symbol? sexp)
                     (if (= sexp old) new sexp)
                     (inline-partial sexp)))]
    (if (empty? slist)
      '()
      (cons
       (subs-exp (first slist))
       (inline-partial (rest slist)))))))

