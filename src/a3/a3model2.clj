(ns a3.model2
  (:use clojure.test)
  (:require [a3.a3fast :as p1]))

;implementation of IBM translation model 2
(with-test
  (defn initial-q-counts [enco esco]
    (apply merge (let [[M L] (mapv #(apply max (mapv count %)) [esco enco])
          lval (float (/ 1 (inc L)))]
      (for [j (range (inc L)) i (range 1 (inc M))] {[ j i L M] lval}))))
  (is (= {[2 2 2 2] 0.33333334, [2 1 2 2] 0.33333334, [1 2 2 2] 0.33333334, 
          [1 1 2 2] 0.33333334, [0 2 2 2] 0.33333334, [0 1 2 2] 0.33333334}
         (initial-q-counts p1/ens p1/ess))))

(defn initial-q-counts1 [enco esco]
  (apply merge
  (for [k (range (count esco))]
    (let [[esent spasent] (mapv #(nth % k) [enco esco])
          [l m] (mapv count [esent spasent])
          lval (float (/ 1 (inc l)))]
      (apply merge (for [j (range (inc l))
            i (range 1 (inc m))] 
        {[j i l m ] lval}))))))

;test fixture
(def iqc  (initial-q-counts1 p1/ens p1/ess))

(with-test
  (defn get-delta-index [enco esco]
    (for [k (range (count enco))
          i (range 1 (inc (count  (nth esco k))))
          j (range (inc (count  (nth enco k))))]
      [(inc k) i j]))
  (is (= '([1 1 0] [1 1 1] [1 1 2] [1 2 0] [1 2 1] [1 2 2])
         (take 6 (get-delta-index p1/ens p1/ess)))))

(with-test
  (defn delta-for-model2-calc
    " k is sentence number.i is spanish word index, j is english word index("
    [enco esco tq-map [k i j]]
    (let [[spasent ensent ] (mapv #(nth % (dec k)) [esco enco])
          [L M] (mapv count [ensent spasent])]      
      (* (tq-map [j i L M]) 
         (tq-map [(nth spasent (dec i)) 
                  (nth (cons "NULL" ensent) j)]))))
  (is (> 0.001
        (Math/abs (- 0.0486 (delta-for-model2-calc p1/ens p1/ess 
                                                   (into p1/model1-tvalues iqc) [1 1 0]))))))

(with-test
  (defn delta
    "returns a map of deltas"
    [enco esco tq-map]
    (let [delta-indexes (get-delta-index enco esco) 
          pfn (partial delta-for-model2-calc enco esco tq-map)
          numer_map (apply merge (mapv #(assoc {} % (pfn %)) delta-indexes))
          denom_map (reduce (fn[rmap [[i j k] v]]
                              (assoc rmap [i j] 
                                     (if-let [cval (rmap [i j])]
                                       (+ v cval) v))) {} numer_map)]
      (apply merge 
             (pmap (fn[[[i j k :as n] v]] {n (/ v (denom_map [i j]))}) numer_map))))
  (is (> 0.001
         (Math/abs (- 0.3333 ((delta p1/ens p1/ess 
                                     (into p1/model1-tvalues iqc)) [3 1 0]))))))

(defn cjilm
  "define c(j i l m) to be the number of times we see an English sentence of length
  l, and a French sentence of length m, where word i in French is aligned to word
  j in English.
   returns  a map containing c(j i l m), c (i l m), c(engword spaword), c(engword)"
  [enco esco delta]
  (apply merge-with + 
         (for [k (range (count esco))]
           (let [[esent spasent] (mapv #(nth % k) [enco esco])
                 [l m] (mapv count [esent spasent])]
             (apply merge-with + 
                    (for [j (range (inc l))
                          i (range 1 (inc m))]
                      (let [mval (delta [(inc k) i j])
                            [engword spaword] [(nth (cons "NULL" esent) j) (nth spasent (dec i))]]
                      (reduce (fn[x y] (assoc x y mval)) {} 
                              [[j i l m ] [i l m] [engword spaword] [engword]]))))))))

;test fixture        
(comment (def allc (cjilm p1/ens p1/ess (delta p1/ens p1/ess (into p1/model1-tvalues iqc)))))

(defn get-t-and-q-vals 
  "returns a map containing t and q counts"
  [allc]
  (reduce 
    (fn[inmap [k v]]
      (let [cnt (count k)]
        (cond (== 2 cnt)
          (let [[eword sword] k]
            (assoc inmap [sword eword] (float (/ v (allc [eword])))))
          (== 4 cnt)
          (let [[j i l m] k]
            (assoc inmap k (float (/ v (allc [i l m])))))
          (== 1 1) inmap))) 
    {} allc))

(comment (def tq-map (get-t-and-q-vals allc)))

(defn iterfn 
  "run one iteration of model 2"
  [{:keys [enco esco tq-map] :as m}]
  (let [deltares (delta enco esco tq-map) ]
    (assoc m :tq-map (get-t-and-q-vals (cjilm enco esco deltares)))))

(defn get-tq-map-iter
  "returns the tq-map after n iterations"
  [param-map num-iter]
  (:tq-map
    (last (take (inc num-iter) (iterate iterfn param-map)))))

;test fixture
(comment (def tq-2
           (get-tq-map-iter {:enco p1/ens :esco p1/ess :tq-map (into p1/model1-tvalues iqc)} 2)))

(with-test 
  (defn get-alignment-seq
    "returns a seq where each item is an alignment in a sentence"
    [tq-map enco esco ]
    (apply concat 
           (for [k (range (count enco))]
             (let [[e1 s1] (mapv #(nth % k) [enco esco])
                   [l m] (mapv count [e1 s1])
                   esent (cons "NULL" e1)
                   spasent s1]
               (map first 
                    (for [i (range 1 (inc m))]
                      (apply (partial max-key second) 
                             (for [j (range (inc l))]
                               (let [q (tq-map [j i l m])
                                     tfe (tq-map [(nth spasent (dec i)) (nth esent j)])]
                                 [[(inc k) i j] (* q tfe)])))))))))
  (is (= '([1 1 2] [1 2 2] [2 1 1] [2 2 2] [3 1 1] [3 2 2])
         (get-alignment-seq tq-2 p1/ens p1/ess))))

(defn write-alseq-tofile
  "write alseq to opfile"
  [opfile alseq]
  (spit (str p1/dir opfile)
        (clojure.string/join "\n" 
                             (map (fn[[a b c]] (str a " " c " " b)) alseq))))

;(write-alseq-tofile "model2.test.key" alseq)

(defn write-test-op
  [tq-map encorp escorp opfile]
  (let [fnx (fn [corp ] (mapv #(vec (.split %1 " ")) (.split corp "\n")))
        [dens dess] (mapv #(fnx (slurp (str p1/dir %))) [encorp escorp])
        alseq (get-alignment-seq tq-map dens dess)]
    (write-alseq-tofile opfile alseq)))

(comment 
(def fin (slurp (str p1/dir "model2dev.key")))
(spit (str p1/dir "model2dev2.key")
      (clojure.string/join "\n" 
                           (map (fn[x] (let [[a b c]  (.split x " ")]
                                         (str a " "c" " b) )) (.split fin "\n")))))