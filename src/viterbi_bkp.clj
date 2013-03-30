(ns viterbi_bkp
  (:use clojure.test)
  (:use q1))

(defn make-ngram-counts [gcr]
  "returns a map where keys are the ngrams and values are counts for those ngrams
   for e.g. [:I-GENE :O :I-GENE] 3491"
  (let [;gcr (q1/read-train-file inpfile "\n")
        t2 (remove #(.equalsIgnoreCase "WORDTAG" (second (.split % " "))) gcr)]
    (reduce merge
            (let [fnx (fn[x] (.split x " "))
                  fnb (fn[x] (let [spl (fnx x)] {(mapv keyword (drop 2 spl))
                                                 (java.lang.Integer/parseInt (first spl))}))] 
              (map fnb t2)))))

(defn q [{:keys [tagseq grammap] :as m}]
  "probability of v given w,u. That is count(v,w,u)/count(w,u)"
  (float (/ (grammap tagseq) (grammap (pop tagseq)))))

(deftest testngramcounts
  (let [gcr (q1/read-train-file "D:\\studies\\nlp\\h1-p\\gene.counts.mod1" "\n")
        grammap (make-ngram-counts gcr)]
    (is (< (java.lang.Math/abs (- 0.37573153 (q {:grammap grammap :tagseq [:O :I-GENE :I-GENE]}))) 0.001))
    (is (< (java.lang.Math/abs (- 0.5949308 (q {:grammap grammap :tagseq [:I-GENE :I-GENE]}))) 0.001))))

(defn e [{:keys [word postag wordmap onegram] :as m}]
  "returns emission probability for word"
  (do ;(println (str "inputs to e " word " postag " postag))
  (let [w1 (wordmap word)
        w2 (if (nil? w1) (wordmap "_RARE_") w1)]
    (float (/ (get w2 postag 0) (onegram postag))))))

(deftest testemissioncounts
  (let [gcr (q1/read-train-file "D:\\studies\\nlp\\h1-p\\gene.counts.mod1" "\n")
        wordmap (q1/makewordmap gcr)
        onegram-map (q1/get-1gram-map gcr)
        inmap {:wordmap wordmap :onegram onegram-map}]
    ;(e (assoc inmap :word "2A" :postag :I-GENE)))
    (is (< (java.lang.Math/abs (- 8.170882E-4 (e (assoc inmap :word "increased" :postag :O)) 0.001))))
    (is (< (java.lang.Math/abs (- 2.1912738E-4 (e (assoc inmap :word "2A" :postag :I-GENE)) 0.001))))
    ;(is (< (java.lang.Math/abs (- 2.1912738E-4 (e (assoc inmap :word "STAT5A" :postag :I-GENE)) 0.001)))))
  ))

(defn kset [i]
  "returns the set k depending on the index i"
  (if (or (= i 0) (= i -1)) (hash-set :*) (hash-set :I-GENE :O)))

(defn bc [k]
  "returns the possible permutations of u,v" 
  (let [v (hash-set :O :I-GENE)
        u (if (= 1 k) (hash-set :*) v)]
    (for [i u j v] [i j])))

(defn abc [k]
  "returns all permutations of [a b c] for a given index of k,
   or [y-2 y-1 y]"
  (let [uvseq (uv k) 
        kseq (kset (- k 2))] 
    ;(println (str (vec uvseq) " " kseq))
    (for [a kseq [b c] uvseq ] [a b c])))

(deftest uv-sequences
  (is (= '([:* :O] [:* :I-GENE]) (bc 1)))
  (is (= '([:O :O] [:O :I-GENE] [:I-GENE :O] [:I-GENE :I-GENE]) (bc 2)))
  (is (= '([:* :* :O] [:* :* :I-GENE]) (abc 1)))
  (is (= '([:* :O :O] [:* :O :I-GENE] [:* :I-GENE :O] [:* :I-GENE :I-GENE]) (abc 2)))
  (is (= '([:O :O :O] [:O :O :I-GENE] [:O :I-GENE :O] [:O :I-GENE :I-GENE] 
                      [:I-GENE :O :O] [:I-GENE :O :I-GENE] [:I-GENE :I-GENE :O] 
                      [:I-GENE :I-GENE :I-GENE])
         (abc 3))))

(defn r0 [{:keys [tagseq word grammap wordmap onegram] :as m}]  
  (let [qres (q m) 
        eres (e (assoc m :postag (last tagseq)))]    
    (* qres eres)))

(defn r 
  "returns a vector, each element is a vectors of 2 elements,
  first is the sequence of tags and the second is score for that tag sequence "
  [{:keys [k word grammap wordmap onegram] :as m}]
  (let [rs (abc k)]    
    (map #(vector % (r0 (assoc m :tagseq %))) rs)))

(def r-mem 
  "memoized version of r"
  (memoize r))

(defrecord gramdata [ grammap wordmap onegram] )
(def gd1 (let [gcr (read-train-file "D:\\studies\\nlp\\h1-p\\gene.counts.mod1" "\n")
      grammap (make-ngram-counts gcr)
      wordmap (q1/makewordmap gcr)
      onegram-map (q1/get-1gram-map gcr)]
           (gramdata. grammap wordmap onegram-map)))

(deftest test-r
  (let [gcr (read-train-file "D:\\studies\\nlp\\h1-p\\gene.counts.mod1" "\n")
        grammap (make-ngram-counts gcr)
        wordmap (q1/makewordmap gcr)
        onegram-map (q1/get-1gram-map gcr)
        inmap {:wordmap wordmap :onegram onegram-map :grammap grammap}]
    (is (= '([[:* :* :O] 5.7949513575294986E-5] [[:* :* :I-GENE] 2.43474877303744E-14])
           (r-mem (assoc inmap :k 1 :word "exclusively"))))))

(def pistart {:pi-results {[0 :* :*] 1} :k 0 :best-path {} })
(with-test 
  (defn add-best-path 
    "appends the best path so far based on max probability for the kth iteration of pi(k,u,v)"
    [{:keys [k pi-results best-path] :as ps}]
    (let [maxval (apply (partial max-key second) (filter #(= ((% 0) 0) k) pi-results))]
      (assoc ps :best-path (assoc best-path k (peek (maxval 0))))))
  (is (= {:k 1, :best-path {1 :I-GENE, 0 :*}, :pi-results {[1 :* :O] 0.2, [1 :* :I-GENE] 0.5}}
         (add-best-path {:pi-results {[1 :* :I-GENE] 0.5 [1 :* :O] 0.2} :k 1 :best-path {0 :*} })))
  (is (= {:pi-results {[0 :* :*] 1}, :k 0, :best-path {0 :*}}
         (add-best-path pistart))))

(with-test
  (defn addk 
    "returns a map with entries in :calc that have pi-values to be calculated next"
    [{:keys [k best-path] :as pi-table}]
    (let [kplus (inc k)
          bcvals (bc kplus)]    
      (assoc (add-best-path pi-table) :calc (for [i bcvals] (into [kplus] i)) :k kplus)))  
  (is (= {:calc '([1 :* :O] [1 :* :I-GENE]), :pi-results {[0 :* :*] 1}, :k 1, :best-path [:*]}
         (addk pistart)))
  (is (= {:k 2, :calc '([2 :O :O] [2 :O :I-GENE] [2 :I-GENE :O] [2 :I-GENE :I-GENE]), :best-path [:* :I-GENE]}
         (select-keys 
           (addk { :k 1,:pi-results{ [0 :* :*] 1, [1 :* :I-GENE] 0.5, [1 :* :O] 0.4},:best-path [:*] })
         [:calc :k :best-path]))))

(defn viterbi-iterfn
  "adds one iteration of viterbi, adding to the pi(k,u,v) table"
  [{:keys [cache] :as gd1}]  
  (let [tocalc (addk cache)]
    (calc-pi (assoc gd1 :cache tocalc))))

(def test-sentence-1 ["STAT5A" "mutations" "in" "the" "Src" "homology"])
(def test-sent-2 ["Therefore", ",", "we", "suggested", "that", "both", "proteins", "might", "belong", "to", "the", "PLTP", "family", "."])
(def test-sent-3 
  ["More", "importantly", ",", "this", "fusion", "converted", "a", "less", "effective", "vaccine", 
   "into", "one", "with", "significant", "potency", "against", "established", "E7", "-", "expressing", "metastatic", "tumors", "."])
                  
(defn calc-pi-table
  "calculates the value of pi(k,u,v) for the sentence"
  [{:keys [sentence] :as m}]
  (:cache (last (take (inc (count sentence)) 
                      (iterate viterbi-iterfn m)))))

(with-test
  (defn get-y-seq 
    "returns a vector of words to pos tags for the given sentence"
    [{:keys [sentence] :as m}]
    (let [tr1 (calc-pi-table m)
          s1 (get-y (assoc m :cache tr1))]
      (for [i (range 0  (count sentence))]
        [(sentence i) (name (s1 (inc i)))])))
  (is (= '(["Therefore" "O"] ["," "O"] ["we" "O"] ["suggested" "O"] ["that" "O"] 
                             ["both" "O"] ["proteins" "O"] ["might" "O"] ["belong" "O"] 
                             ["to" "O"] ["the" "O"] ["PLTP" "O"] ["family" "O"] ["." "O"])
         (get-y-seq (assoc gd1 :sentence test-sent-2 :cache pistart))))
  (is (= '(["More" "O"] ["importantly" "O"] ["," "O"] ["this" "O"] ["fusion" "O"] 
                        ["converted" "O"] ["a" "O"] ["less" "O"] ["effective" "O"] 
                        ["vaccine" "O"] ["into" "O"] ["one" "O"] ["with" "O"] ["significant" "O"] 
                        ["potency" "O"] ["against" "O"] ["established" "O"] ["E7" "I-GENE"] 
                        ["-" "I-GENE"] ["expressing" "O"] ["metastatic" "O"] ["tumors" "O"] ["." "O"])
         (get-y-seq (assoc gd1 :sentence test-sent-3 :cache pistart)))))

(count (:best-path tr1)) 
(filter (fn[[k v]] (= (k 0) 14)) (:pi-results tr1))

(with-test
  (defn calc-pi 
    "calculates pi(k,u,v) for all the elements in :calc in cache"
    [{:keys [cache] :as m}]
    (let [pi-res (for [i (cache :calc)] 
                   ;for every pi(fixed k,u,v) 
                   (let [pi-args (getargs i)
                         fnx (fn[ar] (calc-k1 (merge ar m)))
                         pi2 (map #(select-keys % [:prob :kbc]) (map fnx pi-args))]                     
                     (apply (partial max-key :prob) pi2)))
          pi-res-map (reduce #(assoc  %1 (:kbc %2) (:prob %2)) (cache :pi-results) pi-res)
          ncache (assoc (dissoc cache :calc) :pi-results pi-res-map) ]
      (assoc m :cache ncache)))
  (is (= {[1 :* :I-GENE] 2.4347487730374372E-14, [1 :* :O] 2.6077281290781694E-5, [0 :* :*] 1} 
         (:pi-results (:cache (let [cache {:calc '([1 :* :O] [1 :* :I-GENE]),:pi-results{ [0 :* :*] 1}}]
           (calc-pi (merge gd1  {:sentence ["just"] :cache cache})))))))
  (is (= {[2 :I-GENE :I-GENE] 1.0965650318678921E-29, [2 :I-GENE :O] 4.8988566822069757E-20, 
          [2 :O :I-GENE] 2.3697466553321795E-20, [2 :O :O] 1.1630832286609045E-10, 
          [1 :* :O] 2.6077281290781694E-5, [1 :* :I-GENE] 2.4347487730374372E-14, [0 :* :*] 1} 
         (:pi-results (:cache (let [cache {:calc '([2 :O :O] [2 :O :I-GENE] [2 :I-GENE :O] [2 :I-GENE :I-GENE]), 
                                  :pi-results {[1 :* :I-GENE] 2.4347487730374372E-14, 
                                               [1 :* :O] 2.6077281290781694E-5, [0 :* :*] 1}}]
           (calc-pi (merge gd1  {:sentence ["just" "another"] :cache cache}))))))))
                  
(with-test
  (defn getargs 
    "returns the arguments for pi, e and q methods"
    [[k b c] ]
    (let [w (kset (- k 2))]
      (for [i w] 
        {:kbc [k b c] :pi  [(dec k) i b] :ev {:postag c} :qv  {:tagseq  [i b c]}} )))
  (is (= '({:kbc [2 :* :O], :pi [1 :* :*], :ev {:postag :O}, :qv {:tagseq [:* :* :O]}})          
         (getargs [2 :* :O])))
  (is (= '({:kbc [1 :* :O], :pi [0 :* :*], :ev {:postag :O}, :qv {:tagseq [:* :* :O]}}) 
         (getargs [1 :* :O])))
  (is (= '({:kbc [3 :O :O], :pi [2 :O :O], :ev {:postag :O}, :qv {:tagseq [:O :O :O]}} 
            {:kbc [3 :O :O], :pi [2 :I-GENE :O], :ev {:postag :O}, :qv {:tagseq [:I-GENE :O :O]}})
         (getargs [3 :O :O]))))

(getargs [14 :I-GENE :I-GENE])

(with-test
  (defn calc-k1
    "calculated log-sum-exp of the probabilities for pi, q and e"
    [{:keys [pi ev qv sentence kbc cache ] :as gramdata}]
    (let [res [(q (merge qv gramdata))
               (e (assoc (merge ev gramdata)
                         :word (sentence (dec (kbc 0))))) ((cache :pi-results) pi)]]
      ;(println (str "calc-k1: kbc " kbc "res " res))
      (assoc gramdata :prob
             (java.lang.Math/exp (apply + (map #(java.lang.Math/log %) res))))))
  (is (=  0.07886479324909336
         (:prob (calc-k1 (merge {:kbc [1 :* :O], :pi [0 :* :*], :ev {:postag :O}, :qv {:tagseq [:* :* :O]} 
                                 :sentence ["STAT5A"] :cache pistart} gd1))))))

(defn get-y-stop 
  "returns arguments for calculating y,y-1 "
  [n]
  (for [i (bc n)]
    {:pi (into [n] i) :qv {:tagseq (conj i :STOP)}})) 

(defn get-pos-tag 
  "returns the part of speech tag with max probability for argseq"
  [{:keys [argseq cache] :as m}]
  (apply (partial max-key :y)
         (for [i argseq]
           (let [yval (:y (calc-y (merge (assoc m :cache cache) i)))]
             {:y yval :postag (second (:tagseq (:qv i)))}))))

(get-pos-tag (assoc gd1 :argseq (get-y-stop 14) :cache tr1))

(defn get-y 
  [{:keys [sentence cache] :as m}]
  (let [n (count sentence)
        res (for [i [n (dec n)]]
              (assoc (get-pos-tag (assoc m :argseq (get-y-stop i))) :n i) )
        resty (for [i (range (- n 2) 0 -1)]
                ((cache :best-path) i))]
    (reduce #(assoc %1 (:n %2) (:postag %2)) (cache :best-path) res)))

(defn calc-y
  "calculate y (pos tag)"
    [{:keys [pi qv cache ] :as gramdata}]
    (let [res [(q (merge qv gramdata))
               ((cache :pi-results) pi)]]      
      (assoc gramdata :y
             (java.lang.Math/exp (apply + (map #(java.lang.Math/log %) res))))))

(run-tests 'viterbi/addk)

(def gd (slurp "D:\\studies\\nlp\\h1-p\\gene.dev"))
(def senten 
  (map vec (remove #(= % '("")) (partition-by (fn[x] (= x "")) (.split gd "\n")))))

(defn write-dev-output [infile outfile]
  (spit outfile
        (let [gd (slurp infile)
              senten (map vec (remove #(= % '("")) (partition-by (fn[x] (= x "")) (.split gd "\n"))))]
          (clojure.string/join (for [i senten]
                                 (let [g2 (get-y-seq (assoc gd1 :sentence i :cache pistart))]
                                   (str (clojure.string/join "\n" (map (fn[[x y]] (str x " " y)) g2)) "\n\n")))))))

(write-dev-output "D:\\studies\\nlp\\h1-p\\gene.dev"
                  "D:\\studies\\nlp\\h1-p\\gene.dev.output")
;got a F1-score of 0.32 instead of 0.40.

(write-dev-output "D:\\studies\\nlp\\h1-p\\gene.test"
                  "D:\\studies\\nlp\\h1-p\\gene_test.p2.out")


