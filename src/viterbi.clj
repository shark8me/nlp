(ns viterbi
  (:use clojure.test)
  (:use q1))

;implementation of the viterbi algorithm for an HMM based tagger
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

(defrecord gramdata [ grammap wordmap onegram] )
(def gd1 (let [gcr (q1/read-train-file "D:\\studies\\nlp\\h1-p\\gene.counts.mod1" "\n")
      grammap (make-ngram-counts gcr)
      wordmap (q1/makewordmap gcr)
      onegram-map (q1/get-1gram-map gcr)]
           (gramdata. grammap wordmap onegram-map)))

(deftest testngramcounts
  (is (< (java.lang.Math/abs (- 0.57785 (q (assoc gd1 :tagseq [:O :I-GENE :I-GENE])))) 0.001))
  (is (< (java.lang.Math/abs (- 0.5949308 (q (assoc gd1 :tagseq [:I-GENE :I-GENE])))) 0.001)))

(defn e [{:keys [word postag wordmap onegram] :as m}]
  "returns emission probability for word"
  (do ;(println (str "inputs to e " word " postag " postag " wordmap word " (wordmap word) " wordmap rare " (wordmap "_RARE_")))
    (let [w1 (wordmap word)
          ;w2 (if (nil? w1) (wordmap "_RARE_") w1)       
          w2 (if (nil? w1)
             (let [s1 (str "_" (name (q1/getclass word)) "_")
                   s2 (wordmap s1)]
               ;(println (str " word " w1 " not found , replacing with " s1 ))
               s2) w1)]
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

(deftest uv-sequences
  (is (= '([:* :O] [:* :I-GENE]) (bc 1)))
  (is (= '([:O :O] [:O :I-GENE] [:I-GENE :O] [:I-GENE :I-GENE]) (bc 2)))
  )
;test fixture
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
      ;(assoc (add-best-path pi-table) :calc (for [i bcvals] (into [kplus] i)) :k kplus)
      (assoc pi-table :calc (for [i bcvals] (into [kplus] i)) :k kplus)))  
  (is (= {:calc '([1 :* :O] [1 :* :I-GENE]), :pi-results {[0 :* :*] 1}, :k 1, :best-path {0 :*}}
         (addk pistart)))
  (is (= {:k 2, :calc '([2 :O :O] [2 :O :I-GENE] [2 :I-GENE :O] [2 :I-GENE :I-GENE]), :best-path {1 :I-GENE, 0 :*}}
         (select-keys 
           (addk { :k 1,:pi-results{ [0 :* :*] 1, [1 :* :I-GENE] 0.5, [1 :* :O] 0.4},:best-path {0 :*} })
         [:calc :k :best-path]))))

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

(with-test
  (defn calc-k1
    "calculated log-sum-exp of the probabilities for pi, q and e"
    [{:keys [pi ev qv sentence kbc cache ] :as gramdata}]
    (let [res [(q (merge qv gramdata))
               (e (assoc (merge ev gramdata)
                         :word (sentence (dec (kbc 0))))) ((cache :pi-results) pi)]
          calc-val (java.lang.Math/exp (apply + (map #(java.lang.Math/log %) res)))]
          ;calc-val (apply * res)]
      ;(println (str "calc-k1: kbc " kbc " q e pi " res " result " calc-val))
      (assoc gramdata :prob calc-val)))
  (is (=  0.07886479324909336
         (:prob (calc-k1 (merge {:kbc [1 :* :O], :pi [0 :* :*], :ev {:postag :O}, :qv {:tagseq [:* :* :O]} 
                                 :sentence ["STAT5A"] :cache pistart} gd1))))))

(with-test
  (defn calc-pi 
    "calculates pi(k,u,v) for all the elements in :calc in cache"
    [{:keys [cache] :as m}]
    (let [pi-res (for [i (cache :calc)] 
                   ;for every pi(fixed k,u,v) 
                   (let [pi-args (getargs i)
                         fnx (fn[ar] (calc-k1 (merge ar m)))
                         pi2 (map #(select-keys % [:prob :kbc :qv]) (map fnx pi-args))]                     
                     (apply (partial max-key :prob) pi2)))
          pi-res-map (reduce #(assoc  %1 (:kbc %2) (:prob %2)) (cache :pi-results) pi-res)
          bestpath (reduce #(assoc  %1 (:kbc %2) (first (:tagseq (:qv %2)))) (cache :best-path) pi-res)
          ncache (assoc (dissoc cache :calc) :pi-results pi-res-map :best-path bestpath) ]
      (assoc m :cache ncache)))
  (is (= {[1 :* :I-GENE] 0.0, [1 :* :O] 2.466151779965467E-5, [0 :* :*] 1} 
         (:pi-results (:cache (let [cache {:calc '([1 :* :O] [1 :* :I-GENE]),:pi-results{ [0 :* :*] 1}}]
           (calc-pi (merge gd1  {:sentence ["just"] :cache cache})))))))
  (is (= {[2 :I-GENE :I-GENE] 0.0, [2 :I-GENE :O] 1.0872976261372944E-18, [2 :O :I-GENE] 0.0, 
          [2 :O :O] 2.8121617007113696E-9, [1 :* :O] 2.6077281290781694E-5, 
          [1 :* :I-GENE] 2.4347487730374372E-14, [0 :* :*] 1} 
         (:pi-results (:cache (let [cache {:calc '([2 :O :O] [2 :O :I-GENE] [2 :I-GENE :O] [2 :I-GENE :I-GENE]), 
                                  :pi-results {[1 :* :I-GENE] 2.4347487730374372E-14, 
                                               [1 :* :O] 2.6077281290781694E-5, [0 :* :*] 1}}]
           (calc-pi (merge gd1  {:sentence ["just" "another"] :cache cache}))))))))

(defn viterbi-iterfn
  "adds one iteration of viterbi, adding to the pi(k,u,v) table"
  [{:keys [cache] :as gd}]  
  (let [tocalc (addk cache)]
    (calc-pi (assoc gd :cache tocalc))))
                 
(with-test 
  (defn calc-pi-table
    "calculates the value of pi(k,u,v) for the sentence"
    [{:keys [sentence] :as m}]
    (:cache (last (take (inc (count sentence)) 
                        (iterate viterbi-iterfn m)))))
  (is (not(nil? 
            (:best-path (let [test-sent-2 ["Therefore", ",", "we", "suggested", "that", "both", 
                                           "proteins", "might", "belong", "to", "the", "PLTP", "family", "."]]                          
                          (calc-pi-table (assoc gd1 :sentence test-sent-2 :cache pistart))))))))

(def bp1 (let [test-sent-2 ["Therefore", ",", "we", "suggested", "that", "both", 
                                           "proteins", "might", "belong", "to", "the", "PLTP", "family", "."]]                          
                          (calc-pi-table (assoc gd1 :sentence test-sent-2 :cache pistart))))

(with-test 
  (defn bp[{:keys [k y best-path] :as m}]
    (assoc m :k (dec k) :y (assoc y k (best-path [(+ k 2) (y (+ k 1)) (y (+ k 2))]))))
  (is (= {12 :I-GENE, 13 :O, 14 :I-GENE}
         (:y (bp  {:best-path (:best-path bp1) :y {14 :I-GENE 13 :O} :k 12})))))

(with-test
  (defn get-y-firstpart [mapinput]
    (:y (last (take-while (fn[{:keys [k] :as m}] (>= k 0)) 
                          (iterate bp mapinput)))))
  (is (= {1 :O, 2 :O, 3 :O, 4 :O, 5 :O, 6 :O, 7 :O, 8 :O, 9 :O, 10 :I-GENE, 
          11 :I-GENE, 12 :I-GENE, 13 :O, 14 :I-GENE}
         (get-y-firstpart {:best-path (:best-path bp1) :y {14 :I-GENE 13 :O} :k 12}))))

(with-test
  (defn get-y-stop 
    "returns arguments for calculating y,y-1 "
    [n]
    (for [i (bc n)]
      {:pi (into [n] i) :qv {:tagseq (conj i :STOP)}}))
  (is (= '({:pi [10 :O :O], :qv {:tagseq [:O :O :STOP]}} 
            {:pi [10 :O :I-GENE], :qv {:tagseq [:O :I-GENE :STOP]}} 
            {:pi [10 :I-GENE :O], :qv {:tagseq [:I-GENE :O :STOP]}} 
            {:pi [10 :I-GENE :I-GENE], :qv {:tagseq [:I-GENE :I-GENE :STOP]}})
         (get-y-stop 10))))

;(get-pos-tag (assoc gd1 :argseq (get-y-stop 14) :cache tr1))
(defn calc-y
  "calculate y (pos tag)"
    [{:keys [pi qv cache ] :as gramdata}]
    (let [res [(q (merge qv gramdata))
               ((cache :pi-results) pi)]
          finres (java.lang.Math/exp (apply + (map #(java.lang.Math/log %) res)))]
      ;(println (str "calc-y input " pi  "pi input " (cache :pi-results) " calc " res " res " finres))      
      (assoc gramdata :y finres )))

(defn get-pos-tag 
  "returns the part of speech tag with max probability for argseq"
  [{:keys [argseq cache] :as m}]
  (apply (partial max-key :y)
         (for [i argseq]
           (let [yval (:y (calc-y (merge (assoc m :cache cache) i)))]
             {:y yval :postag (second (:tagseq (:qv i)))}))))

(defn get-y 
  [{:keys [sentence cache] :as m}]
  (let [n (count sentence)
        res (for [i [n (dec n)]]
              (assoc (get-pos-tag (assoc m :argseq (get-y-stop i))) :n i) )
        resty (get-y-firstpart (assoc cache :y {n (:postag (first res))
                                            (dec n) (:postag (second res))} 
                                :k (- n 2))) ]
    ;(println (str "last 2 " (vec res)))
    resty))

;test fixtures

(def test-sent-3 ["More", "importantly", ",", "this", "fusion", "converted", "a", 
                     "less", "effective", "vaccine","into", "one", "with", 
                     "significant", "potency", "against", "established", "E7", "-", 
                     "expressing", "metastatic", "tumors", "."])
(def test-sent-2 ["Therefore", ",", "we", "suggested", "that", "both", 
                     "proteins", "might", "belong", "to", "the", "PLTP", "family", "."])
(with-test
  (defn get-y-seq 
    "returns a vector of words to pos tags for the given sentence"
    [{:keys [sentence] :as m}]
    (let [tr1 (calc-pi-table m)
          s1 (get-y (assoc m :cache tr1))]
      ;(println (str " sentence " sentence " tags " s1))
      (for [i (range 0  (count sentence))]
        [(sentence i) (name (s1 (inc i)))])))
  (let [test-sentence-1 ["STAT5A" "mutations" "in" "the" "Src" "homology"]
        test-sent-2 ["Therefore", ",", "we", "suggested", "that", "both", 
                     "proteins", "might", "belong", "to", "the", "PLTP", "family", "."]
        test-sent-3 ["More", "importantly", ",", "this", "fusion", "converted", "a", 
                     "less", "effective", "vaccine","into", "one", "with", 
                     "significant", "potency", "against", "established", "E7", "-", 
                     "expressing", "metastatic", "tumors", "."]]
    (is (= '(["Therefore" "O"] ["," "O"] ["we" "O"] ["suggested" "O"] ["that" "O"] 
                               ["both" "O"] ["proteins" "O"] ["might" "O"] ["belong" "O"] 
                               ["to" "O"] ["the" "O"] ["PLTP" "O"] ["family" "O"] ["." "O"])
           (get-y-seq (assoc gd1 :sentence test-sent-2 :cache pistart))))
    (is (= '(["More" "O"] ["importantly" "O"] ["," "O"] ["this" "O"] ["fusion" "O"] 
                          ["converted" "O"] ["a" "O"] ["less" "O"] ["effective" "O"] 
                          ["vaccine" "O"] ["into" "O"] ["one" "O"] ["with" "O"] ["significant" "O"] 
                          ["potency" "O"] ["against" "O"] ["established" "O"] ["E7" "I-GENE"] 
                        ["-" "I-GENE"] ["expressing" "O"] ["metastatic" "O"] ["tumors" "O"] ["." "O"])
           (get-y-seq (assoc gd1 :sentence test-sent-3 :cache pistart))))))

;(run-tests 'viterbi)

(defn write-dev-output [infile outfile gramrecord]
  (spit outfile
        (let [gd (slurp infile)
              senten (map vec (remove #(= % '("")) (partition-by (fn[x] (= x "")) (.split gd "\n"))))]
          (clojure.string/join (for [i senten]
                                 (let [g2 (get-y-seq (assoc gramrecord :sentence i :cache pistart))]
                                   (str (clojure.string/join "\n" (map (fn[[x y]] (str x " " y)) g2)) "\n\n")))))))

(comment
(def gd2 (let [gcr (q1/read-train-file "D:\\studies\\nlp\\h1-p\\gene.counts.mod2" "\n")
      grammap (make-ngram-counts gcr)
      wordmap (q1/makewordmap gcr)
      onegram-map (q1/get-1gram-map gcr)]
           (gramdata. grammap wordmap onegram-map)))

(write-dev-output "D:\\studies\\nlp\\h1-p\\gene.dev"
                  "D:\\studies\\nlp\\h1-p\\gene.dev.output1"
                  gd1)

(write-dev-output "D:\\studies\\nlp\\h1-p\\gene.dev"
                  "D:\\studies\\nlp\\h1-p\\gene.dev.output2"
                  gd2)
;got a F1-score of 0.32 instead of 0.40.

(write-dev-output "D:\\studies\\nlp\\h1-p\\gene.test"
                  "D:\\studies\\nlp\\h1-p\\gene_test.p2.out" gd1)

(write-dev-output "D:\\studies\\nlp\\h1-p\\gene.test"
                  "D:\\studies\\nlp\\h1-p\\gene_test.p2.out" gd2)

)
