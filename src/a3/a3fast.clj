(ns a3.a3fast
  (:use clojure.test))

;implementation of IBM translation model 1 
(def dir "/home/kiran/Dropbox/h3/")
(def en (slurp (str dir "corpus.en")))
(def es (slurp (str dir "corpus.es")))

(defn uniq-words
  "returns unique words in the corpus"
  [corpus]
  (reduce conj #{} (flatten (for [i  (.split corpus "\n")]
                              (vec  (.split i " "))))))
         
(defn map-words-to-int 
  "returns a map where a word is a key and the value is an int"
  [uniq]
  (zipmap uniq (range (count uniq))))

(defn cvt-to-int [langmap sent]
  (mapv langmap (.split sent " ")))

(def enmap (map-words-to-int (uniq-words en)))
(def esmap (map-words-to-int (uniq-words es)))
(def corpus
  (let [enmap (map-words-to-int (uniq-words en))
        esmap (map-words-to-int (uniq-words es))
        m1 (mapv (partial cvt-to-int enmap) (.split en "\n"))
        m2 (mapv (partial cvt-to-int esmap) (.split es "\n"))]
    {:encorp m1 :escorp m2 :enmap enmap :esmap esmap}))

(with-test
  (defn add-sent-to-map
    [{:keys [ensent enspa nmap] :as m}]
    (reduce (partial merge-with conj) 
            (reduce (fn[m n] (if (m n) m (assoc m n #{}))) nmap ensent)                                                 
            (for [i ensent j enspa] {i j})))
  (is (= {6133 [7341 4225 4217 479 8213], 4724 [7341 4225 4217 479 8213], 6360 [7341 4225 4217 479 8213], 3530 [7341 4225 4217 479 8213]}
 (add-sent-to-map {:ensent (first (corpus :encorp)) 
                   :enspa (first (corpus :escorp))
                   :nmap {}}))))

(defn corpus-counts
  [en es]
  (reduce (fn[m [en es] ] 
            (add-sent-to-map {:ensent en :enspa es :nmap m})) {}
          (pmap #(vector %1 %2) en es)))

;test fixtures
(def ens (mapv #(.split % " ") ["the car", "the house", "the woman"]))
(def ess (mapv #(.split % " ") ["la coche", "el casa", "el mujer"]))

(with-test       
  (defn n-counts
    "returns the n(word) for each word in english corpus (enco)"
    [enco esco]
    (let [cc (corpus-counts enco esco)]
      (assoc (reduce (fn[m [k v]] (assoc m k v)) {} 
                     (pmap (fn[[k v]] [k (count v)]) cc)) 
             "NULL" (count (reduce #(into  %1 %2) #{} esco)))))
  (is (= {"NULL" 5, "the" 5, "car" 2, "house" 2, "woman" 2}
         (n-counts ens ess))))

(with-test
  (defn initial-t-counts [enco esco]
    (let [cc (n-counts enco esco)
          fnx (fn[x y] 
                (let [sx (set x)
                      sy (set y)]
                  (apply merge (for [i sy j (conj sx "NULL")] 
                                 {[i,j] (float (/ 1 (cc j)))}))))]
      (apply merge (pmap fnx enco esco))))
  (is (= {["la" "car"] 0.5, ["coche" "car"] 0.5, ["el" "NULL"] 0.2, ["mujer" "the"] 0.2, ["casa" "the"] 0.2, 
          ["el" "the"] 0.2, ["la" "NULL"] 0.2, ["mujer" "woman"] 0.5, ["coche" "NULL"] 0.2, ["casa" "house"] 0.5, 
          ["mujer" "NULL"] 0.2, ["el" "house"] 0.5, ["el" "woman"] 0.5, ["casa" "NULL"] 0.2, ["la" "the"] 0.2, ["coche" "the"] 0.2}
         (initial-t-counts ens ess))))

;test fixture
(def itc  (initial-t-counts ens ess))

;2-4 secs
(with-test 
  (defn delta-for-corpus
    "returns a map of deltas for the given corpus"
    [enco esco t-counts]
    (let [itc t-counts]
      (reduce merge {} 
              (for [i (range 1 (inc (count enco)))
                    ^CharSequence j (esco (dec i))]
                (let [kall (cons "NULL" (enco (dec i))) 
                      ksum (apply + (pmap #(itc [j %])  kall))]
                  (reduce merge {} 
                          (for [k kall]
                            (let [numer (itc [j k] 1)]
                              {[i j k] (float (/ numer ksum))}))))))))
  (is (= (< 0.0001 (Math/abs (- 0.5555555
                                (val (first (delta-for-corpus ens ess itc)))))))))

;test fixture
(def dfc (delta-for-corpus ens ess itc))

(with-test
  (defn get-ce-counts 
    "get the counts for single english words"
    [enwords delta]
    (apply (partial merge-with +) (mapv (fn[[[i j k] y]] {k y}) delta)))
  (is (= {"car" 1.111111044883728, "the" 1.3333333432674408, "NULL" 1.3333333432674408, "woman" 1.111111044883728, "house" 1.111111044883728}
         (get-ce-counts #{"car" "the" "NULL" "woman" "house"} dfc))))


(defn get-cef-counts
  "get the counts for c(e,s)"
  [enco esco dfc]
  (apply (partial merge-with +) 
         (flatten (for [s (range (count enco))]
                    (let [ind (inc s)]  
                      (for [i (cons "NULL" (enco s)) j (esco s)] 
                        {[i j] (dfc [ind j i])}))))))
(comment
  (is (= {["car" "coche"] 0.5555555, ["NULL" "el"] 0.4444444477558136, ["car" "la"] 0.5555555, ["the" "coche"] 0.22222222, ["the" "la"] 0.22222222, ["woman" "el"] 0.5555555, ["NULL" "casa"] 0.22222222, ["house" "el"] 0.5555555, ["NULL" "mujer"] 0.22222222, ["house" "casa"] 0.5555555, ["woman" "mujer"] 0.5555555, ["NULL" "coche"] 0.22222222, ["NULL" "la"] 0.22222222, ["the" "el"] 0.4444444477558136, ["the" "casa"] 0.22222222, ["the" "mujer"] 0.22222222}         
         (get-cef-counts ens ess dfc))))

;test fixture
(def cecounts (get-ce-counts #{"car" "the" "NULL" "woman" "house"} dfc))
(def cefcounts (get-cef-counts ens ess dfc))

(defn calc-t
  "get t values by dividing c(e,f)/c(e)"
  [cefcounts cecounts]
   (apply merge (for [[[e s] v] cefcounts]
                 {[s e] (float (/ v (cecounts e)))})))

(comment
  (is (= 
        {["la" "car"] 0.5, ["coche" "car"] 0.5, ["el" "NULL"] 0.33333334, ["mujer" "the"] 0.16666667, ["casa" "the"] 0.16666667, ["el" "the"] 0.33333334, ["la" "NULL"] 0.16666667, ["mujer" "woman"] 0.5, ["coche" "NULL"] 0.16666667, ["casa" "house"] 0.5, ["mujer" "NULL"] 0.16666667, ["el" "house"] 0.5, ["el" "woman"] 0.5, ["casa" "NULL"] 0.16666667, ["la" "the"] 0.16666667, ["coche" "the"] 0.16666667}
(calc-t cefcounts cecounts))))

(defn repiter
  "one iteration of model 1"
  [{:keys [ens1 ess1 itc uniq-eng-words] :as m}]
   (let [dfc (delta-for-corpus ens1 ess1 itc)
              [cecounts cefcounts] (pvalues (get-ce-counts uniq-eng-words dfc) 
                                           (get-cef-counts ens1 ess1 dfc))]
    ;(println (str "completed one iteration "))    
    (assoc m :itc (calc-t cefcounts cecounts))))     

(defn t-values-model1
  "calculate n iterations for model 1"
  [ens ess n]
  (let [itc (initial-t-counts ens ess)
        uniq-eng-words (reduce conj #{"NULL"} (for [i ens j i] j))
        res (:itc (last (take (inc n) (iterate repiter 
                                               {:ens1 ens :ess1 ess :itc itc :uniq-eng-words uniq-eng-words}))))]
    ;(println (str "calculating t-values done"))
    res))
    
(defn get-word-index 
  [ensent1 spsent [^CharSequence spaword ^CharSequence enword] ]
  (do 
    (let [spasent (vec spsent)
          ensent (vec (cons "NULL" ensent1))
          fnx (fn[^CharSequence word sent]
                (first (filter #(.equals word (sent %)) (range (count sent)))))]
      [ (fnx enword ensent) (inc (fnx spaword spasent)) ])))

(defn alignment 
  [encorp escorp t-values]
  (let [fnx (fn[esword enword] 
              (let [enset (conj (set enword) "NULL")
                    sset (set esword)
                    filtmap (filter (fn[[[s e] v]] (and  (enset e) (sset s))) t-values)
                    mfn (fn[w]
                          (apply max-key (fn[[[s e] v]] v) 
                                 (filter (fn[[[s e] v]] (and 
                                                          (.equals s w) 
                                                          (enset e))) filtmap)))]
                ;(println (str "\n en word" (vec  enword)))
                      (pmap #((mfn %) 0) (vec esword))))
        a1 (pmap fnx escorp encorp) 
        fny (fn[ x y z] 
              (pmap (partial get-word-index  x y) z)) 
        res (pmap #(for [i %2] (cons %1 i)) (range 1 (inc (count encorp))) 
                  (pmap fny encorp escorp a1))]
    (println (str "completing alignment " (count res)))
    res))


(defn write-model1-alignment2 [opfile t-vals encorp escorp ]
  (let [ mod1res    
        (try 
          (let [fnx (fn [corp ] (mapv #(vec (.split %1 " ")) (.split corp "\n")))
                dens (fnx (slurp (str dir encorp)))
                dess (fnx (slurp (str dir escorp)))]
            (alignment dens dess t-vals))
          (catch Throwable t(.printStackTrace t)))]
    (println (str " writing output file "))
    (spit (str dir opfile)
          (clojure.string/join (vec (for [i mod1res j i] (str j "\n")))))))

(comment
  (time (write-model1-alignment2 "mytest.key" final-tvals  "test.en" "test.es")))

(defn write-model1-alignment [opfile]
  (let [ mod1res    
        (try 
          (let [n 5
              fnx (fn [corp ] (mapv #(vec (.split %1 " ")) (.split corp "\n")))
              ens (fnx (slurp (str dir "corpus.en")))
              ess (fnx (slurp (str dir "corpus.es")))
              empty-linefn (fn [x] (and (== 1 (count x)) (.equals "" (first x))))
              wanted-lines (remove #(or (empty-linefn (ess %)) (empty-linefn (ens %)))  (range (count ens)))
              ens1 (mapv ens wanted-lines) 
              ess1 (mapv ess wanted-lines)
              t-vals (t-values-model1 ens ess n)
              dens (fnx (slurp (str dir "dev.en")))
              dess (fnx (slurp (str dir "dev.es")))]
          (alignment dens dess t-vals))
      (catch Throwable t(.printStackTrace t)))]
    (println (str " writing output file "))
      (spit (str dir opfile)
            (clojure.string/join (vec (for [i mod1res j i] (str j "\n")))))))
(comment
  (time (write-model1-alignment "mydev.key"))) 








  

