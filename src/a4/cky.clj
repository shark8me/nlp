(ns a4.cky
  (:use clojure.test))
(require '[a4.mle :as mle])
(require '[a4.core :as a4core])

(def fixt 
  (let [mydir "D:\\studies\\nlp\\h2.2\\assignment\\" 
        counts (str mydir "parse_train.counts.out")
        trainfile (str mydir "parse_train.dat")
        cmap (mle/get-count-map counts)
        sent1  (vec (.split "When was the bar-code invented ?" " "))
        wf (a4core/get-word-frequencies (a4core/read-train-file (fixt :trainfile)))
        ;memo (init-pi cmap sent1)
        ]
    {:counts counts :cmap cmap :sent1 sent1 :trainfile trainfile :wf wf
     :nonterminals (keys (cmap :nonterminal))}))

(defn once-fixture [f]
  fixt
  (f))
(use-fixtures :once once-fixture)

(with-test
  (defn init-pi
    "initial values of pi"
    [cmap words]
    (let [nonterminals (keys (cmap :nonterminal))]
      (apply merge (for [w words x nonterminals] 
                     {[w w x] (mle/qxw cmap x w)}))))
  (is (< (Math/abs (- 0.0046828436 
                      ((init-pi (fixt :cmap) (fixt :sent1))
                        ["invented" "invented" "VERB"]))) 0.001))
  (is (= 0.0 ((init-pi (fixt :cmap) (fixt :sent1))
             ["bar-code" "bar-code" "NP+NUM"]))))
 
(def fixt (assoc fixt :memo  (init-pi (fixt :cmap) (fixt :sent1))))
  
(defn getfixt [sent]
  (let [mydir "D:\\studies\\nlp\\h2.2\\assignment\\" 
        counts (str mydir "parse_train.counts.out")
        trainfile (str mydir "parse_train.dat")
        cmap (mle/get-count-map counts)
        sent1  (vec (.split sent " "))
        wf (a4core/get-word-frequencies (a4core/read-train-file trainfile))
        memo (init-pi cmap sent1)]        
    (merge cmap {:counts counts :senten sent1 :trainfile trainfile :wf wf
                 :nonterminals (keys (cmap :nonterminal))
                 :memo memo})))


(with-test
  (defn pi-ij
    "returns a list of i,j required to calculate pi for a sentence"
    [words]
    (let [n (count words) ]
      (for [l (range 1 n)
            i (range 1 (inc (- n l)))]
        (let [j (+ i l)]
          [i j]))))
  (is (= '([1 2] [2 3] [3 4] [4 5] [5 6] [1 3] [2 4] 
                 [3 5] [4 6] [1 4] [2 5] [3 6] [1 5] [2 6] [1 6])
        (pi-ij (fixt :sent1)))))

(with-test
  (defn pi-ijx
    "returns a list of input arguments to pi"
    [{:keys [nonterminal] :as m} sent]
    (for [i (pi-ij sent) j (keys nonterminal)]
      (conj i j)))
  (is (= [1 6 "QP"]
         (last (pi-ijx (fixt :cmap ) (fixt :sent1))))))

(pi-ijx (fixt :cmap ) "What are geckos ?")
(with-test
  (defn pi-ijx-args 
    "returns a list of arguments for [q, pi, pi] 
     to pass to single invocation to pi(i,j,X)"
    [{:keys [nonterminal binaryrule] :as m} [i j X]]
    (let [yzs (vec (keys (binaryrule X)))]
      (for [[y z] yzs s (range i j)] 
        [[ X y z] [i s y] [(inc s) j z]])))
    (is (= '([["QP" "DET" "NUM"] [1 1 "DET"] [2 3 "NUM"]] 
              [["QP" "DET" "NUM"] [1 2 "DET"] [3 3 "NUM"]] 
              [["QP" "DET" "NOUN"] [1 1 "DET"] [2 3 "NOUN"]] 
              [["QP" "DET" "NOUN"] [1 2 "DET"] [3 3 "NOUN"]] 
              [["QP" "ADJ" "QP"] [1 1 "ADJ"] [2 3 "QP"]] 
              [["QP" "ADJ" "QP"] [1 2 "ADJ"] [3 3 "QP"]] 
              [["QP" "ADP" "NUM"] [1 1 "ADP"] [2 3 "NUM"]] 
              [["QP" "ADP" "NUM"] [1 2 "ADP"] [3 3 "NUM"]] 
              [["QP" "ADV" "NUM"] [1 1 "ADV"] [2 3 "NUM"]] 
              [["QP" "ADV" "NUM"] [1 2 "ADV"] [3 3 "NUM"]])
           (pi-ijx-args (fixt :cmap) [1 3 "QP"])))
    (is (= '([["QP" "DET" "NUM"] [2 2 "DET"] [3 3 "NUM"]] 
              [["QP" "DET" "NOUN"] [2 2 "DET"] [3 3 "NOUN"]] 
              [["QP" "ADJ" "QP"] [2 2 "ADJ"] [3 3 "QP"]] 
              [["QP" "ADP" "NUM"] [2 2 "ADP"] [3 3 "NUM"]] 
              [["QP" "ADV" "NUM"] [2 2 "ADV"] [3 3 "NUM"]])
           (pi-ijx-args (fixt :cmap) [2 3 "QP"])))) 

(pi-ijx-args inoco  [1,5,"SBARQ"])

(with-test
  (defn fill-words
    "replace indexes with words in sentence"
    [sent [i j s :as ijs]]
    [(sent (dec i)) (sent (dec j)) s])
  (is (= ["When" "When" "ADJ"]
         (fill-words (fixt :sent1) [1 1 "ADJ"]))))

(with-test
  (defn calc-pi-part 
    ""
    [{:keys [nonterminal binaryrule memo senten res] :as m} [q s s1 :as qss]]
    (do ;(println (str "cpp " q " s" s " s1 " s1 " m " (count memo)))
      (let [fw (fn[x] (fill-words senten x))
            nmemo (fn[ m2 uf]
                    (let [r2 (if ((:memo m2) (fw uf)) m2 
                               (calc-pi-value m2 uf))]
                      ;(println (str "cpp " (count (:bp r2))))
                      r2))
            nmemo2 (reduce nmemo m [s s1])
            q1 ((:memo nmemo2) (fw s))
            q2 ((:memo nmemo2) (fw s1))
          cres (* (mle/qxyy m (q 0) (q 1) (q 2)) q1 q2)]
      ;(println (str "cpp " q " s" s " s1 " s1 "res " cres))
      (assoc nmemo2 :res (conj res cres)))))
  (is (= (> 0.01 (Math/abs (- 0.00461538
         (try
           (:res (calc-pi-part (assoc (fixt :cmap) :senten (fixt :sent1) :memo 
                                (assoc (fixt :memo) ["When" "When" "PP"] 0.5
                                       ["was" "was" "VERB"] 0.6))
                         [["SBAR" "WHPP" "S"] [1 1 "PP"] [2 2 "VERB"]]))
           (catch Throwable t(.printStackTrace t)))))))))

(with-test
  (defn calc-pi-value
    ""
    [{:keys [nonterminal binaryrule memo senten bp] :as m} [i j X :as ijx]]
    (do ;(println (str "cpv start " ijx " bp " (count bp)))
    (let [pi-args (pi-ijx-args m ijx)
          ofw (fill-words senten ijx)
          pargs (partial calc-pi-part m) 
          rres (reduce calc-pi-part (assoc m :res []) pi-args)
          res (if (empty? pi-args) [0 []] 
                (apply 
                  (partial max-key (fn[[x y]] x))
                  (mapv vector (:res rres) pi-args)))
          nbp (assoc (merge bp (:bp rres)) ofw (second res))]      
      ;(println (str "cpv " ijx " bpafter  " (count nbp)))
      (let [nmem (:memo rres)]
        (assoc rres :memo (assoc nmem ofw (first res)) 
               :bp nbp)))))
  (is (= 0.0
         (try
           (calc-pi-value (assoc (fixt :cmap) :senten (fixt :sent1) :memo (fixt :memo))
                          [1 3 "QP"])
           (catch Throwable t(.printStackTrace t)))))
  (is (= 0.0
         ((:memo 
         (try
           (calc-pi-value (assoc (fixt :cmap) :senten  "What are geckos ?" 
                                 :memo (init-pi (fixt :cmap) "What are geckos ?"))
                          [1 4 "SBARQ"])
           (catch Throwable t(.printStackTrace t)))) ["What" "?" "SBARQ"]))))
  
(defn ckyimpl [sent1] 
  (let [mydir "D:\\studies\\nlp\\h2.2\\assignment\\" 
        counts (str mydir "parse_train.counts.out")
        cmap (mle/get-count-map counts)
        memo (init-pi cmap sent1)
        fx (merge {:counts counts :senten sent1 
                   :memo memo :bp {}
                   :nonterminals (keys (cmap :nonterminal))} cmap)]
    (calc-pi-value fx [1 (count sent1) "SBARQ"])))

(comment 
(def c1 (ckyimpl ["Where" "is" "_RARE_" "_RARE_" "?"]))
((:memo c1) ["_RARE_" "_RARE_" "VERB"])
((:memo c1) ["Where" "Where" "WHADVP+ADV"])

((:memo c1) ["?" "?" "NP+PRON"])

((:memo c1) ["What" "_RARE_" "WHADVP+ADV"]))
  
(defn bppath [r3 s2 ip]
  (try
  (let [bp (:bp r3)
        [[x1 x2 x3 :as x] y z](bp ip)                                 
        sfn (fn[[w1 w2 w3 :as w]]
              (if (= w1 w2) [w3 (s2 (dec w1))] 
                                    (bppath r3 s2 (fill-words s2 w))))]
    ;(println (str " -- x  " x " y " y " z " z ))    
    [x1 (sfn y) (sfn z)])
  (catch Throwable t (.printStackTrace t))))

(defn txfrm-sent [freq sent]
  (let [s (vec (.split sent " "))]
    (mapv (fn[x] (if (< (get freq x 0) 5) "_RARE_" x)) s)))

(with-test
  (defn cky-r2 [sv]
    (let [tp [(sv 0) (last sv) "SBARQ"]] 
      (bppath (ckyimpl sv) sv tp)))
  (is (= ["SBARQ" ["WHNP+PRON" "What"] 
          ["SBARQ" ["SQ" ["VERB" "are"] 
                    ["NP+NOUN" "_RARE_"]] ["." "?"]]]
         (cky-r2 (txfrm-sent (fixt :wf) "What are geckos ?"))))
  (is (= []
         (cky-r2 (txfrm-sent (fixt :wf) 
                             "How many miles is it from London , England to Plymouth , England ?"))))
  (is (= []
         (cky-r2 (txfrm-sent (fixt :wf) 
                             "Where is Inoco based ?"))))
  (is (= []
         (cky-r2 (txfrm-sent (fixt :wf) 
                             "Name the first private citizen to fly in space .")))))

;(def c1 (ckyimpl (txfrm-sent (fixt :wf) "Where is Inoco based ?")))

;(txfrm-sent wf "How many miles is it from London , England to Plymouth , England ?")