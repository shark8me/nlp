(ns a4.cky2
  (:use clojure.test))
(require '[a4.mle :as mle])
(require '[a4.core :as a4core])
(require '[clojure.data.json :as json])
(require '[criterium.core :as bench])
(set! *warn-on-reflection* true)  

;similar to a4.cky, with hacks for performance improvement.
(def fixt2 
  (let [mydir "D:\\studies\\nlp\\h2.2\\assignment\\" 
        counts (str mydir "parse_train.counts.out")
        trainfile (str mydir "parse_train.dat")
        cmap (mle/get-count-map counts)
        sent1  (vec (.split "When was the bar-code invented ?" " "))
        wf (a4core/get-word-frequencies (a4core/read-train-file trainfile))
        ;memo (init-pi cmap sent1)
        ]
    {:counts counts :cmap cmap :sent1 sent1 :trainfile trainfile :wf wf
     :nonterminals (keys (cmap :nonterminal))}))
(defn once-fixture [f]
  fixt2  
  (f))
(use-fixtures :once once-fixture)

(with-test
  (defn init-pi
    "initial values of pi"
    [cmap words]
    (let [nonterminals (keys (cmap :nonterminal))]
      (apply merge (for [w (range 1 (inc (count words))) x nonterminals] 
                     {[w w x] (mle/qxw cmap x (words (dec w)))}))))
  (is (< (Math/abs (- 0.0046828436 
                      ((init-pi (fixt2 :cmap) (fixt2 :sent1))
                        [5 5 "VERB"]))) 0.001))
  (is (= 0.0 ((init-pi (fixt2 :cmap) (fixt2 :sent1))
             [4 4 "NP+NUM"]))))
  
(defn getfixt [^String sent]
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
        (pi-ij (fixt2 :sent1)))))

(with-test
  (defn pi-ijx
    "returns a list of input arguments to pi"
    [{:keys [nonterminal] :as m} sent]
    (for [i (pi-ij sent) j (keys nonterminal)]
      (conj i j)))
  (is (= [1 6 "QP"]
         (last (pi-ijx (fixt2 :cmap ) (fixt2 :sent1))))))

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
           (pi-ijx-args (fixt2 :cmap) [1 3 "QP"])))
    (is (= '([["QP" "DET" "NUM"] [2 2 "DET"] [3 3 "NUM"]] 
              [["QP" "DET" "NOUN"] [2 2 "DET"] [3 3 "NOUN"]] 
              [["QP" "ADJ" "QP"] [2 2 "ADJ"] [3 3 "QP"]] 
              [["QP" "ADP" "NUM"] [2 2 "ADP"] [3 3 "NUM"]] 
              [["QP" "ADV" "NUM"] [2 2 "ADV"] [3 3 "NUM"]])
           (pi-ijx-args (fixt2 :cmap) [2 3 "QP"])))) 

(with-test
  (defn fill-words
    "replace indexes with words in sentence"
    [sent [i j s :as ijs]]
    [(sent (dec i)) (sent (dec j)) s])
  (is (= ["When" "When" "ADJ"]
         (fill-words (fixt2 :sent1) [1 1 "ADJ"]))))

(declare calc-pi-value)

(with-test
  (defn calc-pi-part 
    "calculate pi for the given q s s1. No argmax involved"
    [{:keys [nonterminal binaryrule memo senten res] :as m} [q s s1 :as qss]]
    (let [fw (fn[x] (fill-words senten x))
          nmemo (fn[ m2 uf]
                  (let [r2 (if ((:memo m2) uf) m2 
                             (calc-pi-value m2 uf))] r2))
          nmemo2 (reduce nmemo m [s s1])
          q1 ((:memo nmemo2) s)
          q2 ((:memo nmemo2) s1)
          cres (* (mle/qxyy m (q 0) (q 1) (q 2)) q1 q2)]
      ;(println (str "cpp " q " s" s " s1 " s1 "res " cres))
      (assoc nmemo2 :res (conj res cres)))))

(with-test
  (defn calc-pi-value
    "calculate pi for the given i j X, which is the argmax of the set of all 
     inputs to calc-pi-part"
    [{:keys [nonterminal binaryrule memo senten bp] :as m} [i j X :as ijx]]
    (let [pi-args (pi-ijx-args m ijx)
          ofw (fill-words senten ijx)
          pargs (partial calc-pi-part m) 
          rres (reduce calc-pi-part (assoc m :res []) pi-args)
          res (if (empty? pi-args) [0 []] 
                (apply 
                  (partial max-key (fn[[x y]] x))
                  (mapv vector (:res rres) pi-args)))
          nbp (assoc (merge bp (:bp rres)) ijx (second res))]      
      ;(println (str "cpv " ijx " bpafter  " (count nbp)))
      (let [nmem (:memo rres)]
        (assoc rres :memo (assoc nmem ijx (first res)) 
               :bp nbp)))))
  
(def imap 
  (let [mydir "D:\\studies\\nlp\\h2.2\\assignment\\" 
        counts (str mydir "parse_train.counts.out")
        cmap (mle/get-count-map counts)
        fx1 {:counts counts :bp {}
                   :nonterminals (keys (cmap :nonterminal)) :cmap cmap} ]
    fx1))

(defn ckyimpl [sent1] 
  (let [memo (init-pi (imap :cmap) sent1)
        fx (merge {:counts (imap :counts) :senten sent1 
                   :memo memo :bp {}
                   :nonterminals (keys ((imap :cmap) :nonterminal))} (imap :cmap))]
    (calc-pi-value fx [1 (count sent1) "SBARQ"])))

(defn bppath
  "reassemble the full tree using back pointers"
  [r3 s2 orig_s ip]
  (let [bp (:bp r3)
        [[x1 x2 x3 :as x] y z] ((:bp r3) ip)                                 
        sfn (fn[[w1 w2 w3 :as w]]
              (if (== w1 w2) [w3 (let [r (s2 (dec w1)) ]
                                  (if (.equals "_RARE_" r) (orig_s (dec w1)) r))] 
                                    (bppath r3 s2 orig_s w)))]
    ;(println (str " -- ip " ip "  x  " x " y " y " z " z ))    
    [x1 (sfn y) (sfn z)]))

(defn txfrm-sent [freq ^String sent]
  (let [s (vec (.split sent " "))]
    (mapv (fn[x] (if (< (get freq x 0) 5) "_RARE_" x)) s)))

(with-test
  (defn cky-r2
    "run cky algorithm for the given sentence"
    [wf ^String sent]
    (let [sv (txfrm-sent wf sent)
          tp [1 (count sv) "SBARQ"]] 
      ;(println (str " sv " (vec sv)))
      (bppath (ckyimpl sv) sv (vec (.split sent " ")) tp )))
  (is (= ["SBARQ" ["WHNP+PRON" "What"] ["SBARQ" ["SQ" ["VERB" "are"] ["NP+NOUN" "geckos"]] ["." "?"]]]
         (cky-r2 (fixt2 :wf) "What are geckos ?")))
  (is (= ["SBARQ" ["WHNP" ["WHADJP" ["ADV" "How"] ["ADJ" "many"]] ["NOUN" "miles"]] ["SBARQ" ["SQ" ["VERB" "is"] ["SQ" ["NP+PRON" "it"] ["VP" ["PP" ["ADP" "from"] ["NP" ["NOUN" "London"] ["NP" ["." ","] ["NP+NOUN" "England"]]]] ["PP" ["PRT" "to"] ["NP" ["NOUN" "Plymouth"] ["NP" ["." ","] ["NP+NOUN" "England"]]]]]]] ["." "?"]]]
         (cky-r2 (fixt2 :wf) "How many miles is it from London , England to Plymouth , England ?")))
  (is (= ["SBARQ" ["WHADVP+ADV" "Where"] ["SBARQ" ["SQ" ["VERB" "is"] 
                                                   ["NP" ["NOUN" "Inoco"] 
                                                    ["NOUN" "based"]]] ["." "?"]]]
         (cky-r2 (fixt2 :wf) "Where is Inoco based ?")))
  (is (= ["SBARQ" ["SQ" ["VERB" "Name"] ["SQ" ["NP" ["DET" "the"] ["NP" ["ADJ" "first"] ["NOUN" "private"]]] ["VP" ["VERB" "citizen"] ["S+VP" ["PRT" "to"] ["VP" ["VERB" "fly"] ["PP" ["ADP" "in"] ["NP+NOUN" "space"]]]]]]] ["." "."]]
         (cky-r2 (fixt2 :wf) "Name the first private citizen to fly in space ."))))

(defn run-cky [inpfile opfile]
  (let [mydir "D:\\studies\\nlp\\h2.2\\assignment\\" ]
    (spit (str mydir opfile)
          (clojure.string/join "\n" 
                               (for [i (.split (slurp (str mydir inpfile)) "\n")]
                                 (do (println (str "q " i))
                                   (json/write-str (cky-r2 (fixt2 :wf) i))))))))
(comment
  (run-cky "parse_dev.dat" "parse_dev.out")
  (run-cky "parse_test.dat" "parse_test.p2.out"))

(comment (criterium.core/bench (cky-r2 (fixt2 :wf) "What are geckos ?") :verbose ))
;(run-tests 'a4.cky2)