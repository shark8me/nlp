(ns a4.mle
  (:use clojure.test))

;Maximum likelihood estimates for q(X->w) and q(X-> y1 y2)
(defn once-fixture [f]
  (def fixt 
    (let [mydir "D:\\studies\\nlp\\h2.2\\assignment\\" 
          counts (str mydir "parse_train.counts.out")]
    {:counts counts}))
  (f))
  
(use-fixtures :once once-fixture)

(defn add-count
  "returns a map containing counts for nonterminal,unary
   and binary rules"
  [{:keys [nonterminal unaryrule binaryrule] :as m} inpstr]
  (let [sarr (vec (.split inpstr " "))
        k (sarr 1)
        ipar (fn[x] (java.lang.Integer/parseInt x))]
    (cond (.equals k "NONTERMINAL") 
      (assoc m :nonterminal 
             (assoc nonterminal (last sarr) (ipar (first sarr))))
      (.equals k "UNARYRULE") 
      (assoc m :unaryrule
             (assoc unaryrule (str (sarr 2) "=" (sarr 3)) (ipar (sarr 0))))
      (.equals k "BINARYRULE") 
      (assoc m :binaryrule
             (let [x (sarr 2)
                   k (binaryrule x)
                   yz [(sarr 3) (sarr 4)]
                   yzprob (ipar (sarr 0))]
               (assoc binaryrule x
                      (if k
                        (assoc k yz yzprob)                               
                        {yz yzprob})))))))

(with-test
  (defn get-count-map
    "returns a map with symbol counts"
    [inpfile]
    (reduce
      (fn[ m inpstr] (add-count m inpstr))
      {:nonterminal {} :unaryrule {} :binaryrule {}}
      (.split (slurp inpfile) "\n")))    
  (is (= 1
        (((get-count-map (fixt :counts)) :unaryrule)  "NOUN=I")))
  (is (= 63
        ((((get-count-map (fixt :counts)) :binaryrule) "SQ") ["VERB" "VP"]))))

(with-test
  (defn qxw
    "returns the MLE estimate for count(X->w)"
    [{:keys [nonterminal unaryrule ] :as m} X w]
    (float (/ (get unaryrule (str X "=" w) 0) (nonterminal X))))
  (is (< (Math/abs (- 0.7992656 (qxw (get-count-map (fixt :counts)) "WHNP+PRON" "What"))) 0.001))
  (is (< (Math/abs (- 0.7378641 (qxw (get-count-map (fixt :counts)) "NUM" "_RARE_"))) 0.001)))

(with-test
  (defn qxyy
    "returns the MLE estimate for count(X->y1 y2)"
    [{:keys [nonterminal binaryrule ] :as m} X y1 y2]
    (float (/ ((binaryrule X) [y1 y2]) (nonterminal X))))
  (is (< (Math/abs (- 0.015384615 (qxyy (get-count-map (fixt :counts)) "SBAR" "WHPP" "S"))) 0.001))
  (is (< (Math/abs (- 0.07254038 (qxyy (get-count-map (fixt :counts)) "SBARQ" "SQ+VP" "."))) 0.001)))

(run-tests 'a4.mle)