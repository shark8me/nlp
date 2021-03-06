(ns a4.core
  (:use clojure.test))

(require '[clojure.data.json :as json])
(require '[clojure.zip :as zip])

;nlp class assignment 2 part 1.
;read in the parse trees and replace words which appear
;less than 5 times with _RARE_

;first read the parse trees, get the leaf words,
;determine frequencies of all words in the training set.

;then read the training set again, replace rare words (less than 5 occurrances)
;with _RARE_, write each sentence out using json.
;use the python count_cfg_freq.py to generate counts and submit it.

(def fixt 
    (let [mydir "D:\\studies\\nlp\\h2.2\\assignment\\" 
          smalltrainfile (str mydir "parse_train_small.dat")
          trainfile (str mydir "parse_train.dat")
          ;smallsent (read-train-file smalltrainfile)
          ]
    {:smalltrainfile smalltrainfile
     :trainfile trainfile
     ;:smallsent smallsent
     ;:sent1 (first smallsent)
     }))

(defn once-fixture [f]
  fixt 
  (f))
  
(use-fixtures :once once-fixture)

(with-test
  (defn read-train-file
    "returns a vector of json-parsed strings"
    [inpfile]
    (let[ t2 (slurp inpfile)
         jfn (fn[x] (json/read-str (str (clojure.string/join x))))]
      (map jfn (.split t2 "\n"))))
  (is (= ["S" ["PP" ["ADP" "In"] ["NP" ["DET" "the"] 
                                  ["NP" ["ADJ" "late"] ["NOUN" "1700<s"]]]] 
          ["S" ["NP" ["ADJ" "British"] ["NOUN" "convicts"]] 
           ["S" ["VP" ["VERB" "were"] ["VP" ["VERB" "used"] 
                                       ["S+VP" ["PRT" "to"] ["VP" ["VERB" "populate"] 
                                                             ["WHNP" ["DET" "which"] ["NOUN" "colony"]]]]]] ["." "?"]]]] 
         (fixt :sent1))))
       
(def fixt 
    (let [smallsent (read-train-file (fixt :smalltrainfile))]
    (assoc fixt
           :smallsent smallsent
           :sent1 (first smallsent))))

(defn once-fixture [f]
  fixt 
  (f))
  
(use-fixtures :once once-fixture)

(with-test
  (defn get-words
    "returns a vector of words, which are leaves in the tree"
    [original]
    (loop [loc original
           words []]
      (if (zip/end? loc)
        words
        (recur 
          (zip/next loc)
          (if (and (zip/branch? loc) (= 2 (count (zip/children loc))))
            (conj words (second (zip/children loc)))
            words)))))
  (is (= ["In" "the" "late" "1700<s" "British" "convicts" 
          "were" "used" "to" "populate" "which" "colony" "?"]
         (get-words (zip/vector-zip (fixt :sent1))))))

(with-test 
  (defn get-word-frequencies
    "returns a map where keys are words and values are frequencies"
    [sentences]
    (frequencies 
      (flatten (for [i sentences] (-> i zip/vector-zip get-words)))))
    (is (= 6 
          ((get-word-frequencies (fixt :smallsent)) "What"))))

(with-test
  (defn get-rare-words 
    "returns a set of rare words in the input file"
    [inpfile]
    (set (map (fn [[k v]] k) 
         (filter 
           (fn [[k v]] (< v 5)) 
           (get-word-frequencies (read-train-file inpfile))))))
  (is (= 47 
         (count (get-rare-words (fixt :smalltrainfile))))))

(defn is-leaf? 
  "returns true if node is a leaf node"
  [node]
  (and (not (zip/branch? node)) (nil? (zip/right node)) (= 1 (count (zip/lefts node)))))

(with-test
  (defn change-rare-words
    "returns a zipper where leaf nodes which are in rarewords
     are replaced with _RARE_"
    [original rarewords]
    (loop [loc original]
      (if (zip/end? loc)
        (zip/root loc)
        (recur
          (zip/next 
            (if (is-leaf? loc) 
              (if (rarewords (zip/node loc))
                (do 
                  ;(println (str "rare word " (zip/node loc)))
                  (zip/edit loc (fn[x] (str "_RARE_")))) loc)
              loc))))))
  (is (= ["S" ["PP" ["ADP" "In"] ["NP" ["DET" "the"] 
                                  ["NP" ["ADJ" "_RARE_"] ["NOUN" "_RARE_"]]]] 
          ["S" ["NP" ["ADJ" "British"] ["NOUN" "_RARE_"]] 
           ["S" ["VP" ["VERB" "were"] ["VP" ["VERB" "used"] 
                                       ["S+VP" ["PRT" "to"] 
                                        ["VP" ["VERB" "_RARE_"] 
                                         ["WHNP" ["DET" "which"] ["NOUN" "_RARE_"]]]]]] 
            ["." "?"]]]]
         (let [rarewords (get-rare-words (fixt :trainfile))
               s1 (zip/vector-zip (fixt :sent1))]
           (change-rare-words s1 rarewords)))))

(run-tests 'a4.core) 
;generate the output for part 1 of assgn2
(comment
  (spit "D:\\studies\\nlp\\h2.2\\assignment\\parse_train_output.dat"
        (let [tfile (fixt :trainfile)
              rarewords (get-rare-words tfile)
              sent (read-train-file tfile)]
          (clojure.string/join "\n" 
                               (map #(json/write-str (change-rare-words %1 rarewords))
                                    (map zip/vector-zip sent))))))
