(ns q1
  (:use clojure.test))

(defn read-train-file [inp delim]
  (.split (slurp inp) delim))

(defn  get-word-counts [inpfile delim] 
  "returns a map, key is word, value is number of times word appeared in inpfile."
  (let [ifile (read-train-file inpfile delim)] 
  (reduce (partial merge-with +) (map #(hash-map (first (.split % " ")) 1) ifile))))

(deftest basic-wc
  (let [word-counts (get-word-counts "D:\\studies\\nlp\\h1-p\\gene.train" "\n")]
    (= 6  (word-counts "morbidity"))))

(def word-counts (get-word-counts "D:\\studies\\nlp\\h1-p\\gene.train" "\n"))
(filter (fn[[x y]] (< y 5)) word-counts)

(with-test
  (defn getclass [strin]
    (try
    (cond 
      (java.util.regex.Pattern/matches  "[a-z]*" strin) :lowercase
      (java.util.regex.Pattern/matches  "[A-Z][a-z]*" strin) :capitalized
      (java.util.regex.Pattern/matches  "[a-z]*[A-Z]" strin) :lastcapital
      (java.util.regex.Pattern/matches  "[A-Z]*" strin) :allcaps
      (java.util.regex.Pattern/matches  "[a-zA-Z0-9]*" strin) :alnum
      (java.util.regex.Pattern/matches  "[^\\w]*" strin) :non-word
      (java.util.regex.Pattern/matches  ".*" strin) :rare)
    (catch Throwable t (str " exception for " strin " message " (.getMessage t)))))
  (is (= :lowercase (getclass "abcd")))
  (is (= :capitalized (getclass "Abcd")))
  (is (= :lastcapital (getclass "abcD")))
  (is (= :lastcapital (getclass "aD")))
  (is (= :allcaps (getclass "ABCD")))
  (is (= :alnum (getclass "ab9Z")))
  (is (= :non-word (getclass "(-")))
  (is (= :non-word (getclass "fdsdf")))
  (is (= :rare (getclass "PI3K_68D"))))


;replace the test file with _RARE_
;(word-counts "became")

(defn replace-with-rare-fn [rarewords inp ]
  "replace the first word with _RARE_ if found in the rarewords set"
  (let [fp2 (.split inp " ")
        fp1 (first fp2)] 
    ;(println (str "inp " inp (if (rarewords fp1) "rare" "norm")))
  (if (rarewords fp1) (str "_RARE_ " (clojure.string/join  (rest fp2))) inp))) 

(defn replace-with-rare-fn-part2 [rarewords inp ]
  "replace the first word with _RARE_ if found in the rarewords set" 
    (try
  (let [fp2 (.split inp " ")
        fp1 (first fp2)] 
  (if (rarewords fp1)  
    (str "_" (name (getclass fp1)) "_ " (clojure.string/join  (rest fp2))) inp))
  (catch Throwable t (println (str "input string" inp " :" (.getMessage t))))))

(defn replace-with-rare [inpfile delim opfile replace-fn wordcounts ]
  (let [;gene-train (read-train-file inpfile delim)
        rarewords (set (map first (filter #(< (second %) 5) wordcounts)))
        cfn (partial replace-fn rarewords)]
    (spit opfile 
          (with-open [rdr (clojure.java.io/reader inpfile)]
            (clojure.string/join "\n" (map cfn (line-seq rdr)))))
    (flush)))
;replace the training file modified with _RARE_ for words occurring less 
;than 5 times.
(replace-with-rare "D:\\studies\\nlp\\h1-p\\gene.train" "\n" 
                   "D:\\studies\\nlp\\h1-p\\gene.train.mod1" 
                   replace-with-rare-fn
                   word-counts)

(replace-with-rare "D:\\studies\\nlp\\h1-p\\gene.train" "\n" 
                   "D:\\studies\\nlp\\h1-p\\gene.train.mod2" 
                   replace-with-rare-fn-part2
                   word-counts)

;now run the python program to count, using gene.train.mod1
;python count_freqs.py gene.train.mod1 > gene.counts.mod1
;python count_freqs.py gene.train.mod2 > gene.counts.mod2

(defn linesplit [line ]
  (let [all (. line split " ")
        ttype (nth all 1)]
    (cond 
      (.equalsIgnoreCase ttype "WORDTAG")
      {:count (Integer/parseInt (nth all 0)) :type (keyword ttype) :tag (nth all 2) :word (last all)}
      (.equalsIgnoreCase ttype "1-GRAM")
      {:count (Integer/parseInt (nth all 0)) :type (keyword ttype) :tag (keyword (nth all 2))})
    ))

;(def gcr (read-train-file "D:\\studies\\nlp\\h1-p\\gene.counts.mod1" "\n"))

;(def testdata '({"family" {:igene 120}} {"Cdc42" {:igene 11}} {"Cdc42" {:o 11}}))

(defn makewordmap [gcr]
  (let [;gcr (read-train-file inpfile "\n")
        ;(read-train-file "D:\\studies\\nlp\\h1-p\\gene.counts" "\n")
        imap (map (fn [{:keys [count tag word] :as m}]                  
                    {word {(keyword tag) count}}) 
                  (filter  #(= (:type %) :WORDTAG)  (map linesplit gcr)))
        assocfn (fn [oldv newv] (merge oldv newv ))]
    (reduce (partial merge-with assocfn) imap)))

;(def gcr (read-train-file "D:\\studies\\nlp\\h1-p\\gene.counts.mod1" "\n"))
;(def wordmap (makewordmap gcr))
;(wordmap "ill")

(defn get-1gram-map [gcr]
  (let [listofmaps (filter  #(= (:type %) :1-GRAM)  (map linesplit gcr))] 
    (zipmap (map :tag listofmaps) (map :count listofmaps ))))

;(def onegram-map (get-1gram-map gcr))

(defn lookup [{:keys [word wordmap onegram] :as m}]
  "returns POS tag for given word"
  (let [w1 (wordmap word)
        rare (wordmap "_RARE_")
        tfn (fn [kw ] (get w1 kw 0.0000001))
        igc (float (/ (tfn :I-GENE) (onegram :I-GENE)))
        oc (float (/ (tfn :O) (onegram :O)))]
    ;(println (str w1 " " igc " " oc))
    (if (> igc oc) (name :I-GENE) (name :O))))

(defn generate-onegram-mapped-output [genecounts inpfile opfile]
  (let [gcr (read-train-file genecounts "\n")
        wordmap (makewordmap gcr)
        onegram-map (get-1gram-map gcr)   
        gd (read-train-file inpfile "\n")]
    (spit opfile  
          (str (clojure.string/join "\n" 
                                    (let [kmap {:wordmap wordmap :onegram onegram-map}]
                                      (map #(str % " "
                                                 (if (empty? %) "" (lookup (assoc kmap :word %)))) gd))) 
               "\n\n"))))

(comment 
(generate-onegram-mapped-output "D:\\studies\\nlp\\h1-p\\gene.counts.mod1"
                                "D:\\studies\\nlp\\h1-p\\gene.dev" 
                                "D:\\studies\\nlp\\h1-p\\gene_dev.p1.out"))

;got the right answer!
;$ python.exe eval_gene_tagger.py gene.key gene_dev.p1.out
;Found 2669 GENEs. Expected 642 GENEs; Correct: 424.
;
;         precision      recall          F1-Score
;GENE:    0.158861       0.660436        0.256116
;generate the test output
(comment
(generate-onegram-mapped-output "D:\\studies\\nlp\\h1-p\\gene.counts.mod1"
                                "D:\\studies\\nlp\\h1-p\\gene.test" 
                                "D:\\studies\\nlp\\h1-p\\gene_test.p1.out"))
