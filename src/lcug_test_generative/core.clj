(ns lcug-test-generative.core
  (:use clojure.test.generative)
  (:require [ clojure.test.generative.generators :as gen]))


;; test.generative is a library which allows us to write generative
;; tests in clojure

;; Similar to Haskell's quickcheck

;; Generative testing: describe roughly what your input data should
;; look like, and what properties should hold
;; Let the library take care of the rest


;; HELLO WORLD example
;; (from the documentation)

(defspec integers-closed-over-addition
  (fn [a b] (+' a b))                    ;; input fn
  [^long a ^long b]                     ;; input spec
      (assert (integer? %)))                ;; 0 or more validator forms

;; defspec defines a function which executes the input function and
;; then calls the assertion on the result
;; Metadata defines the generators to use for inputs

;; run the test:

; (test-vars #'integers-closed-over-addition)


;; This runs for 10 seconds across all of my cores unless I tell it
;; otherwise

; (binding [*cores* 1 *msec* 5000] (test-vars  #'integers-closed-over-addition))


;; test.generative is trying to break your stuff! It will run until it
;; finds a failing example

(defspec integer-addition-is-always-positive
  (fn [a b] (+' a b))
  [^long a ^long b]
  (assert (>= % 0)))


;; =============================================================== 
;; You have a lot of flexibility for defining generators

;; the generators namespace has generators for all standard data types
;; (gen/int)
;; (gen/keyword)
;; (gen/string)

;; also can define specifications for collections with content/size specifications

;; (gen/vec gen/int 5)

;; Different distributions: uniform, geometric (mean 1/p)


;; yahtzee game scorer

;; What applicable combinations are available for scoring

(defn get-available-scores [dice]
  {
   :yahtzee-score (if  (apply = dice) (apply + dice))})


(defspec yahtzee-score-available-when-all-dice-are-the-same
  (fn [dice-value] (get-available-scores (repeat 5 dice-value)))
  [^int dice-value]
  (assert (= (* 5 dice-value) (:yahtzee-score %))))

(defspec yahtzee-score-not-available-when-dice-are-not-the-same
  get-available-scores
  [^{:tag (vec int 5)} dice ]
  (assert (= nil (:yahtzee-score %))))

;  (test-vars #'yahtzee-score-available-when-all-dice-are-the-same  #'yahtzee-score-not-available-when-dice-are-not-the-same)

;; dice-value in both cases can be set to a very high
;; value. We should think about constraining it. How about some custom
;; generators. These are just functions.

(defn dice-throw []
  (gen/one-of 1 2 3 4 5 6))

(defn yahtzee-throw []
  (gen/vec (dice-throw) 5))

(defn non-yahtzee-throw []
  (let [first-value (dice-throw)
        subsequent (take 4 (remove #{first-value} (repeatedly dice-throw)))
        ]
    (cons first-value subsequent)))
;; (yahtzee-throw)
;; (non-yahtzee-throw)

(defspec yahtzee-score-available-when-all-dice-are-the-same-2
  get-available-scores
  [^{:tag `yahtzee-throw} dice]
  (assert (= (apply + dice) (:yahtzee-score %))))

(defspec yahtzee-score-not-available-when-dice-are-not-the-same-2
  get-available-scores
  [^{:tag `non-yahtzee-throw} dice]
  (assert (= nil (:yahtzee-score %))))
;  (test-vars #'yahtzee-score-available-when-all-dice-are-the-same-2  #'yahtzee-score-not-available-when-dice-are-not-the-same-2)

;; Running these - now we dont get to 10 seconds as has exhausted the
;; list of available values

;; if we want to see what it is doing:

;; (binding [*verbose* true] (test-vars #'yahtzee-score-available-when-all-dice-are-the-same-2  #'yahtzee-score-not-available-when-dice-are-not-the-same-2))


;; ==============================================================

;; Sweet spots for test.generative

;; gets you thinking about the limit of your solution 

;; Part of a merge sort implementation with traditional recursion

(defn naive-merge-left-right [left right]
  (cond
   (and (not (seq left)) (not (seq right))) []
   (not (seq left))  right
   (not (seq right)) left
   :else (let [[lh & lr] left [rh & rr] right]
           (if (< lh rh)
             (cons lh (naive-merge-left-right lr right))
             (cons rh (naive-merge-left-right left rr))))))

;; this works ok for any sort of readable test data I want to give it:

; (naive-merge-left-right [1 2 3] [2 3 4])

;; Defining it in terms of invariants

(defspec merge-result-is-ordered
  (fn [n]
    (let [[left right ] (split-at (quot n 2) (range n))]
      (naive-merge-left-right left right)))
  [^int n]
  (assert (apply <= %)))

;; Boom! Out of heap space!

(defspec merge-result-is-ordered-with-limits
  (fn [n]
    (let [[left right] (split-at (quot n 2) (range n))]
      (naive-merge-left-right left right)))
  [^{:tag (gen/uniform 0 200000)} n]
  (assert (apply <= %)))
;; Different Boom!

;; rewriting with loop/recur
(defn merge-left-right [left right]
  (loop [res [] left left right right]
    (cond
     (and (not (seq left)) (not (seq right))) res
     (not (seq left))  (concat res right)
     (not (seq right)) (concat res left)
     :else (let [[lh & lr] left [rh & rr] right]
             (if (< lh rh)
               (recur (conj res lh) lr right)
               (recur (conj res rh) left rr))))))


(defspec merge-result-is-ordered-with-limits-improved
  (fn [n]
    (let [[left right] (split-at (quot n 2) (range n))]
      (merge-left-right left right)))
  [^{:tag (gen/uniform 0 200000)} n]
  (assert (apply <= %)))

;; Uniform distribution gives us an equal chance of returning any
;; number between 0 and 200000. No guarentee that we will hit 200,000
;; ... if we were concerned about upper limit performance, perhaps we
;; could choose a different distribution or just make msec very high
;; and run it over night!


;; Advtanges

;; Separation of test input creation and test specification - avoid test duplication

;; Change thinking: from 1 + 2 = 3 style tests to more general
;; overarching rules

;; Choosing the right input generation fns could help find obscure
;; bugs, system constraints


;; Drawbacks
;; Existing framework still a WIP
;; Might be overkill for certain types of problem
;; Takes a long time to run by default (10 seconds)
;; Laptop melting


;; FURTHER RESOURCES

;; https://github.com/clojure/test.generative - good source comments

;; https://github.com/abedra/the-generative-generation  - Aaron
;; Bedra's "Generative Generation" Presentation

;; http://blog.fogus.me/2012/03/23/lein-generative/ Lein plugin (I
;; have not tried this)

