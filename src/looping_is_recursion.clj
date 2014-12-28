(ns looping-is-recursion)

(defn power [base exp]
  (if (zero? exp)
    1
    (let [helper (fn [result i]
                   (if (< i 2)
                    result
                    (recur (* result base) (dec i))))]
      (helper base exp))))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (== 1 (count a-seq)) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [i 0]
    (cond
      (== (count a-seq) i) nil
      (pred (get a-seq i)) i
      :else (recur (inc i)))))

(defn avg [a-seq]
  (loop [sum 0
         i 0]
    (if (== (count a-seq) i)
      (/ sum (count a-seq))
      (recur (+ sum (get a-seq i)) (inc i)))))

(defn parity [a-seq]
  (loop [freq (frequencies a-seq)
         odds #{}]
    (if (empty? freq)
      odds
      (recur (rest freq) (if (integer? (/ (second (first freq)) 2))
                                 odds
                                 (conj odds (first (first freq))))))))

(defn fast-fibo [n]
  (if (< n 2)
    n
    (loop [n-2 0
           n-1 1
           n n]
      (if (== n 2)
        (+ n-1 n-2)
        (recur n-1 (+ n-1 n-2) (dec n))))))

(defn cut-at-repetition [a-seq]
  (loop [seq-first (first a-seq)
         i 1]
      (cond
        (== i (count a-seq)) a-seq
        (= seq-first (get a-seq i)) (take i a-seq)
        :else (recur seq-first (inc i)))))

