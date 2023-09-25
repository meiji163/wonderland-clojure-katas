(ns alphabet-cipher.coder
  (:require [clojure.string :as string]))

(defn chr-to-id [c]
  (- (int c) (int \a)))

(defn id-to-chr [n]
  (char (+ (int \a) n)))

(defn encode-with [func keyword message]
  (let [msg-len (count message)
        msg-list (map chr-to-id message)
        kw-list (map chr-to-id
                     (take msg-len (cycle keyword)))]
    (->> (map vector msg-list kw-list)
         (map func)
         (map #(mod % 26))
         (map id-to-chr)
         string/join)))

(defn encode [keyword message]
  (encode-with
   (fn [p]
     (let [[a b] p] (+ a b)))
   keyword message))

(defn decode [keyword message]
  (encode-with
   (fn [p]
     (let [[a b] p] (- a b)))
   keyword message))

(defn chunk-n [n s]
  "split string into chunks of length n"
  (re-seq
   (re-pattern (format ".{1,%d}" n))
   s))

(defn cycle-len? [n str]
  "check if string is cyclical with period n"
  (let [cycles (chunk-n n str)
        [last-cycle & rest-cycles] (reverse cycles)]
    (and
     (apply = rest-cycles)
     (string/starts-with? (first rest-cycles) last-cycle)))
  )

(defn detect-cycle-len [str]
  (letfn [(f [s n]
            (cond (cycle-len? n s) n
                  (>= n (count str)) n
                  :else (f s (inc n))))]
    (f str 1)))

(defn decipher [cipher message]
  (let [dec-str (decode message cipher)
        n (detect-cycle-len dec-str)]
    (subs dec-str 0 n)))
