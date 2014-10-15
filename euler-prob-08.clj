; euler-prob-08
; author: Ray Wach
; date: 2014-10-14
; info: Clojure function to find the solution for Project Euler problem 8.
;   This is my first time programming Clojure so the function could probably be
;   golfed down and cleaned up considerably, but it gets the job done.

(ns euler)

(defn max-product-n-digits
  "Given an integer n and a string of decimal digits s, return the maximum
  product of n consecutive digits of that string."
  [n s]
  (when (and (> n 0) (re-matches #"\d+$" s))
    (loop [p 0 t s]
      (let [c (apply * (map (comp #(Integer/parseInt %) str) (take n t)))]
        (if (empty? t)
          p
          (recur (if (< p c) c p) (rest t)))))))
