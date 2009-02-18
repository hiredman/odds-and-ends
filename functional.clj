(require '[clojure.zip :as zip])

(defn transform [se pred fn]
      (loop [se (zip/seq-zip se)]
            (if (zip/end? se)
              (zip/root se)
              (if (pred se)
                (recur (fn se))
                (recur (zip/next se))))))

(defmacro pl [& forms]
  (let [x (transform forms
                     #(= "·" (let [c (zip/node %)] (and (symbol? c) (name c))))
                     #(-> % zip/remove (zip/insert-left 'clojure.core/comp)))
        x (transform x
                     #(= 9021 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 0)))
                     #(-> % (zip/replace (list 'uncurry (symbol (subs (name (zip/node %)) 1))))))]
    `(do ~@x)))
