(require '[clojure.zip :as zip])

(import '(java.util LinkedList Collections))

(defn shuffle [x]
      (let [a (LinkedList. x)]
        (Collections/shuffle a)
        (cond
          (vector? x) (vec a)
          (set? x)    (set a)
          (seq? x)    (seq a))))

(defn transform [se pred fn]
      (loop [se (zip/seq-zip se)]
            (if (zip/end? se)
              (zip/root se)
              (if (pred se)
                (recur (fn se))
                (recur (zip/next se))))))

(defn uncurry [x]
      (fn uc [& y]
          (if (seq y)
            (uncurry (apply partial x y))
            (x))))

(defn flip [x]
      (fn [& y]
          (apply x (reverse y))))

(defn call [x & y] (apply x y))

(defmacro pl [& forms]
  (let [x (transform forms
                     #(= "·" (let [c (zip/node %)] (and (symbol? c) (name c))))
                     #(-> % zip/remove (zip/insert-left 'clojure.core/comp) zip/next))
        x (transform x
                     #(= 9021 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 0)))
                     #(-> % (zip/replace (list 'uncurry (symbol (subs (name (zip/node %)) 1))))))
        x (transform x
                     #(= 8597 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 0)))
                     #(-> % (zip/replace (list 'flip (symbol (subs (name (zip/node %)) 1))))))]
    `(do ~@x)))

;; (pl
;;   (↕map (↕map (replicate 3 (range 3)) (call · (⌽map inc))) shuffle))
