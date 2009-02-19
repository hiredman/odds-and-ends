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
                (recur (zip/next (fn se)))
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
                     #(= "$" (let [c (zip/node %)] (and (symbol? c) (name c))))
                     #(let [n (list (zip/node (zip/left %)) (zip/node (zip/right %)))]
                        (-> % zip/left (zip/insert-left n)
                            zip/remove zip/next zip/remove zip/next zip/remove)))
        x (transform x
                     #(= "·" (let [c (zip/node %)] (and (symbol? c) (name c))))
                     #(let [n (list 'comp (zip/node (zip/left %)) (zip/node (zip/right %)))]
                        (-> % zip/left (zip/insert-left n)
                            zip/remove zip/next zip/remove zip/next zip/remove)))
        x (transform x
                     #(= 9021 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 0)))
                     #(-> % (zip/replace (list 'uncurry (symbol (subs (name (zip/node %)) 1))))))
        x (transform x
                     #(= 8597 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 0)))
                     #(-> % (zip/replace (list 'flip (symbol (subs (name (zip/node %)) 1))))))]
    `(do ~@x)))

;; (pl (↕map (replicate 3 (↕apply (vector (↕map (range 10) inc · inc · inc)) call · ⌽* $ 10 · call · (⌽+ -2) map)) shuffle))
;; (pl (map call · ⌽+ $ 5 range $ 5))
