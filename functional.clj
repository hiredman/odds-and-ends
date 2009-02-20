(require '[clojure.zip :as zip])

(import '(java.util LinkedList Collections))

(defn shuffle [x]
      (let [a (LinkedList. x)]
        (Collections/shuffle a)
        (cond
          (vector? x) (vec a)
          (set? x)    (set a)
          (seq? x)    (seq a))))

(defn transform [se pred fn adv fin]
      (loop [se se]
            (if (fin se)
              se
              (if (pred se)
                (recur (adv (fn se)))
                (recur (adv se))))))

(defn transforml [se pred fn]
      (transform se pred fn zip/next zip/end?))

(defn transformr [se pred fn]
      (transform se pred fn zip/prev (comp not zip/prev)))

(defn fast-fwd [se]
      (if (zip/end? (zip/next se))
        se
        (recur (zip/next se))))

(defn uncurry [x]
      (fn uc [& y]
          (if (seq y)
            (uncurry (apply partial x y))
            (x))))

(defn flip [x]
      (fn [& y]
          (apply x (reverse y))))

(defn call [x & y] (apply x y))

(defn dollar-sign-application
      "walks backwards through a zipper turning a $ b into (a b)"
      [zip]
      (transformr zip
                  #(= "$" (and (symbol? (zip/node %)) (name (zip/node %))))
                  #(let [n (list (zip/node (zip/left %)) (zip/node (zip/right %)))]
                     (-> % zip/remove (zip/insert-left n) zip/right zip/remove zip/remove))))

(defn interpunc-comp
      "walks through a zipper tuning a · b into (comp a b)"
      [zip]
      (transforml zip
                  #(= "·" (let [c (zip/node %)] (and (symbol? c) (name c))))
                  #(let [n (list 'comp (zip/node (zip/left %)) (zip/node (zip/right %)))]
                     (-> % zip/left (zip/insert-left n)
                         zip/remove zip/next zip/remove zip/next zip/remove))))
(defn prefix-uncurry
      "walks through a zipper and turns ⌽a into (uncurry a)"
      [zip]
      (transforml zip
                  #(= 9021 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 0)))
                  #(-> % (zip/replace (list 'uncurry (symbol (subs (name (zip/node %)) 1)))))))

(defn prefix-flip
      "walks through a zipper and turns ↕a into (flip a)"
      [zip]
      (transforml zip
                  #(= 8597 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 0)))
                  #(-> % (zip/replace (list 'flip (symbol (subs (name (zip/node %)) 1)))))))

(defmacro pl [& forms]
  `(do ~@(-> forms zip/seq-zip fast-fwd dollar-sign-application
             interpunc-comp zip/root zip/seq-zip
             prefix-uncurry zip/root zip/seq-zip
             prefix-flip zip/root)))

(pl
  (↕map (replicate 3 (↕apply vector $ (↕map range $ 10 inc · inc · inc) call · ⌽* $ 10 · call · (⌽+ -2) map)) shuffle))
 
;; (pl
;;   (↕map (↕map (replicate 3 (range 3)) call · (⌽map inc)) shuffle))
