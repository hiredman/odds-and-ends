(require '[clojure.zip :as zip])
(import '(java.util LinkedList Collections))

;;(load-file "/home/hiredman/odds-and-ends/functional.clj")

;;(defn shuffle
;;      "return a shuffled collection of the same type
;;      and constituted from the same elementss as the
;;      collection passed in"
;;      [x]
;;      (let [a (LinkedList. x)]
;;        (Collections/shuffle a)
;;        (cond
;;          (vector? x) (vec a)
;;          (set? x)    (set a)
;;          (seq? x)    (seq a))))

(defn transform
  "ugh, read the source. this started as a very specific function
  and turned into a very generic one."
  [se pred fn adv fin]
  (if (fin se)
    se
    (if (pred se)
      (recur (adv (fn se)) pred fn adv fin)
      (recur (adv se) pred fn adv fin))))

(defn transforml
  "like transformr, but walks the zipper from the left"
  [se pred fn]
  (transform se pred fn zip/next zip/end?))

(defn transformr
  "takes a zipper, a predicate, and a function
  walks the zipper from the current node towards the left
  until the the end. fn is applied to any loc where pred is true"
  [se pred fn]
  (transform se pred fn zip/prev (comp not zip/prev)))

(def #^{:doc "fast forward the loc of a zipper to the lastnode reached by zip/next"}
     fast-fwd
     (comp last
           (partial take-while (comp not zip/end? zip/next))
           (partial iterate zip/next)))

(defn uncurry
      "applied to a function it returns a function that will continue
      partially applying until it is called with no arguments"
      [x]
      (fn uc [& y]
          (if (seq y)
            (uncurry (apply partial x y))
            (x))))

(defn flip
      "takes a function and returns a function that takes
      arguments in the opposite order"
      [x]
      (fn [& y]
          (apply x (reverse y))))

(defn call "(apply x y)" [x & y] (apply x y))

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
                  #(= "·" (let [c (zip/node %)] (when (symbol? c) (name c))))
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

(defn lambda
  [zip]
  (transformr zip
              #(let [n (zip/node %)]
                 (if (symbol? n)
                   (let [lam (first (name n))]
                     (= lam \λ))))
              #(-> % (zip/replace
                       (let [[_ & vs] (name (zip/node %))]
                         (list 'fn (vec (map (comp symbol str) vs)) 
                               (zip/node (zip/next %)))))
                 zip/right zip/remove)))

(defn pl-vector [zip]
  (transforml zip
              (comp vector? zip/node)
              #(-> %  (zip/replace
                        (-> % zip/node seq zip/seq-zip fast-fwd lambda zip/root vec)))))

(defmacro pl
  "replaces a $ b with (a b) walking right to left
  replaces a · b with (comp a b) left to right
  ⌽a with (uncurry a) left to right
  ↕a with (flip a) left to right"
  [& forms]
  `(do ~@(-> forms zip/seq-zip fast-fwd lambda zip/root zip/seq-zip fast-fwd
             dollar-sign-application
             interpunc-comp zip/root  zip/seq-zip
             prefix-uncurry  zip/root zip/seq-zip 
             prefix-flip zip/root zip/seq-zip fast-fwd lambda
             zip/root zip/seq-zip pl-vector zip/root)))

;; (assert (= '(do (inc (inc 1))) (macroexpand-1 '(pl inc $ inc $ 1))))
;; (assert (= '(do ((flip map) (range 3) inc)) (macroexpand-1 '(pl (↕map range $ 3 inc)))))
;; (assert (= '(do ((flip map) (replicate 3 ((flip apply) (vector ((flip map) (range 10) (comp (comp inc inc) inc))) (comp (comp (comp call ((uncurry *) 10)) call) ((uncurry +) -2)) map)) shuffle))
;;             (macroexpand-1 '(pl (↕map (replicate 3 (↕apply vector $ (↕map range $ 10 inc · inc · inc) call · ⌽* $ 10 · call · (⌽+ -2) map)) shuffle)))))
;; (assert (= '((1 2 3) (1 2 3) (1 2 3)) (pl (↕map (replicate 3 (range 3)) call · (⌽map inc)))))
