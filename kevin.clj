(in-ns 'kevin)

(clojure.core/refer 'clojure.core)


(defn java-file-reader [x]
      (new java.io.FileReader x))

(defn java-file-writer [x]
      (new java.io.FileWriter x))

;(defmacro close "close something" [f] `(. ~f (close)))
;(with-open f (java-file-writer "branches/code/test") (. f (write "bob")))

(defn lazy-stream
      "takes a java object, returns a lazy seq of whatever read() returns cast as a char"
      [s]
      (let [curr-char (. s (read))]
        (lazy-cons (char curr-char)
                    (when (not (= curr-char -1))
                      (lazy-stream s)))))

(defn file-lazy-read "returns a lazy steam of characters from a file" [x]
			(lazy-stream
				(new java.io.BufferedReader
						 (java-file-reader x))))


(defn make-line
			"returns a string from a sequence of characters, from the first char to the first \newline"
			[x]
			(if (not (nil? x))
			(let [v (first x) b (rest x)]
				(if (= v \newline)
					""
					(let [h (make-line b)]
						(if (nil? h) h (str v h)))))))



(defn file-lazy-strings
			"takes a seq of characters, returns lazy seq of strings, newline as seperater"
			[x]
			(if (nil? x)
				x
			(let [a (make-line x)]
				(lazy-cons a (file-lazy-strings (drop (inc (count a)) x))))))

(defn agent-generator [x]
      (lazy-cons (agent x) (agent-generator x)))

(def agents (agent-generator {}))

;(map test (map last (filter #((meta (last %)) :arglists) (ns-interns 'ui))))

(defn input-seq [x]
      (lazy-cons (.readLine (java.io.BufferedReader. (java.io.InputStreamReader. x)))
                 (lazy-input x)))


(defn test-interns [nspace]
			(map test
				 (filter #(% :arglists)
									 (map last
												(ns-interns nspace)))))

(def test-interns #(map #([(last %) (test (last %))]) (ns-interns %)))

(defn test-interns [nspace]
			(map #(list (last %) (test (last %))) (ns-interns nspace)))


(defn K [x] (fn [y] x ))
(defn S [x] (fn [y] (fn [z] ((x z) (y z)))))
(defn I [x] (((S K) K) x))
(defn M [x] (((S I) I) x))

(defmacro λ [x & l]
	`(fn ~x ~@l))

(defmacro |> [& x]
          (loop [q (rest x) w (first x)]
                (if q
                  (recur (rest q) (list (first q) w))
                  w)))

(def ♥ :heart)

(defn c [& x]
      (reduce #(% %2) x))


(defmacro combinate [& x]
  (loop [i x]
        (if (> (count i) 2)
          (recur (conj (drop 2 i) (list (first i) (second i))))
          i)))

(defmacro time-meta
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
				 tim# (/ (- (. System (nanoTime)) start#))]
     (with-meta [ret#] {:time tim#})))

(defn c [x]
      (when (or (number? x) x)
        (fn [& y]
            (if (not y) (c (inc x)) x))))

(defn a [x & [w & i]]
			(fn [& [y & u]]
					(if (number? y)
						(a (+ x y) (w))
						(/ x (w :s)))))

(defn arith-mean [f n]
      (let [a (fn a [x & [w & i]]
                  (fn [& [y & u]]
                      (if (number? y)
                        (a (+ x y) (inc w))
                        (/ x w ))))]
        (if (number? f)
          ((a f 1) n)
          (f n))))

;http://paste.lisp.org/display/70623

(defmulti fib (fn [x] x) :default)
(defmethod fib 0 [_] 0)
(defmethod fib 1 [_] 1)
(defmethod fib :default [n] (+ (fib (- n 2)) (fib (- n 1))))


(defn j-add [fr & rt]
      (reduce #(do (.add %1 %2) %1) (concat [fr] rt)))


(defn form-reader [charsource]
      (loop [[c & r] (seq charsource) open-parens-count 0 out []]
            (cond 
              (= \( c)
                (recur r (inc open-parens-count) (conj out c))
              (= \) c)
                (recur r (dec open-parens-count) (conj out c))
              (and (not= 0 (count out)) (= open-parens-count 0))
                (apply str out)
              :else
                (recur r open-parens-count (conj out c)))))


(defn form-at-n [n file]
      (let [pho (fn pho [file action]
                    (try
                      (let [chunk (action file)]
                        (if chunk
                          (lazy-cons chunk (pho file action))
                          (do (.close file) nil)))
                      (catch java.io.IOException e nil)))
            rducer (fn [x y]
                       (if (not (string? x))
                         (if (= n (.getLineNumber x))
                           (let [s (form-reader (pho x #(char (.read %))))]
                             (.close x)
                             s)
                           x)
                         x))]
        (reduce rducer
                (let [f (-> file
                            java.io.File.
                            java.io.FileReader.
                            java.io.LineNumberReader.)]
                  (cons f (pho f #(.readLine %)))))))

"tag:thelastcitadel.com,1984-09-25:KevinDowney"

(defn pho
      "this was originally a lazy seq over a file, now it is a lazy seq over a
      thunk, in desperate need of refactoring"
      [file action]
      (try
        (let [chunk (action)]
          (if chunk
            (lazy-cons chunk (pho file action))
            (do (.close file) nil)))
        (catch java.io.IOException e nil)))

(rread {:close (> (.getLineNumber f) 4)   ;close predicate
        :stream [f (-> "foo.txt"
                       java.io.File.
                       java.io.FileReader.
                       java.io.LineNumberReader.)]
        :chunk [c #(.readLine f)]}        ;thunk that gives us chunks
                                          ; of the file
       (fn [a b]
           ;two thunks at a time, so when we
           ;get here the first time around, the
           ;fp has already advanced to line 2
           (if (= (.getLineNumber f) 2)
             (dotimes i 2 (c))
             (str a \newline b))))
       
(defmacro rread [junks func]
      `(let [~((:stream junks) 0) ~((:stream junks) 1)
             ~((:chunk junks) 0) ~((:chunk junks) 1)
             f# ~func]
         (reduce (fn [x# y#]
                     (when ~(:close junks)
                       (.close ~((:stream junks) 0)))
                     (f# x# y#))
                 (pho ~((:stream junks) 0) ~((:chunk junks) 0)))))


(defn reduce-file [func chunker file]
      (let [file-s (pho file #(chunker file))]
        (reduce (fn [line1 line2]
                    (func file line1 line2))
                file-s)))

(reduce-file (fn [file line1 line2]
                 (when (= (.getLineNumber file) 10)
                   (.close file))
                 (str line1 \newline line2))
             (fn [x] (.readLine x))
             (-> "/home/hiredman/readme.txt"
                 java.io.File.
                 java.io.FileReader.
                 java.io.LineNumberReader.))


(((partial (partial comp (partial + 1)) (partial + 2)) 
                 (partial + 3))


 (defn a [& x]
       (fn [& y]
           (if (number? (first y))
             (a (+ (first x) (first y)))
             (first x))))

(reduce (fn [x y]
            (if (and (vector? x) (= (x 0) :scan))
              (let [nx (/ )])
              [:scan x (/ x y)]))
        [64 4 2 4])

(defn show
  ([x] (show x nil))
  ([x i]
      (let [c (if (class? x) x (class x))
            items (sort
                    (for [m (concat (.getFields c)
                                    (.getMethods c)
                                    (.getConstructors c))]
                      (let [static? (bit-and Modifier/STATIC
                                             (.getModifiers m))
                            method? (instance? Method m)
                            ctor?   (instance? Constructor m)
                            text (if ctor?
                                   (str "(" (apply str (interpose ", "
(.getParameterTypes m))) ")")
                                   (str
                                     (if (pos? static?) "static ")
                                     (.getName m) " : "
                                     (if method?
                                       (str (.getReturnType m) " ("
                                            (count (.getParameterTypes m)) ")")
                                       (str (.getType m)))))]
                        [(- static?) method? text (str m) m])))]
        (if i
          (last (nth items i))
          (do (println "=== " c " ===")
            (doseq [[e i] (map list items (iterate inc 0))]
              (printf "[%2d] %s\n" i (nth e 2))))))))

(import '(java.io BufferedReader
                  InputStreamReader))

(defn system [cmd-string]
  (let [process (.. Runtime getRuntime (exec cmd-string))
        reader (BufferedReader.
                (InputStreamReader.
                 (.getInputStream process)))]
    (apply str (rest (interpose "\n" (line-seq reader))))))




(def a (system "svn -v --xml --limit 5 log  https://clojure.svn.sourceforge.net/svnroot/clojure")

(def b "svn -v --xml --limit 5 log  https://clojure.svn.sourceforge.net/svnroot/clojure")

(def a
(clojure.xml/parse
  (.getInputStream (.. Runtime getRuntime (exec b))))
)
(defn partition-with [pred col]
      (if col
        (let [x (split-with pred col)]
          (lazy-cons (first x) (partition-with pred (last x))))
        nil))


(defn sandbox [func]
      (let [perms (java.security.Permissions.)
            domain (java.security.ProtectionDomain.
                     (java.security.CodeSource. nil
                                                (cast java.security.cert.Certificate nil))
                     perms)
            context (java.security.AccessControlContext. (into-array [domain]))
            pA (proxy [java.security.PrivilegedAction] [] (run [] (func)))]
        (System/setSecurityManager (SecurityManager.))
        (java.security.AccessController/doPrivileged
          pA context)))

(sandbox 
#(doto (-> "foo.bar" java.io.File. java.io.FileWriter.) (.write "foo") .close)
)
(defn parse-xml-url [url]
      (let [url (java.net.URL. url)
            stream (.openStream url)
            out (clojure.xml/parse stream)
            _ (.close stream)]
        out))

(import '(java.io File FileReader)
        '(org.supercsv.io CsvMapReader CsvBeanReader CsvListReader)
        '(org.supercsv.prefs CsvPreference))

(with-open [file (-> "2008-12-19.csv" File. FileReader.)
            csv (CsvListReader. file (CsvPreference/EXCEL_PREFERENCE))]
           (doall (map #(into [] %) (take-while #(> (count %) 0) (map #(%) (cycle [#(.read csv)]))))))

(def family-name second)

(defn street [x]
      (first (drop 4 x)))

(defn city [x]
      (first (drop 5 x)))

(defn state [x]
      (first (drop 6 x)))

(defn zip [x]
      (first (drop 7 x)))

(defn aname [x]
      (first (drop 17 x)))

(defn dob [x]
      (first (.split (first (drop 18 x)) " ")))

(defn email [x]
      (first (drop 21 x)))

(defn im [x]
      (first (drop 22 x)))


(defmacro amb [[v s] & forms]
      `(let [a# ~s
             ~v (first a#)]
         (try (do ~@forms)
              (catch Exception e#
                     ~(if s
                        `(amb [~v (rest a#)] ~@forms)
                        `(throw e#))))))

(defmacro amb [[a b] & forms]
  `(let [f# (fn [~a] ~@forms)]
     (loop [a# ~b]
           (if a#
             (let [b# (try (f# (first a#))
                           (catch Exception e# e#))]
               (if (= (class (Exception.)) (class b#))
                 (recur (rest a#))
                 b#))
             (throw (Exception. "amb failed"))))))




(macroexpand-1 `(amb [v (range 1 10)] (if (not= 8 v) (throw (Exception. "foo")) v)))


(amb [v [(do (println :0) 0) 1 (do (println :2) 2) 3 4 5 6 7 8]] (if (not= 8 v) (throw (Exception. "not an eight")) v))



(sort (fn [a b] (if (and (= a :d) (not= b :a)) 1 (if (= b :d) -1 0)))
      [:a :b :c :d])

  (defn word-seq [s]
      (re-seq #"[^ ,.]+" s))

(word-seq "Hello, my name is Kevin.")


(grapher {:lines (word-seq "Hello, my name is Kevin.")})

(grapher {:lines (word-seq "Kevin is my name.") :graph *1})

(defn grapher [{:keys [graph lines] :or {graph {}}}]
      (if lines
        (let [a (first lines)
              b (when-let [t (second lines)] t)
              r (if-let [c (graph a)] c {})
              c (inc (r b 0))
              r (assoc r b (inc (r b 0)))]
          (recur {:graph (if (graph a)
            (assoc-in graph [a b] c)
            (assoc graph a r))
                 :lines (rest lines)}))
        graph))


(defn shuffle [sides]
      (let [a (java.util.LinkedList. sides)
            _ (java.util.Collections/shuffle a)]
        (seq a)))

(defn next [edges]
      (first
        (shuffle
          (reduce #(apply conj
                          %
                          (take (second %2)
                                (cycle [(first %2)])))
                  '()
                  edges))))

(defn rnd-start [g]
      (first (shuffle (keys g))))

(defn drunkard-walk [{:keys [start graph result] :or {result []}}]
      (let [n (next (graph start))]
        (if n
          (recur {:start n :graph graph :result (conj result start)})
          result)))

(def bb (with-open [r (java.io.BufferedReader.
                        (java.io.FileReader.
                          "/usr/home/kpd/bookchan/Exit to Eden.txt"))]
                   (doall (line-seq r))))


(def t (make-graph t (map word-seq (filter #(not (.equals "" %)) bb))))

(defn make-graph [g [f & r]]
      (if f
        (recur (grapher {:lines f :graph (if g g {})}) r)
        g))

(apply str (interleave (drunkard-walk {:start (rnd-start t) :graph t}) (cycle (list " "))))

(defstruct n :n :c :p)

(defn zip-markov [root]
      (clojure.zip/zipper map?
                          (comp seq :c)
                          (fn [n c]
                              (assoc n :c (and (apply vector c))))
                          root))

(defn has-child-x? [l c]
      (some #{c} (map :n l)))

(defn get-child-x? [z c f]
      (when z
        (if (.equals (f (zip/node z)) c)
          z
          (recur (zip/left z) c f)))))

(defn grapher [{:keys [graph lines] :or {graph (zip-markov (struct n :root []))}}]
      (if lines
        (if-let [c (get-child-x? (zip/down graph) (first lines) :n)]
                (recur {:graph (zip/edit c (fn [n & args] (assoc n :p (inc (:p n)))))
                        :lines (rest lines)})
                (recur {:graph (zip/down
                                 (zip/insert-child
                                   graph (struct n (first lines) nil 1)))
                        :lines (rest lines)}))
        (zip/root graph)))

(-> (zip-markov (struct n :root []))
    (zip/append-child (struct n "Hello" nil 1))
    zip/down
    (zip/append-child (struct n "my" nil 1))
    zip/down
    (zip/append-child (struct n "name" nil 1))
    zip/down
    (zip/append-child (struct n "is" nil 1))
    zip/down
    (zip/append-child (struct n "Kevin" nil 1))
    zip/root
    zip-markov
    (zip/insert-child (struct n "Kevin" nil 1))
    zip/down
    (get-child-x? "Kevin" :n)
    (zip/append-child (struct n "is" nil 1))
    zip/root)

(defmacro my-doc [s]
      `(let [m# (meta (var ~s))
            al# (:arglists m#)
            docstring# (:doc m#)]
        (.replaceAll (.replaceAll (str al# "; " docstring# ) "\n" "") (str \\ \s) " ")))
    (get-child-x? "Kevin" :n))

(defmacro C [& exp])

(macroexpand-1
  '(C if (> 1 6){
   (println :a)
   } else {
   }))

(defn fold2 [f1 acc1 f2 acc2 list]
      (if (nil? list)
        [acc1 acc2]
        (recur f1 (f1 (first list) acc1) f2 (f2 (first list) acc2) (rest list))))

(fold2
  #(if (even? %) (conj %2 %) %2)
  []
  #(if (odd? %) (conj %2 %) %2)
  []
  (range 10000))

(defn seperate [pred list]
      (let [a #(if (pred %) (conj %2 %) %2)
            b #(if ((complement pred) %) (conj %2 %) %2)]
        (fold2 a [] b [] list)))

(time (seperate even? (range 1 100)))




(declare *fails* *succeeds* *exits*)
(def *horizon* nil)

(defmacro when-hrz [which & forms]
  `(do
     (when (not *horizon*) (throw (RuntimeException. "Naked Singularity")))
     (swap!
       ~(condp = which :fails '*fails* :succeeds '*succeeds* :exits '*exits*)
       conj (fn [] ~@forms))))

(defmacro horizon [& body]
  `(binding [*horizon* true
             *fails* (atom (list))
             *succeeds* (atom (list))
             *exits* (atom (list))]
            (try
              (let [y# (do ~@body)] (dorun (map (fn[x#](x#)) @*succeeds*)) y#)
              (catch Exception e# (dorun (map (fn[x#] (x#)) @*fails*))
                     (throw e#))
              (finally
                (dorun (map (fn[x#] (x#)) @*exits*))))))

(defn foo []
  (when-hrz :fails (prn "failed"))
  (when-hrz :succeeds (prn "succeeded 1"))
  (when-hrz :succeeds (prn "succeeded 2"))
  (when-hrz :exits (prn "exited")))

(horizon
 (foo)
 42)

(horizon
 (foo)
 (throw (Exception. "err"))
 42)

(defn censor [x]
      (when-hrz :exits (.close x))
      x)

(horizon
  (.readLine (censor (java.io.BufferedReader. (java.io.StringReader. "foo\n")))))

(defmacro pure [& body]
  (binding [*out* (proxy [java.io.StringWriter] [] (write [x] (throw (java.io.IOException. "OUT"))))
            *in* (java.io.PushbackReader. (proxy [java.io.StringReader] [""] (read [] (throw (java.io.IOException. "IN")))))]
           `(do
              (when (some (fn [x#] (= '. x#)) (tree-seq seq? macroexpand-1 '(do ~@body)))
                (throw (java.io.IOException. "Java")))
              ~@body)))

(pure
  :foo)

(defmacro mk-interface [class func]
  (let [c class
        method (symbol (:name (bean (first (.getMethods java.io.FilenameFilter)))))
        f func]
    `(proxy [~class] []
            (~method [& args#]
                     (apply ~f args#)))))

(defmacro mk-interface [class func]
  (concat (list 'proxy [`~class] [])
          (map #(list (symbol (.getName %))
                      ['& 'x#]
                      (list 'apply func (keyword (.getName %)) 'x#))
               `~(.getMethods `class))))

(macroexpand-1 '(mk-interface java.awt.event.KeyListener (fn [event n] n)))


(require '[clojure.zip :as zip])

(-> (zip/seq-zip '(a · b · (partial x (a · b))))
    zip/down
    zip/next
    zip/remove
    (zip/insert-left 'comp)
    zip/left
    zip/up
    delete· 
    zip/node)

(defn delete· [se]
      (zip/seq-zip (filter #(not= "·" (and (symbol? %) (name %))) (zip/node se))))

(defn find· [se]
      (if-let [x (and (zip/up se) (zip/next se))]
                (if (= "·" (let [c (zip/node x)] (and (symbol? c) (name c))))
                  (recur (-> x
                             zip/remove
                             (zip/insert-left 'clojure.core/comp)
                             zip/up
                             delete·
                             zip/down
                             zip/right))
                  (recur x))
                se))

(defmacro pl [& forms]
  (let [x (-> (zip/seq-zip forms)
              zip/down
              user/find·
              zip/root)]
    `(~@x)))

;(macroexpand `(pl ((c )c · b · a · b · c)))

;(do (clojure.core/comp user/c (clojure.core/comp user/a user/b)))

(pl
  ((first · map) (first · :list) [{:list '(:a :b)}]))

(macroexpand '(pl (first · map)))

(defn f [a b] [a b])

(try (f 'a)
     (catch IllegalArgumentException e
            (partial f 'a)))

(def #^{:doc "almost pointless inits"} inits
     (comp (partial map first)
           (partial take-while second)
           (partial map split-at (iterate inc 0))
           repeat
           (partial apply concat)
           reverse
           (partial list [nil])))

(.contains (map #(.getName %) (.getMethods (class (seq [:a])))) "contains")



(defn transform [se pred fn]
      (loop [se (zip/seq-zip se)]
            (if (zip/end? se)
              (zip/root se)
              (if (pred se)
                (recur (fn se))
                (recur (zip/next se))))))

(transform '((a · b) · (c · d))
           #(= "·" (let [c (zip/node %)] (and (symbol? c) (name c))))
           #(-> % zip/remove (zip/insert-left 'clojure.core/comp)))

(defmacro pl [& forms]
  (let [x (transform forms
                     #(= "·" (let [c (zip/node %)] (and (symbol? c) (name c))))
                     #(-> % zip/remove (zip/insert-left 'clojure.core/comp)))]
    `(do ~@x)))

(macroexpand '(pl ((c · d) · (a · b))))

(⌽ java.util.concurrent.Callable (fn [a] a))

(⌽ java.util.concurrent.Executors
   (fn [a] a))


(defmacro ⌽ [class fn]
  (conj
    (map #(list (second %)
                ['& 'x]
                (list 'apply 'fn (first %) 'x))
         (map #(vector (keyword %) (symbol %))
              (map #(.getName %) (.getMethods (class class))))) [] [`~class] 'proxy))

(count (.getTypeParameters (second (.getMethods (class "")))))

(.codePointAt "⌽foo" 0)
9021

(transform '(⌽foo bar baz beep)
           #(= 9021 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 0)))
           #(-> % (zip/replace (symbol (subs (name (zip/node %)) 1)))
                (zip/insert-left 'partial)))

(defn random-person [bot]
      (randth (filter #(not (.equals % (:nick bot)))
                      (apply concat (map last (everyone-I-see bot))))))

;; (comp randth
;;       (partial filter #(not (.equals % (:nick bot))))
;;       (partial apply concat)
;;       (partial map last)
;;       everyone-I-see)

(-> '(comp)
    zip/seq-zip
    zip/down
    (zip/insert-right (-> '(map last (everyone-I-see bot))
                          zip/seq-zip
                          zip/root))
    zip/root)

(cons 'partial (take-while #(not (seq? %)) '(map last (everyone-I-see bot))))

(f (comp not seq?) '(map last (everyone-I-see bot)))


(loop [exp '(a b (h y (g y))) op ['comp]]
      (if (and (not (empty? exp)) (seq? (last exp)))
        (recur (last exp) (conj op (let [x (butlast exp)]
                                     (if (> (count x) 1)
                                       (cons 'partial x)
                                       (first x)))))
        (seq (concat op exp))))


(-> (seq "(comp randth (first a))")
    zip/seq-zip
    zip/next
    (zip/insert-left \newline)
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/node)

(with-local-vars [depth 0]
(println
  (apply str (transform '(comp randth (first a))
                      #(do % true)
                      #(let [n (zip/node %)]
                         (cond
                           (and (= n \() )))))))

(defn paren-zip [exp])

(defn pipeline [a]
(loop [exp 'a op ['comp]]
      (if (and (not (empty? exp)) (seq? (last exp)))
        (recur (last exp) (conj op (let [x (butlast exp)]
                                     (if (> (count x) 1)
                                       (cons 'partial x)
                                       (first x)))))
        (seq (concat op exp)))))

(def tp (doto (javax.swing.JTextPane.)
              (.setPreferredSize (java.awt.Dimension. 800 600))))

(def w (javax.swing.JFrame. "text"))

(def p (doto (javax.swing.JPanel.)
             (.setPreferredSize (java.awt.Dimension. 800 600))))

(.add p tp)
(.add w p)

(.pack w)
(.setVisible w true)

(defmacro pl [& forms]
  (let [x (transform forms
                     #(= "·" (let [c (zip/node %)] (and (symbol? c) (name c))))
                     #(-> % zip/remove (zip/insert-left 'clojure.core/comp)))
        x (transform x
                     #(= 9021 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 0)))
                     #(-> % (zip/replace (symbol (subs (name (zip/node %)) 1)))
                            (zip/insert-left 'partial)))
        x (transform x
                     #(and (= 63 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 0)))
                           (> 58 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 1)) 47))
                     #(let [n (zip/node %)
                           num (Integer/parseInt (subs (name n) 1))]
                        (zip/replace % (list 'rand-int num))))]
    `(do ~@x)))

(pl 
  ((⌽map first) [[:a :b]]))

((comp first list) :a :b)

((fn [x]
    (apply concat (reverse (map reverse (partition (int (/ (count x) 2)) x)))))
 [:a :b :c :d :e :f :g])

(partition 1 [:a :b :c])

(int 1)

(concat )

(macroexpand
'(pl
  ((a · b) · (c · ?5))))

(transform '(+ 1 ?4)
           #(and (= 63 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 0)))
                 (> 58 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 1)) 47))
           #(let [n (zip/node %)
                  num (Integer/parseInt (subs (name n) 1))]
              (zip/replace % (list 'rand-int num))))

(defmulti  max- count :default :else)
(defmethod max- 0 [_] (throw (Exception. "max- on empty list")))
(defmethod max- 1 [[x]] x)
(defmethod max- :else [[a & b]]
  (let [max-b (trampoline max- b)] (if (> a max-b) a max-b)))


(time (max- (range 1000000)))

(time (apply max (range 1000000)))

(defn max- [x]
      (condp = (count x)
             0 (throw (Exception. "max- on empty list"))
             1 (first x)
             (count x) (let [[a b & c] x]
                         (if (> a b)
                           (recur (cons a c))
                           (recur (cons b c))))))

(range 1 21)

(defn div-by? [seq n]
      (reduce #(and % (zero? (rem n %2)))  true seq))

(div-by? (range 1 11) 2520)

(last (for [x (iterate inc 1) :when (not (div-by? (range 1 21) x))] x))


(def parameters (into-array [java.net.URL]))

(def sysloader (cast java.net.URLClassLoader (ClassLoader/getSystemClassLoader)))
(def sysclass java.net.URLClassLoader)

(def method (.getDeclaredMethod sysclass "addURL" parameters))

(.setAccessible method true)

(.invoke method sysloader (into-array Object [(.toURL (java.io.File. "./neo-1.0-b7/neo-1.0-b7.jar"))]))


(defn add-classpath- [path]
      (let [sysloader (cast java.net.URLClassLoader (ClassLoader/getSystemClassLoader))
            sysclass java.net.URLClassLoader
            parameters (into-array [java.net.URL])
            method (.getDeclaredMethod sysclass "addURL" parameters)
            _ (.setAccessible method true)]
        (.invoke method sysloader (into-array Object [(.toURL (java.io.File. path))]))))

(defstruct monad :wrap :pass :value)

(defn new-identity-monad [value]
      (fn this [& args]
          (condp = (first args)
                 :value
                    value
                 :pass
                    (new-identity-monad ((second args) (this :value))))))

(def m (new-identity-monad 1))

((:pass m) m)

(return m inc)

(defn mvector [value]
      (fn this [& args]
          (condp = (first args)
                 :value
                    (if (vector? value)
                      value
                      [value])
                 :join
                    (mvector (conj (this :value) (second args)))
                 :empty
                    (mvector [])
                 :+
                   (mvector (vec (concat (this :value) ((second args) :value)))) 
                 :pass
                    (mvector (vec (map (second args) (this :value)))))))

(((((mvector 1) :pass first) :join (mvector 2)) :join (mvector 3)) :value)

((((mvector [1 2 4]) :pass inc) :join 10) :value)

(((mvector [1 2 3]) :+ (mvector [4 5 6])) :value)

(defn mvector [val]
      (let [value #(if (vector? val)
                     val
                     [val])
            join #(mvector (conj (value) %))
            empty #(mvector [])
            + #(mvector (vec (concat (value) (:value %))))
            pass #(mvector (vec (map % (value))))]
        {:value (value) :join join :empty empty :+ + :pass pass}))

((((((((mvector 1) :join) 2) :+) (mvector [10 11 12])) :pass) inc) :value)

(defn mmaybe [value])

(defn throw-unsupported [& x]
      (throw (UnsupportedOperationException.)))

(def monad {:bind throw-unsupported :unit throw-unsupported})

(defn failable [value success]
      (let [unit #(failable % true)
            bind #(if success (% value))]
        (assoc monad
               :bind bind
               :unit unit
               :value value
               :success success)))

(defn success [x]
      (failable x true))

(defn failure [x]
      (failable x false))

(ByteBuffer/allocate 10)

(import '(java.nio ByteBuffer))

(import '(java.nio ByteBuffer))

(defn get-byte
        [#^ByteBuffer buf]
          (let [x (.val (clojure.lang.Box. (.get buf)))]
                (short (bit-and x 0xff))))
 
(defn get-short
        [#^ByteBuffer buf]
          (let [x (.val (clojure.lang.Box. (.getShort buf)))]
                (int (bit-and x 0xffff))))

(defn update-textarea [text-area buf]
      (.append text-area (String. buf, 0, len))
      (.setCaretPosition text-area (.. text-area (getDocument) (getLength)))
      (let [ideal-size 1000
            max-excess 500
            excess (- (.. text-area (getDocument) (getLength)) ideal-size)]
        (when (>= excess max-excess)
          (.replaceRange text-area "" 0 excess))))

(defn reader-thread [pi ta]
      (proxy [Thread]
             (run []
                  (loop []
                        (try
                          (let [buf (make-array Byte/TYPE 1024)
                                len (.read pi buf)]
                            (when (not= -1 len)
                              (SwingUtilities/invokeLater #(update-textarea ta buf))))
                          (catch IOException e
                                 (println e)))
                    (recur)))))

(import '(javax.swing JFrame JTextArea JScrollPane))
(import '(java.io PipedInputStream PipedOutputStream))

(defn console []
      (let [pi-out (PipedInputStream.)
            po-out (PipedOutputStream. pi-out)
            pi-error (PipedInputStream.)
            po-error (PipedOutputStream. pi-error)
            text-area (JTextArea.)
            window (JFrame. "Console")]
        (doto text-area
              (.setEditable false)
              (.setRows 20)
              (.setColumns 20))
        (.add (.getContentPane window)
              (JScrollPane. text-area))
        (doto window .pack (.setVisible true))
        (.start (reader-thread pi-out text-area))
        (.start (reader-thread pi-error text-area))))

(defmacro f [& x]
      `(try
         (~@x)
         (catch IllegalArgumentException e#
                (let [func# (partial ~@x)]
                  (fn [& t#] (apply func# t#))))))

((f map inc) (range 10))

(defn uncurry [x]
      (fn uc [& y]
          (if (seq y)
            (uncurry (apply partial x y))
            (x))))

(transform '(⌽map identity)
           #(= 9021 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 0)))
           #(-> % (zip/replace (list 'uncurry (symbol (subs (name (zip/node %)) 1))))))


(defmacro pl [& forms]
  (let [x (transform forms
                     #(= "·" (let [c (zip/node %)] (and (symbol? c) (name c))))
                     #(-> % zip/remove (zip/insert-left 'clojure.core/comp)))
        x (transform x
                     #(= 9021 (and (symbol? (zip/node %)) (.codePointAt (name (zip/node %)) 0)))
                     #(-> % (zip/replace (list 'uncurry (symbol (subs (name (zip/node %)) 1))))))
        ]
    `(do ~@x)))

(def binary
     (comp (partial format "%08d")
           #(Integer/parseInt %)
           #(Integer/toBinaryString %)
           int))
;http://funcall.blogspot.com/2008/03/problem-with-monads.html
;(define (bind M f)
;  (lambda (ma)
;      (let* ((intermediate (M ma))
;                 (i-state (get-state intermediate))
;                            (i-value (get-value intermediate))
;                                       (next    (f i-value)))
;                                             (next i-state))))

(defn bind [M f]
      (fn [me]))
