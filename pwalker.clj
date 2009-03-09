(ns hiredman.pwalker
    (:import (java.io File)
             (java.util.concurrent ThreadPoolExecutor TimeUnit LinkedBlockingQueue)))

(def #^{:private true}
     runner (ThreadPoolExecutor. (+ 1 (.availableProcessors (Runtime/getRuntime)))
                                 (+ 1 (.availableProcessors (Runtime/getRuntime)))
                                 (long 10000)
                                 TimeUnit/MINUTES
                                 (LinkedBlockingQueue.)))

(defn- execute [#^Callable fn]
       (.execute #^ThreadPoolExecutor runner fn))

(defn walk
      "takes a direcotyr as a File, a function to be applied to all directories
      a fn to be applied to all files, returns a ref for bookkeeping"
      ([dir dir-fn file-fn]
       (walk dir dir-fn file-fn (ref 0)))
      ([dir dir-fn file-fn r]
       (let [files (.listFiles #^File dir)
             [++1 +-1] [#(dosync (alter r inc)) #(dosync (alter r dec))]]
         (doseq [entry files]
                (++1)
                (if (.isDirectory entry)
                  (execute #(do
                              (dir-fn entry)
                              (walk entry dir-fn file-fn r)
                              (+-1)))
                  (do
                    (file-fn entry)
                     (+-1)))))
       r))



(defn collect-files
      "takes a directory name as a string, and optionally a predicate to filter files
      and walks a directory in a recursive and concurrent manner"
      [dir & opt]
      (let [q (LinkedBlockingQueue.)
            f (fn f [#^LinkedBlockingQueue q]
                  (lazy-seq
                    (when (.peek q)
                      (cons (.take q) (f q)))))
            fil (if opt (first opt) identity) 
            x #(when (fil %) (.put q %))
            count (walk (File. #^String dir) identity x)]
        (take-while #(when (or (not= 0 @count) %) %)
                    (repeatedly #(.poll q)))))
