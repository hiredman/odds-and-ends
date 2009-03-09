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

(defn walk [dir dir-fn file-fn r]
       (let [files (.listFiles #^File dir)
             f (seq (filter #(.isDirectory #^File %) files))
             d (seq (filter #(not (.isDirectory #^File %)) files))
             [++1 +-1] (if r
                         [#(dosync (alter r inc)) #(dosync (alter r dec))]
                         (repeat 2 #(do nil)))]
         (doseq [entry files]
                (++1)
                (if (.isDirectory entry)
                  (execute #(do
                              (dir-fn entry)
                              (walk entry dir-fn file-fn r)
                              (+-1)))
                  (do
                    (file-fn entry)
                    (+-1))))))

(defn collect-files [dir & opt]
      (let [q (LinkedBlockingQueue.)
            f (fn f [#^LinkedBlockingQueue q]
                  (lazy-seq
                    (when (.peek q)
                      (cons (.take q) (f q)))))
            fil (if opt (first opt) identity) 
            x #(when (fil %) (.put q %))
            count (ref 0)]
        (walk (File. #^String dir) identity x count)
        (take-while #(when (or (not= 0 @count) %) %)
                    (repeatedly #(.poll q)))))
