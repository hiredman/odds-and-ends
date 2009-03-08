(ns hiredman.pwalker
    (:import (java.io File)
             (java.util.concurrent ThreadPoolExecutor TimeUnit LinkedBlockingQueue)))

(def runner (ThreadPoolExecutor. (+ 1 (.availableProcessors (Runtime/getRuntime)))
                                 (+ 1 (.availableProcessors (Runtime/getRuntime)))
                                 (long 10000)
                                 TimeUnit/MINUTES
                                 (LinkedBlockingQueue.)))

(defn walk
      "walks a directory recursively, queing up recursive walks
      on a threadpoolexecutor. returns a list of files"
      ([dir]
       (let [result (ref []) co-ord (ref 1)]
         (.execute runner #^Callable #(do
                                        (walk (File. dir) result co-ord)
                                        (dosync (alter co-ord dec))))
         (loop []
               (when (> @co-ord 0)
                 (recur)))
         @result))
      ([dir results co]
       (let [files (.listFiles dir)
            f (seq (filter #(.isDirectory %) files))
            d (seq (filter #(not (.isDirectory %)) files))]
           (doseq [fi f]
                  (dosync (alter co inc))
                  (.execute runner #^Callable #(do
                                                 (walk fi results co)
                                                 (dosync (alter co dec)))))
           (doseq [di d]
                  (dosync
                    (alter results conj di))))))
