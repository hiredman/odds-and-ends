(ns hiredman.zip
	(:import (java.util ZipEntry ZipOutputStream ZipInputStream)
			 (java.io ByteArrayInputStream ByteArrayOutputStream)))

(defn zip-string [name string]
	  (let [a (ByteArrayOutputStream. )]
		(doto (ZipOutputStream. a)
			  (.putNextEntry (ZipEntry. name))
			  (.write (.getBytes string))
			  .finish
			  .close)
		(.toByteArray a)))

(defn unzip-string [string])
