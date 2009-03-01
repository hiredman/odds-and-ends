(ns hiredman.crypto
    (:import (java.io FileInputStream FileOutputStream File FileWriter FileReader InputStreamReader OutputStreamWriter BufferedReader)
             (javax.crypto Cipher KeyGenerator CipherInputStream CipherOutputStream)
			 (javax.crypto.spec SecretKeySpec)))

(defn cipher []
      (Cipher/getInstance "DES/ECB/PKCS5Padding"))

(defn generate-key []
      (.generateKey (KeyGenerator/getInstance "DES")))

(defn encipher [key cleartext]
      (.doFinal
        (doto (cipher)
              (.init Cipher/ENCRYPT_MODE key))
        cleartext))

(defn decipher [key ciphertext]
      (.doFinal
        (doto (cipher)
              (.init Cipher/DECRYPT_MODE key))
        ciphertext))

(defn write-key [key file]
	  (with-open [f (-> file java.io.File. java.io.FileOutputStream.)]
				 (.write f (.getEncoded key))))

(defn read-key [file]
	  (with-open [f (-> file java.io.File. java.io.FileInputStream.)]
				 (let [buf (make-array Byte/TYPE (.available f))]
				   (.read f buf)
				   (SecretKeySpec. buf "DES"))))

(defn cipher-writer [key file]
	  (let [c (doto (cipher) (.init Cipher/ENCRYPT_MODE key))
			op-s (-> file File. FileOutputStream.)
			cop-s (CipherOutputStream. op-s c)]
		(-> cop-s OutputStreamWriter.)))

(defn cipher-reader [key file]
	  (let [c (doto (cipher) (.init Cipher/DECRYPT_MODE key))
			op-s (-> file File. FileInputStream.)
			cop-s (CipherInputStream. op-s c)]
		(-> cop-s InputStreamReader.)))

;; (let [k (generate-key)
;;       c (cipher)
;;       cleartext (.getBytes "foo")
;;       ciphertext (.doFinal
;;                    (doto c (.init Cipher/ENCRYPT_MODE k))
;;                    cleartext)
;;       cleartext2 (.doFinal
;;                    (doto c (.init Cipher/DECRYPT_MODE k))
;;                    ciphertext)
;; 	  _ (write-key k "/tmp/34")
;; 	  k2 (read-key "/tmp/34")]
;;   (assert (= (seq cleartext) (seq cleartext2)))
;;   (assert (= k k2))
;;   (with-open [o (cipher-writer k "/tmp/34")]
;; 			 (.write o "foobar baz"))
;;   (assert (= "foobar baz" (with-open [i (java.io.BufferedReader. (cipher-reader k "/tmp/34"))] (.readLine i)))))

;; (let [key (generate-key)]
;;   (with-open [outp (cipher-writer key "/home/hiredman/two")]
;;(write-key key "/home/hiredman/two.key")
;;(.write outp (slurp "/home/hiredman/.two"))))
