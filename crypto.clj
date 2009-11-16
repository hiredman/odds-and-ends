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

(defn cipher-output-stream [key stream]
	  (let [c (doto (cipher) (.init Cipher/ENCRYPT_MODE key))]
		(-> stream (CipherOutputStream. c))))

(defn cipher-input-stream [key stream]
	  (let [c (doto (cipher) (.init Cipher/DECRYPT_MODE key))]
		(-> stream (CipherInputStream. c))))

(defn cipher-writer [key file]
	  (let [op-s (-> file File. FileOutputStream.)
			cop-s (cipher-output-stream key op-s)]
		(-> cop-s OutputStreamWriter.)))

(defn cipher-reader [key file]
	  (let [op-s (-> file File. FileInputStream.)
			cop-s (cipher-input-stream key op-s)]
		(-> cop-s InputStreamReader.)))

