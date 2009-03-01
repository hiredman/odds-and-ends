(ns hiredman.crypto
    (:import (java.io FileInputStream FileOutputStream File FileWriter FileReader InputStreamReader OutputStreamWriter BufferedReader)
             (javax.crypto Cipher KeyGenerator CipherInputStream CipherOutputStream)
			 (javax.crypto.spec SecretKeySpec)))

(defn cipher
      "returns an instance of DES cipher"
      []
      (Cipher/getInstance "DES/ECB/PKCS5Padding"))

(defn generate-key
      "generates a DES key"
      []
      (.generateKey (KeyGenerator/getInstance "DES")))

(defn encipher
      "enciphers cleartext using key"
      [key cleartext]
      (.doFinal
        (doto (cipher)
              (.init Cipher/ENCRYPT_MODE key))
        cleartext))

(defn decipher
      "decipher ciphertext using key"
      [key ciphertext]
      (.doFinal
        (doto (cipher)
              (.init Cipher/DECRYPT_MODE key))
        ciphertext))

(defn write-key
      "write key to file"
      [key file]
	  (with-open [f (-> file java.io.File. java.io.FileOutputStream.)]
				 (.write f (.getEncoded key))))

(defn read-key
      "read key from file"
      [file]
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

(let [k (generate-key) c (cipher)
      text  (apply str (map (comp char (partial + 40)) (take 100 (repeatedly #(rand-int 80)))))
      cleartext (.getBytes text)
      ciphertext (.doFinal (doto c (.init Cipher/ENCRYPT_MODE k)) cleartext)
      cleartext2 (.doFinal (doto c (.init Cipher/DECRYPT_MODE k)) ciphertext)
      file (File/createTempFile "crypto" "test") _ (.deleteOnExit file)
      file (.toString file) _ (write-key k file)
	  k2 (read-key file)]
  (assert (= (seq cleartext) (seq cleartext2)))
  (assert (= k k2))
  (with-open [o (cipher-writer k file)]
			 (.write o text))
  (assert (= text (with-open [i (java.io.BufferedReader. (cipher-reader k file))] (.readLine i)))))
