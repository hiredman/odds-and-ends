(ns hiredman.crypto
    (:import (java.io FileInputStream FileOutputStream File FileWriter FileReader InputStreamReader OutputStreamWriter BufferedReader)
             (javax.crypto Cipher KeyGenerator CipherInputStream CipherOutputStream)
			 (javax.crypto.spec SecretKeySpec)))

<<<<<<< HEAD:crypto.clj
(defstruct crypt :cipher-type :key-type :key :cipher)

(defn cipher
      "returns an instance of DES cipher"
      []
      (Cipher/getInstance "DES/ECB/PKCS5Padding"))

(defn de [c key]
      (doto c (.init Cipher/DECRYPT_MODE key)))

(defn en [c key]
      (doto c (.init Cipher/ENCRYPT_MODE key)))

(defn generate-key
      "generates a DES key"
      []
      (.generateKey (KeyGenerator/getInstance "DES")))

(defn encipher
      "enciphers cleartext using key"
      [{:keys [cipher key]} cleartext]
      (.doFinal (en cipher key) cleartext))

(defn decipher
      "decipher ciphertext using key"
      [key ciphertext]
      (.doFinal (de (cipher) key) ciphertext))
=======
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
>>>>>>> 7ef9e4e4de2f2bb04819b777eceb3d854fe1521f:crypto.clj

(defn write-key [key file]
	  (with-open [f (-> file java.io.File. java.io.FileOutputStream.)]
				 (.write f (.getEncoded key))))

(defn read-key [file]
	  (with-open [f (-> file java.io.File. java.io.FileInputStream.)]
				 (let [buf (make-array Byte/TYPE (.available f))]
				   (.read f buf)
				   (SecretKeySpec. buf "DES"))))

(defn cipher-output-stream [key stream]
<<<<<<< HEAD:crypto.clj
      (let [c (en (cipher) key)]
        (-> stream (CipherOutputStream. c))))

(defn cipher-input-stream [key stream]
      (let [c (de (cipher) key)]
        (-> stream (CipherInputStream. c))))
=======
	  (let [c (doto (cipher) (.init Cipher/ENCRYPT_MODE key))]
		(-> stream (CipherOutputStream. c))))

(defn cipher-input-stream [key stream]
	  (let [c (doto (cipher) (.init Cipher/DECRYPT_MODE key))]
		(-> stream (CipherInputStream. c))))
>>>>>>> 7ef9e4e4de2f2bb04819b777eceb3d854fe1521f:crypto.clj

(defn cipher-writer [key file]
	  (let [op-s (-> file File. FileOutputStream.)
			cop-s (cipher-output-stream key op-s)]
		(-> cop-s OutputStreamWriter.)))

(defn cipher-reader [key file]
	  (let [op-s (-> file File. FileInputStream.)
			cop-s (cipher-input-stream key op-s)]
		(-> cop-s InputStreamReader.)))

;; (let [k (generate-key)
<<<<<<< HEAD:crypto.clj
;;       cipher_ (cipher)
;;       text  (apply str (map (comp char (partial + 40)) (take 100 (repeatedly #(rand-int 80)))))
;;       cleartext (.getBytes text)
;;       ciphertext (.doFinal (en cipher_ k) cleartext)
;;       cleartext2 (.doFinal (de cipher_ k) ciphertext)
;;       file (File/createTempFile "crypto" "test")
;;       _ (.deleteOnExit file)
;;       file (.toString file)
;;       _ (write-key k file)
;;       k2 (read-key file)
;;       _ (with-open [o (cipher-writer k file)] (.write o text))
;;       file-text (with-open [i (java.io.BufferedReader. (cipher-reader k file))] (.readLine i))]
;;   (assert (= (seq cleartext) (seq cleartext2)))
;;   (assert (= k k2))
;;   (assert (= text file-text)))
=======
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
>>>>>>> 7ef9e4e4de2f2bb04819b777eceb3d854fe1521f:crypto.clj
