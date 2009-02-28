(ns hiremdna.crypto
    (:import '(java.io FileInputStream FileOutputStream)
             '(javax.crypto Cipher KeyGenerator)))

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

(let [k (generate-key)
      c (cipher)
      cleartext (.getBytes "foo")
      ciphertext (.doFinal
                   (doto c (.init Cipher/ENCRYPT_MODE k))
                   cleartext)
      cleartext2 (.doFinal
                   (doto c (.init Cipher/DECRYPT_MODE k))
                   ciphertext)]
  (assert (= (seq cleartext) (seq cleartext2))))
