(ns hiredman.xmpp
    (:import (org.jivesoftware.smack XMPPConnection ConnectionConfiguration
                                     RosterListener PrivacyListManager
                                     PacketListener SSLXMPPConnection)
             (org.jivesoftware.smack.packet IQ IQ$Type Presence Presence$Type)
             (org.jivesoftware.smack.filter PacketFilter)
             (org.jivesoftware.smackx.packet VCard)
             (org.jivesoftware.smackx.filetransfer FileTransferManager)
             (java.io File)))

(defn presence [type & msg]
      (let [p (Presence. (Presence$Type/valueOf (name type)))]
        (when msg
          (.setStatus p (apply str msg)))
        p))


(defn roster-listener [func]
      (proxy [RosterListener] []
             (entriesAdded [& args]
                           (apply func args))))

(defn create-chat [conn who ml]
      (.createChat (.getChatManager conn) who ml))

(defn add-ml [conn who ml]
      (let [conn (chat conn who)]
        (.addMessageListener ((:chats conn) who) ml)
        conn))

(defn add-to-roster [conn who & [nickname group]]
      (.createEntry (.getRoster (:connection conn)) who (str nickname) group)
      conn)

(defn send-file [conn who file & huh]
      (let [manager (FileTransferManager. (:connection conn))
            transfer (.createOutgoingFileTransfer manager who)
            x (.sendFile transfer (File. file) (apply str huh))]
        (assoc conn :ftm [manager transfer])))

(defn flush-roster [conn]
      (dorun
        (map #(.removeEntry(.getRoster (:connection conn)) %)
             (.getEntries (.getRoster (:connection conn)))))
      conn)

(defn update-presence [con pres]
      (send-off con (fn [c]
                        (.sendPacket (:connection c) pres)
                        c)))

(defn avatar-bytes [filename]
      (let [file (java.io.File. filename)
            length (.length file)
            is (java.io.FileInputStream. file)
            bytes (make-array Byte/TYPE length)]
        (.read is bytes)
        bytes))

(defn set-avatar [conn url]
      (send conn
            (fn [c]
                (let [co (:connection c)
                      vc (VCard.)
                      x (.setAvatar vc url)
                      x (.save vc co)]
                  c))))

(defn roster [conn]
      (.getEntries (.getRoster (:connection conn))))

(defn chute
      "make a chute, takes an agent holding an xmpp struct
      a jid and an optional function of arity 2 that to be used as a msg listener
      the optional function will be passed the chat and message object. if you don't
      pass a message listener func then messages are put in a que as a tuple of strings.
      a chute is a function, sort of a poor man's multimethod. it dispatches on the 
      first argument. :send sends the rest of args as a string to the jid. 
      "
      [con who & infunc]
      (let [inq (ref [])]
        (if (not infunc)
          (send-off con add-ml
                    who
                    (message-lr c m
                                (dosync
                                  (ref-set inq (cons [(str (.getParticipant c)) (.getBody m)] @inq)))))
          (send-off con add-ml
                    who
                    (message-lr c m
                                ((first infunc) c m))))
        (fn [& args]
            (cond
              (= :send (first args))
                (send-off con msg who (apply str (rest args)))
              (= :recv (first args))
                (let [v (last @inq)]
                  (dosync (ref-set inq (butlast @inq)))
                  v)
              (= :send-file (first args))
                (send-off con send-file 
                          (-> (:connection @con)
                              .getRoster
                              (.getPresence who) .getFrom)
                          (frest args) (apply str (rest (rest args))))
              (= :listeners (first args))
                :stuff
              :else
                (throw (Exception. (str "whoops line: " (:line (meta #'chute)))))))))

(defmacro defchute
  "macro for defining a chute"
  [nam conn who & infunc]
  `(def ~nam (chute ~conn ~who ~@infunc)))


(import '(org.jivesoftware.smack XMPPConnection ConnectionConfiguration
                                 RosterListener PrivacyListManager
                                 PacketListener)
        '(org.jivesoftware.smack.packet Message Message$Type IQ IQ$Type Presence Presence$Type)
        '(org.jivesoftware.smack.filter PacketFilter)
        '(org.jivesoftware.smackx.packet VCard)
        '(org.jivesoftware.smackx.filetransfer FileTransferManager)
        '(java.io File))


(defn connect [jid pass]
      (doto (XMPPConnection. (last (.split jid "@")))
            .connect
            (.login (first (.split jid "@")) pass)))

(defn message [dingus]
      (doto (Message.)
            (.setBody (:body dingus))
            (.setLanguage (:lang dingus))
            (.setSubject (:subject dingus))
            (.setThread (:thread dingus))
            (.setType (:type dingus Message$Type/chat))))

(defmacro message-lr
  "macro to generate a message listener with selected symbols bound to
  the chat and message object"
  [ch me & body]
  `(proxy [org.jivesoftware.smack.MessageListener] []
          (processMessage [c# m#]
                          (let [~ch c# ~me m#]
                            ~@body))))

(defn chat [connection jid listener]
      (.createChat (.getChatManager connection) jid listener))

(def chat (memoize chat))

;; (.sendMessage (chat c "redchin@gmail.com" (message-lr _ _ nil)) (message {:body "Hello, how are you?"}))
;; 
;; (def c (connect "drone1@thelastcitadel.com" "1enord"))

