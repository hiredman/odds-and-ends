(import '(java.awt.event ActionListener)
        '(javax.swing JFrame JButton SwingUtilities))

(defn fn->al [fn]
  (proxy [ActionListener] []
    (actionPerformed [event] (future (fn event)))))

(defmacro swing-invoke [& body]
  `(SwingUtilities/invokeLater
     (fn [] ~@body)))

(defmacro window [title & forms]
  `(doto (new javax.swing.JFrame ~title) ~@forms))

(let [counter (atom 0)
      update (fn [event]
               (swap! counter inc)
               (swing-invoke (.setText (.getSource event) (str @counter))))]
  (window "Hello Swing"
          (.setSize 200 100)
          (.add (doto (JButton. "Press me")
                      (.addActionListener (fn->al update))))
          .show))
