(import '(java.awt.event ActionListener)
        '(javax.swing JFrame JButton SwingUtilities))

(defn fn->al [fn]
  (proxy [ActionListener] [] (actionPerformed [event] (future (fn event)))))

(defmacro EDT [& body]
  `(SwingUtilities/invokeLater (fn [] ~@body)))

(defmacro window [title & forms]
  `(doto (javax.swing.JFrame. ~title) ~@forms))

(let [counter (atom 0)
      update (fn [event] (swap! counter inc)
               (EDT (.setText (.getSource event) (str @counter))))]
  (window "Hello Swing" (.setSize 200 100)
          (.add (doto (JButton. "Press me")
                  (.addActionListener (fn->al update))))
          .show))
