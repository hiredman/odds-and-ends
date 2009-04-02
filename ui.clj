(defn fn->al [fn]
  (proxy [java.awt.event.ActionListener] []
    (actionPerformed [event] (fn event))))
