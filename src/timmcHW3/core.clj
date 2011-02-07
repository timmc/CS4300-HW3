(ns timmcHW3.core
   "Core code. Use -main."
   (:import [javax.swing SwingUtilities JFrame JPanel JMenu JMenuBar JMenuItem JButton]
            [java.awt.event ActionListener])
   (:gen-class))

;-- Menu items --;

(def mi-hello
   (proxy [ActionListener] []
      (actionPerformed [e] (println "Clicked!" e))))

;-- Setup --;

(defn launch
   "Create and display the GUI."
   []
   (def mb (doto (JMenuBar.)
              (.add (doto (JMenu. "Spline")
                       (.add (doto (JMenuItem. "Hello!")
                                (.addActionListener mi-hello)))))))
   (def frame (doto (JFrame.)
                 (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                 (.setSize 200 200)
                 (.setJMenuBar mb)
                 ))
   (. frame setVisible true))

(defn -main
   "Main sequence" ;FIXME
   [& args]
   (SwingUtilities/invokeLater launch))


