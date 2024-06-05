(ns s.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn -main
  "Lee un archivo y lo imprime línea por línea."
  [& args]
  (let [filename (first args)
        filepath (if filename
                   (str "doc/" filename)
                   nil)]
    (if filepath
      (with-open [rdr (io/reader filepath)]
        (doseq [line (line-seq rdr)]
          (println line)))
      (println "Por favor, proporciona el nombre del archivo como argumento."))))