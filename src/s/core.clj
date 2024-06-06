(ns s.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

; Esta función parsea las líneas del archivo, extrae el ángulo (como double), el estado inicial y las reglas.
(defn parsear-lineas
  [lineas]
  (let [angulo (Double/parseDouble (first lineas))
        estado-inicial (second lineas)
        reglas (apply hash-map (mapcat #(str/split % #" ") (drop 2 lineas)))]
    [angulo estado-inicial reglas]))

; FUNCION IMPURA! Esta función abre el archivo, lo lee e invoca a la función de parseo.
; Es impura porque tiene contacto con el exterior (el archivo).
(defn leer-archivo
  [ruta-archivo]
  (with-open [rdr (io/reader ruta-archivo)]
    (let [lineas (line-seq rdr)]
      (parsear-lineas lineas))))

; Esta función aplica las reglas de conversión al estado actual.
(defn aplicar-reglas
  [estado reglas]
  (apply str (map (fn [c] (get reglas (str c) (str c))) estado)))

; Esta función realiza las iteraciones del sistema-L.
(defn iteraciones-sistema-l
  [estado-inicial reglas iteraciones]
  (loop [estado-actual estado-inicial
         n iteraciones]
    (if (zero? n)
      estado-actual
      (recur (aplicar-reglas estado-actual reglas) (dec n)))))

; Esta función construye la colección final con el ángulo y el estado final.
; Es decir, las reglas y patrones que va a seguir la tortuga.
(defn construir-reglamento
  [angulo estado-final]
  [angulo estado-final])

(defn -main
  [& args]
  (let [nombre-archivo (first args)
        iteraciones (Integer/parseInt (second args))
        ruta-archivo (str "doc/" nombre-archivo)]
    (if (and nombre-archivo iteraciones)
      (let [informacion-parseada (leer-archivo ruta-archivo)
            [angulo estado-inicial reglas] informacion-parseada]
        (println "Colección inicial:")
        (println [iteraciones angulo estado-inicial reglas])
        (let [estado-final (iteraciones-sistema-l estado-inicial reglas iteraciones)
              reglamento-tortuga (construir-reglamento angulo estado-final)]
          (println "Estado final del sistema-L:" estado-final)
          (println "Colección final:")
          (println reglamento-tortuga)))
      (println "Por favor, proporciona el nombre del archivo y el número de iteraciones como argumentos."))))