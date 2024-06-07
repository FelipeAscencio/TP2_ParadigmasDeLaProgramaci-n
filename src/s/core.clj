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



(defrecord Tortuga [x y angulo pluma-abajo?])

(defn crear-tortuga [tortuga]
  (let [nueva-tortuga (assoc tortuga
                        :x (:x tortuga)
                        :y (:y tortuga)
                        :angulo (:angulo tortuga)
                        :pluma-abajo? (:pluma-abajo? tortuga))]
    nueva-tortuga))

(defn avanzar [tortuga n]
  (let [rads (Math/toRadians (:angulo tortuga))
        dx (* n (Math/cos rads))
        dy (* n (Math/sin rads))
        new-x (+ (:x tortuga) dx)
        new-y (+ (:y tortuga) dy)]
    (assoc tortuga :x new-x :y new-y)))

(defn gira-derecha [tortuga angulo]
  (assoc tortuga :angulo (- (:angulo tortuga) angulo)))

(defn gira-izquierda [tortuga angulo]
  (assoc tortuga :angulo (+ (:angulo tortuga) angulo)))

(defn pluma-arriba [tortuga]
  (assoc tortuga :pluma-abajo? false))

(defn pluma-abajo [tortuga]
  (assoc tortuga :pluma-abajo? true))


;ver desde aca mas que nada
(defn actualizar-camino [tortuga nueva-tortuga camino]
  (conj camino {:x1 (:x tortuga) :y1 (:y tortuga) :x2 (:x nueva-tortuga) :y2 (:y nueva-tortuga)}))

(defn ejecutar-comando2 [comando angulo tortugas camino]
  (let [tortuga (first tortugas)]
    (cond
      (#{\F \G} comando)
      (let [nueva-tortuga (avanzar tortuga 1)]
        [(conj (rest tortugas) nueva-tortuga) (actualizar-camino tortuga nueva-tortuga camino)])
      (#{\f \g} comando)
      (let [nueva-tortuga (avanzar tortuga 1)]
        [(conj (rest tortugas) nueva-tortuga) camino])
      (= comando \+) [(conj (rest tortugas) (gira-derecha tortuga angulo)) camino]
      (= comando \-) [(conj (rest tortugas) (gira-izquierda tortuga angulo)) camino]
      (= comando \[) [(conj tortugas (crear-tortuga tortuga)) camino]
      (= comando \]) [(pop tortugas) camino]
      (= comando \|) [(conj (rest tortugas) (gira-derecha tortuga 180)) camino]
      :else [tortugas camino])))


(defn calcular-limites [camino margen]
      (let [x-values (mapcat [:x1 :x2] camino)
            y-values (mapcat [:y1 :y2] camino)
            min-x (- (apply min x-values) margen)
            max-x (+ (apply max x-values) margen)
            min-y (- (apply min y-values) margen)
            max-y (+ (apply max y-values) margen)]
           [min-x max-x min-y max-y]))


(defn generar-svg [camino margen]
  (let [[min-x max-x min-y max-y] (calcular-limites camino margen)
        view-box (str min-x " " min-y " " (- max-x min-x) " " (- max-y min-y))
        svg-header (str "<svg viewBox=\"" view-box "\" xmlns=\"http://www.w3.org/2000/svg\">")
        svg-content (clojure.string/join "" (map #(str "<line x1=\"" (:x1 %) "\" y1=\"" (:y1 %) "\" x2=\"" (:x2 %) "\" y2=\"" (:y2 %) "\" stroke-width=\"1\" stroke=\"black\" />\n") camino))
        svg-footer "</svg>"]
    (str svg-header svg-content svg-footer)))


(defn guardar-svg-en-archivo [camino nombre-archivo]
  (let [contenido-svg (generar-svg camino 0)]
    (spit nombre-archivo contenido-svg)))