;----------------------------------------------------------
; Proyecto integrador: Simulador y ensamblador de una
; máquina de Von Neumann
; Fecha: 15 de junio, 2022.
; Autores:
;          A01745865 Jose Angel Garcia Gomez
;          A01745907 Jorge Isidro Blanco Martinez
;----------------------------------------------------------
(ns proyecto-integrador)

(defrecord Machine [memory pc sp])

(defn make-machine
  "Crea la máquina de Von Neumann"
  [code size]
  (->Machine (vec (take size (concat code (repeat 0)))) 0 size))
(defn ct
  "Agrega una constante a la pila"
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :memory (assoc memory (dec sp) (memory (inc pc)))
    :pc (+ pc 2)
    :sp (dec sp)))
(defn out
  "Elimina un valor de la pila y lo imprime en la salida estándar seguido de un espacio"
  [{:keys [memory pc sp] :as machine}]
  (print (str (memory sp) " "))
  (assoc machine
    :pc (inc pc)
    :sp (inc sp)))
(defn nop
  "No hace nada"
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :pc (inc pc)))
(defn ld
  "Inserta el valor en la pila en el índice deseado"
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :memory (assoc memory (dec sp) (memory (memory(inc pc))))
    :pc (+ pc 2)
    :sp (dec sp)))
(defn ldi
  "Quita el índice de la pila e inserta el valor dado en ese índice"
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :memory (assoc memory sp (memory (memory sp)))
    :pc (inc pc)))
(defn st
  "Quita un valor de la pila y lo almacena en la memoria en el índice"
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :memory (assoc memory (memory (inc pc)) (memory sp))
    :pc (+ pc 2)
    :sp (inc sp)))
(defn sti
  "Quita el índice de la pilay lo guarda en la memoria en el índice"
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :memory (assoc memory (memory sp) (memory (inc sp)))
    :pc (inc pc)
    :sp (+ sp 2)))
(defn pop
  "Descarta el superior de la pila"
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :pc (inc pc)
    :sp (inc sp)))
(defn swp
  "Quita dos elementos de la pila y los devuelve en orden invertido"
  [{:keys [memory pc sp] :as machine}]
  (let [t1 (memory sp)
        t2 (memory (inc sp))]
    (assoc machine
      :memory (assoc memory sp t2 (inc sp) t1)
      :pc (inc pc))
    ))
(defn dup
  "Quita un valor de la pila y lo inserta dos veces"
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :memory (assoc memory (dec sp) (memory sp))
    :pc (inc pc)
    :sp (dec sp)))
(defn eqz
  "Quita el valor superior de la pila y si es cero inserta uno si no inserta cero"
  [{:keys [memory pc sp] :as machine}]
  (if (zero? (memory sp))
    (assoc machine
      :memory (assoc memory sp 1)
      :pc (inc pc))
    (assoc machine
      :memory (assoc memory sp 0)
      :pc (inc pc))))
(defn jp
  "Continúa la ejecución del programa en la instrucción en la posición índice
  de la memoria"
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :pc (memory (inc pc))))
(defn jpc
  "Quita un valor de la pila si no es igual a cero continua la ejecución del programa
  en la instrucción contenida en la posición índice de otra forma continúa en la
  siguiente instrucción"
  [{:keys [memory pc sp] :as machine}]
  (if (zero? (memory sp))
    (assoc machine
      :pc (+ pc 2))
    (assoc machine
      :pc (memory (inc pc)))))
(defn jpi
  "Quita el índice de la pila y continúa la ejecución del programa en la instrucción
  contenida en la memoria en índice"
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :pc (memory sp)
    :sp (inc sp)))
(defn make-comparation
  "Hace operación de comparación"
  [operation]
  (fn [{:keys [memory pc sp] :as machine}]
    (if (operation (memory (inc sp)) (memory sp))
      (assoc machine
        :memory (assoc memory (inc sp) 1)
        :pc (inc pc)
        :sp (inc sp))
      (assoc machine
        :memory (assoc memory (inc sp) 0)
        :pc (inc pc)
        :sp (inc sp))
      )))
(defn make-operation
  "Hace la operación dependiendo del símbolo que recibe"
  [operation]
  (fn [{:keys [memory pc sp] :as machine}]
    (assoc machine
      :memory (assoc memory
                (inc sp)
                (operation (memory (inc sp))
                           (memory sp)))
      :pc (inc pc)
      :sp (inc sp))))
(defn chr
  "Quita valor de la pila. Imprime en la salida estándar el carácter con un punto de
   código unicode igual al valor, no se agrega espacio"
  [{:keys [memory pc sp] :as machine}]
  (print (char (memory sp)))
  (assoc machine
    :pc (inc pc)
    :sp (inc sp)))
(def operations
  " Asocia el número con su respectiva funcion "
  {1 nop
   2 ld
   3 ldi
   4 ct
   5 st
   6 sti
   7 pop
   8 swp
   9 dup
   10 (make-operation +)
   11 (make-operation -)
   12 (make-operation *)
   13 (make-operation quot)
   14 (make-operation rem)
   15 eqz
   16 (make-comparation =)
   17 (make-comparation not=)
   18 (make-comparation <)
   19 (make-comparation <=)
   20 (make-comparation >)
   21 (make-comparation >=)
   22 jp
   23 jpc
   24 jpi
   25 out
   26 chr
   })
(defn execute
  "Ejecuta las operaciones de los opcodes"
  [code size]
  (loop [machine (make-machine code size)]
    (let [{:keys [memory pc sp]} machine
          opcode (memory pc)]
      (if (zero? opcode)
        (println "\nProgram terminated.")
        (if (contains? operations opcode)
          (recur ((operations opcode) machine))
          (throw (ex-info (str "Invalid opcode: " opcode) {})))))))

(def opcodes
  "Asocia opcodes con su respectivo numero de operacion"
  {
   "hlt" 0
   "nop" 1
   "ld" 2
   "ldi" 3
   "ct" 4
   "st" 5
   "sti" 6
   "pop" 7
   "swp" 8
   "dup" 9
   "add" 10
   "sub" 11
   "mul" 12
   "div" 13
   "rem" 14
   "eqz" 15
   "ceq" 16
   "cne" 17
   "clt" 18
   "cle" 19
   "cgt" 20
   "cge" 21
   "jp" 22
   "jpc" 23
   "jpi" 24
   "out" 25
   "chr" 26
   })
(defn get-index[codes labels]
  "Reemplaza las etiquetas definidas en el diccionario con su respectivo valor"
  (loop [codes codes
         nVec []]
    (cond (empty? codes) nVec
          (contains? labels (str (first codes))) (recur (rest codes) (conj nVec (labels (str(first codes)))))
          (contains? opcodes (str (first codes))) (recur (rest codes) (conj nVec (first codes)))
          (number? (first codes)) (recur (rest codes) (conj nVec (first codes)))
          :else (throw (ex-info (str "ERROR - Declare the variable: " (str (first codes))) {})))))
(defn only-opcodes[codes]
  "Elimina los labels, y datas del vector y obtiene el diccionario de las etiquetas definidas junto con su indice en el que se declaro"
  (loop [initialCode codes
         finalCode []
         labels {}]
    (cond
      (empty? initialCode) (get-index finalCode labels)
      (= "label" (str (first initialCode))) (if (contains? labels (str (second initialCode)))
                                              (throw (ex-info (str "ERROR- Double declaration for the variable: " (str (second initialCode))) {}))
                                              (recur (rest (rest initialCode)) finalCode (assoc labels (str (second initialCode)) (count finalCode))))
      (= "data" (str (first initialCode))) (recur (rest (rest initialCode)) (conj finalCode (second initialCode)) labels)
      :else (recur (rest initialCode) (conj finalCode (first initialCode)) labels)
      )))
(defn final-code[codes]
  "Traduce los opcodes del vector por su respectivo numero de operacion"
  (loop [initialCode codes
         finalCode []]
    (cond
      (empty? initialCode) finalCode
      (not (contains? opcodes (str (first initialCode)))) (recur (rest initialCode) (conj finalCode (first initialCode)))
      (or ( = (opcodes (str (first initialCode))) 2) ( = (opcodes (str (first initialCode))) 5) ( = (opcodes (str (first initialCode))) 22) ( = (opcodes (str (first initialCode))) 23) ( = (opcodes (str (first initialCode))) 4)) (if (symbol? (second initialCode))
                                                                                                                                                                                                                                      (throw (ex-info (str "ERROR - Sintax Error near: " (str (first initialCode))) {})) (recur (rest initialCode) (conj finalCode (opcodes (str (first initialCode))))))
      (= (str (first initialCode)) "hlt") (recur (rest initialCode) (conj finalCode (opcodes (str (first initialCode)))))
      :else (if (symbol? (second initialCode))
              (recur (rest initialCode) (conj finalCode (opcodes (str (first initialCode)))))
              (throw (ex-info (str "ERROR- Sintax Error near: " (str (first initialCode))) {}))
              ))))
(as->
  (slurp "Examples/ejem3.von") here
  (clojure.string/replace here #";.*" "")
  (clojure.string/split here #"\s+")
  (remove #(= % "") here)
  (map clojure.edn/read-string here)
  (apply vector here)
  (only-opcodes here)
  (final-code here)
  (execute here 1000))


