(ns code-katas-1.core)

(defn filter-odd
  "Escribir una funcion que retorne solamente los numeros impares de
   una secuencia"
  [s]
  (for [i s :when (odd? i)] i )
  )

(defn nil-key
  "Escribir una funcion que dada una clave y un mapa, devuelva true, solamente si el mapa
   contiene una entrada con esa clave, y su valor es nil"
  [k m]
  (if (not (empty? (for [i (keys m) :when (and (nil? (get m i)) (= k i))] true)))
    true
    false)
  )

(defn range
  "Escribir una funcion que cree una lista de enteros en un rango dado.
   Restricciones: range"
  [start end]
  ((fn rec [list end cont] (if(= cont end)
                             list
                             (rec (conj list cont) end (+ 1 cont)))) [] end start))

							 
(defn compress-sequence
  "Escribir una funcion que elimine los duplicados consecutivos
   de una secuencia"
  [s]
  (for [i (range 0 (count s)) :when (if (= i 0) true (not=(nth s i)(nth s (dec i))))]  (nth s i))
)

(defn max-value
  "Escribir una funcion que reciba un numero variable de parametros
   y retorne el que tenga el valor mayor
   Restricciones: max y max-key"
  [& args]
  ((fn rec [list]
     (if (= (count list) 2)
                   (if (< (first list) (second list))
                                              (second list)
                                              (first list))
                   (if (< (first list) (second list))
                     (rec (rest list))
                     (rec (concat [(first list)] (rest(rest list))))     
                 )))args)
  )

(defn split-two
  "Escribir una funcion que parta una secuencia en dos partes
   Restricciones: split-at"
  [length s]
  (list (take length s) (take-last (-(count s) length) s))
  )

(defn inter-two
   "Escribir una funcion que reciba dos secuencias y retorne el primero de cada una,
   luego el segundo de cada una, luego el tercero, etc.
   Restricciones: interleave"
  [s1 s2]
  ((fn rec [list newList] (if (empty? list)
                            newList
                            (rec (rest list)(concat newList  (first list))))
     )
                                  (for [i (range 0 (count s1)) :when (not (or(nil?(get s1 i)) (nil?(get s2 i))))]
                                    [(nth s1 i) (nth s2 i)]) [] 
                                  )
  )

(defn retrieve-caps
  "Escribir una funcion que reciba un string y devuelva un nuevo string conteniendo
   solamente las mayusculas."
  [text]
  (clojure.string/join (for [i text:when (and (= (clojure.string/upper-case i) (str i)) (java.lang.Character/isLetter i))](str i)))
  
  )

(defn find-truth
  "Escribir una funcion que tome un numero variable de booleans, y devuelva true
   solamente si alguno de los parametros son true, pero no todos son true. En otro
   caso debera retornar false"
  [& xs]
    (if (and (some true? xs) (some false? xs))
  true
  false)
  )

(defn zip-map
  "Escribir una funcion que reciba un vector de claves y un vector de valores, y
   construya un mapa a partir de ellos.
   Restricciones: zipmap"
  [k v]
   ((fn maprec [mapa k2 v2](if (and (empty? k2) (empty? v2)) mapa 
                             (maprec (assoc mapa (first k2) (first v2)) (rest k2) (rest v2))))
     {} (if (> (count k)(count v))(rest(reverse k)) (reverse k)) (if (< (count k) (count v)) (rest(reverse v)) (reverse v)))) 
