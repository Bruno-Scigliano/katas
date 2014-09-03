(ns code-katas-2.core)


(defn unpartial
  "Escribir una funcion que acepte una funcion parcial con cantidad de argumentos desconocida,
   retornar una funcion equivalente de n argumentos"
  [f]
  )


(defn search
  "Dado un numero cualquiera de secuencias, cada una ya ordenada de menor a mayor, encontrar el numero
   mas chico que aparezca en todas las secuencias, las secuencias pueden ser infinitas."
  [& seqs]
  (if (or (some nil? (map first seqs))(= (first (sort(map first seqs))) (last (sort(map first seqs))))) 
  (if (some nil? (map first seqs)) () (first(first seqs)))
  (recur (for [i seqs ](if (= (reduce min (map first seqs))(first i))(drop 1 i)i)))
  ))


 (defn intercalar
  "Escriba una funcion que tome un predicado de 2 argumentos, un valor y una coleccion, y
   retorne una nueva coleccion donde el valor es insertado intercalado cada dos argumentos
   que cumplan el predicado"
  [predicado valor s]
 (lazy-seq
    (if (nil? (second s)) (if (=(count s)0) [] [(first s)])
    (if (predicado (first s) (second s)) (concat [(first s)] [valor] (intercalar predicado valor (rest s)))
      (cons (first s) (intercalar predicado valor (rest s))))))
    
)


(defn tartamudeo
  "Escriba una funcion que retorne una secuencia lazy que comprima el tartamudeo de una secuencia de numeros.
   Comprimir el tartamudeo se refiere a que [1 1 1] se exprese como [3 1] y a su vez [3 1] se exprese como [1 3 1 1].

   La funcion debe aceptar una secuencia inicial de numeros, y devolver una secuencia infinita de compresiones, donde
   cada nuevo elemento es el elemento anterior comprimido."
  [s]
  (defn j [sec cont ant res] (if (empty? sec)
                                   (into res [cont ant])
                                   (if (= (first sec) ant)
                                     (recur (rest sec) (inc cont) (first sec) res)
                                     (recur (rest sec) 1 (first sec) (into res [cont ant])))))
  (lazy-seq (cons (j (rest s) 1 (first s) [] ) (tartamudeo (j (rest s) 1 (first s) []) )))
  )