module EjerciciosParcial where 

--ejercicio 1

--problema hayPrimosGemelos (d: Z,h: Z) : Bool {
--  requiere: {0 < d ≤ h}
 -- asegura: {res = true <=> existen dos números p1 y p2 contenidos en el rango [d..h] tales que p1 y p2 son primos gemelos}
--}

--Aclaración: Se dice que p1 y p2 son primos gemelos si ambos son primos y además |p2-p1| = 2
--Ejemplo: hayPrimosGemelos 5 7 debe devolver True

hayPrimosGemelos:: Integer -> Integer -> Bool
hayPrimosGemelos x y | recorrerIntervalo x y/=[] && diferencia2(recorrerIntervalo x y) = True
                     | otherwise= False 


--esta funcion devuelve true si solo si existen numeros con diferencia 2
diferencia2::[Integer] -> Bool
diferencia2 [] = False 
diferencia2 [_] = False
diferencia2 (x:y:xs)  | xs == [] = ((y-x) == 2)
                      | y-x == 2  = True
                      | otherwise = diferencia2(y:xs)
        



--con esta funcion recorro intervalo entre los dos numeros ingresados, y devuelvo una lista con todos los primos en ese intervalo
recorrerIntervalo:: Integer -> Integer -> [Integer]
recorrerIntervalo x y  
                       | x>y = []
                       | x <= y  && esPrimo x   = x : recorrerIntervalo (x+1) y
                       | otherwise = recorrerIntervalo (x+1) y


--utilizo la funcion menorDivisor para ver si el menordivisor de ese numero sea el mismo numero, el segundo numero es para iniciar la recorrida, caso contrario devuelvo false
esPrimo:: Integer  -> Bool
esPrimo x  
           | x<=1 = False
           | otherwise =  menorDivisor x (2) == x 



--utilizo esta funcion para encontrar el minimo divisor de un numero sin pensar en 1
menorDivisor:: Integer-> Integer -> Integer
menorDivisor x r | r>1 && x>1 && mod x r == 0 = r
                 | otherwise  = menorDivisor x (r+1)



--ejercicio 2
-- Ejercicio 2 (2 puntos)
--Representaremos un día de cursada de cierta materia con una tupla String x String x Z x Z, donde:

   -- La primera componente de la tupla contiene el nombre de una materia
   -- La segunda componente de la tupla contiene el día de cursada (lunes, martes, etc)
   -- La tercera componente de la tupla contiene el horario de inicio de la cursada de ese día
   -- La cuarta componente de la tupla contiene el horario de fin de la cursada de ese día

--Se pide implementar materiasTurnoTarde, que dada una lista de cursadas devuelva aquellas materias que se cursan en el turno tarde (14 a 17hs)

--problema materiasTurnoTarde (s: seq⟨String x String x Z x Z⟩) :seq⟨String⟩ {
  --requiere: { s[i]1 es alguno de los siguientes valores: "Lunes", "Martes", "Miércoles", "Jueves", "Viernes"}
  --requiere: { s[i]2 ≥ 8 para todo i tal que 0 ≤ i < |s|}
  --requiere: { s[i]3 ≤ 22 para todo i tal que 0 ≤ i < |s|}
  --requiere: { s[i]2 < s[i]3 para todo i tal que 0 ≤ i < |s|}
  --asegura: { res no tiene elementos repetidos}
  --asegura: { res contiene los nombre de todas las materias incluídas en s tales el horario de cursada de dichas materias se superpone (total o parcialmente) con el rango (14..17)}
  --asegura: { res contiene solamente los nombre las materias incluídas en s tales el horario de cursada de dichas materias se superpone (total o parcialmente) con el rango (14..17)}
--}

 --Ejemplo: materiasTurnoTarde [("Introducción a la Programación", "Miércoles", 14, 17)] debe devolver ["Introducción a la Programación"]
    

turnoTarde::[(String,String,Integer,Integer)]
turnoTarde = [ ("Introduccion a la Programacion", "Miercoles", 14, 17),("Algebra","Martes",9,14),("Quimica","Miercoles",7,10)]


materiasTurnoTarde:: [(String,String,Integer,Integer)] -> [String]
materiasTurnoTarde [] = []
materiasTurnoTarde ((nombre,dia,ini,fin):xs) | evaluarDia dia dias && ini >= 8 && fin <= 22 && ini<fin = nombre:materiasTurnoTarde xs
                                             | otherwise = materiasTurnoTarde xs



dias::[String]
dias = ["Lunes","lunes","Martes","martes","Miercoles","miercoles","Jueves","jueves","Viernes","viernes"]
evaluarDia::String->[String]->Bool
evaluarDia _ [] = False
evaluarDia dia [x] = (dia==x)
evaluarDia dia (x:xs) | dia/="" = dia==x 
                      | otherwise = evaluarDia dia xs



--Ejercicio 3 (2 puntos)

--problema maximaSumaDeTresConsecutivos (s: seq⟨Z⟩) : Z {
  --requiere: { |s| ≥ 3}
  --asegura: { res es la suma de tres elementos que se encuentran en posiciones consecutivas de s }
  --asegura: {Para cualquier i en el rango 1 ≤ i < |s|-1, se cumple que s[i-1]+s[i]+s[i+1] ≤ res}
--}

 --Ejemplo: maximaSumaDeTresConsecutivos [3,8,5,4,1] debe devolver 17
    

array::[Integer]
array= [3,8,5,4,1]

maximaSumaDeTresConsecutivos::[Integer] -> Integer
maximaSumaDeTresConsecutivos lista  | longitud lista  > 3 = head (maximoArray(sumade3Consecutivos lista ) )


maximoArray::[Integer] -> [Integer]
maximoArray [] = []  
maximoArray [t] = []  
maximoArray (x:y:xs) | x>y = x:maximoArray (x:xs)
                     |  y>x = y:maximoArray(y:xs)


longitud::[Integer] -> Integer
longitud [] = 0
longitud(x:xs) = 1+ longitud xs

sumade3Consecutivos::[Integer] -> [Integer]
sumade3Consecutivos [] = []
sumade3Consecutivos lista    | longitud lista  >= 3  = hacerSuma lista: sumade3Consecutivos (tail(lista))
                             | otherwise = []

hacerSuma::[Integer] -> Integer
hacerSuma [] = 0
hacerSuma (x:y:z:xs) =x+y+z 



--ejercicio 4
 --Ejercicio 4 (2,5 puntos)

--problema sumaIesimaColumna (matriz: seq⟨seq⟨Integer⟩⟩, col: Integer) : Integer⟩{
  --requiere: {Todos los elementos de la secuencia matriz t [(Int,Int)]  ienen la misma longitud}
  --requiere: {|matriz| > 0}
  --requiere: {|matriz[0]| > 0}
  --requiere: {1 ≤ col ≤ |matriz[0]| }
  --asegura: {res es la sumatoria de los elementos matriz[i][col-1] para todo i tal que 0 ≤ i < |matriz| }
--}

 --Ejemplo: sumaIesimaColumna [[1,2],[3,4]] 1 debe devolver 4
  




 --Ejercicio 5 (0,75 puntos)

--Conteste marcando la opción correcta.
--Sean e1 y e2 dos especificaciones con la misma postcondición. Si la precondición de e1 es más débil que la de e2, entonces:

--conclusiones: e1 y e2 tienen los mismos asegura. 
-- pero el requiere de e1 es debil, sobreespecificacion, osea que hay que pueden haber muchos datos de entrada





-- A. Para que un programa sea correcto respecto de e1, debe considerar mayor cantidad de valores de entrada que un programa que busca satisfacer e2.

-- TRUE


--B.Para que un programa sea correcto respecto de e2, debe considerar mayor cantidad de valores de entrada que un programa que busca satisfacer e1.
--Falso, por que la e2 es mas restrictiva.

--C.No es posible afirmar ninguna de las opciones sin conocer en detalle ambas precondiciones. Falso


--Ejercicio 6 (0,75 puntos)

  --  Conteste marcando la opción correcta.

    --Dado un problema con parámetros x e y, cuya precondición es (x>0 ∨ esPar(y)):
    
    
    --No tiene sentido tener un caso de test con x=0, y=1

    --Todos los casos de test deben tener inputs que cumplan x>0 ∧ esPar(y)

    --Independientemente de la precondición, debo testear todas las combinaciones de valores x e y

