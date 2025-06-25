module TestEjerciciosParcial where 
import EjerciciosParcial
import Test.HUnit

-- Casos de Test

runtestdivisor = runTestTT testmenorDivisor
runtestprimo = runTestTT testprimo
runrecorrerIntervalo = runTestTT testintervalo
rundiferencia2 = runTestTT testdiferencia2
runprimosgemelos = runTestTT testPrimosGemelos
--ejercicio 1
testmenorDivisor = test[
    " Caso 6" ~: (menorDivisor 6 2) ~?= 2,
    " Caso 11" ~: (menorDivisor 11 1) ~?= 11,
    " Caso 17" ~: (menorDivisor 17 0) ~?= 17,
    " Caso negativo" ~: (menorDivisor (-30) 0) ~?= -30

 ]

testprimo = test[
    "Caso 13" ~: (esPrimo 13) ~?= True,
    "Caso 2" ~: (esPrimo 2)~?= True ,
    "caso 1"~:(esPrimo 1)~?= False,
    "caso negativo"~:(esPrimo (-10))~?= False
 ]

testintervalo = test[
    "Caso numeros iguales" ~: (recorrerIntervalo 5 5) ~?= [5],
    "Caso primero mayor a segundo" ~: (recorrerIntervalo 50 5) ~?= [],
    "Caso correcto" ~: (recorrerIntervalo 2 20) ~?= [2,3,5,7,11,13,17,19]
 ]

testdiferencia2 = test[
    "Caso numeros iguales" ~: (diferencia2 [5,5,5]) ~?= False,
    "Caso lista vacia" ~: (diferencia2 []) ~?= False,
    "Caso lista 1 numero" ~: (diferencia2 [2,3,5,7]) ~?= True,
    "Caso ejemplo consigna" ~: (diferencia2 [5,7]) ~?= True,
    "Caso false" ~: (diferencia2 [19,23]) ~?= False

 ]

testPrimosGemelos = test[
    "caso consigna" ~: (hayPrimosGemelos 5 7) ~?= True,
    "caso mismo numero" ~: (hayPrimosGemelos 7 7) ~?= False,
    "caso x>y" ~: (hayPrimosGemelos 70 7) ~?= False,
    "caso rango con un primo" ~: (hayPrimosGemelos 4 6) ~?= False,
    "caso mas de dos primos gemelos"~: (hayPrimosGemelos 3 7) ~?= True
 ]


--ejercicio2
runevaluardia = runTestTT testevaluardia


testevaluardia = test[
    "caso true" ~: (evaluarDia "lunes") ~?= True,
    "caso false" ~: (evaluarDia "Domingo") ~?= False
 ]