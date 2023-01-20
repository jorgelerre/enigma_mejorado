module Types (module Types) where

import qualified Data.Map as Map
import Data.Maybe
import Auxiliars

class TypesWithNormalization b where
    norm :: b -> b        --normalization

-- Tipo de dato usado para Ringstellung
data CharOrInt =  CharacterI Char | IntegerI Int deriving Show

extractCI :: CharOrInt -> Int
extractCI (IntegerI n)    = n - 1
extractCI (CharacterI c)  = charToIndex c alphabet

-- Ringstellung: configuración del rotor antes de insertarlo en la máquina
data Ringstellung = RS CharOrInt CharOrInt CharOrInt CharOrInt deriving Show

extractRS :: Ringstellung -> Int -> Int
extractRS (RS coi4 coi3 coi2 coi1) 1 = extractCI coi1
extractRS (RS coi4 coi3 coi2 coi1) 2 = extractCI coi2
extractRS (RS coi4 coi3 coi2 coi1) 3 = extractCI coi3
extractRS (RS coi4 coi3 coi2 coi1) 4 = extractCI coi4

-- Grundstellung: posición básica de los rotores al inicio de la operación de cifrado. Se obtenía girando
-- cada rotor sobre su eje una vez colocados los rotores en sus posiciones dentro de la máquina. Se podía 
-- hacer viendo a través de una ventanita para cada rotor.
-- c_i: Posicion inicial del rotor i
data Grundstellung = GS Char Char Char Char deriving Show

extractGS :: Grundstellung -> Int -> Char
extractGS (GS c4 c3 c2 c1) 1 = c1
extractGS (GS c4 c3 c2 c1) 2 = c2
extractGS (GS c4 c3 c2 c1) 3 = c3
extractGS (GS c4 c3 c2 c1) 4 = c4

-- Rotores de la maquina: consiste en una serie de rotores que internamente tienen un circuito que conmuta
-- la señal de entrada (codificada por una letra) por otra señal de salida (codificada por letra del 
-- alfabeto interno).
-- rotRing: Anillo del rotor. Corresponde con el alfabeto ordenado.
-- notch: Letra en la cual se indica al siguiente rotor que realice un giro.
-- ringstellung: Configuracion del rotor antes de insertarlo en la maquina.
-- wiring: Correspondencia entre letra de entrada y letra de salida en la ida (yendo hacia el reflector).
-- iwiring: Correspondencia entre letra de entrada y letra de salida en la vuelta (volviendo del reflector).

data Rotor = Rotor String String CharOrInt String String deriving Show

--"Metodos get"
extractRotR :: Rotor -> String
extractRotR (Rotor rotRing notch ringstellung wiring iwiring) = rotRing

extractNotch :: Rotor -> String
extractNotch (Rotor rotRing notch ringstellung wiring iwiring) = notch

extractRSW :: Rotor -> Int
extractRSW (Rotor rotRing notch ringstellung wiring iwiring) = extractCI ringstellung

extractWiring :: Rotor -> String
extractWiring (Rotor rotRing notch ringstellung wiring iwiring) = wiring

extractIWiring :: Rotor -> String
extractIWiring (Rotor rotRing notch ringstellung wiring iwiring) = iwiring

reverseWiring :: Rotor -> String
reverseWiring r = [alphabet !! (charToIndex a w) | a <- alphabet] 
    where w = extractWiring r

-- Coloca un rotor en la posicion pasada como parametro (segun numero o letra mostrada por ventana)
setRSRotor :: Rotor -> CharOrInt -> Rotor
setRSRotor (Rotor rr nt r w iw) (IntegerI n) = Rotor (rotateLev m rr) nt (IntegerI (n-1)) w iw
    where
        m = extractCI (IntegerI n)
setRSRotor r (CharacterI c) = setRSRotor r coi
    where
        coi = IntegerI (1 + extractCI (CharacterI c))

-- Reflector de la maquina: similar a un rotor, pero este se mantiene fijo (no tiene posicion) y se 
-- comunica unicamente con un rotor. Internamente tiene una serie de cables que enlaza sus pines de
-- dos en dos.
-- wiringRef: Salida que devuelve el reflector dada una posicion de entrada.
data Reflector = Reflector String deriving Show

extractWiringRef :: Reflector -> String
extractWiringRef (Reflector s) = s

-- Eleccion y configuracion de rotores de la maquina.
-- r4-r3-r2-r1: Rotores escogidos. La corriente viaja desde el rotor 1 al 4, se refleja en el reflector,
-- y recorre de vuelta desde el rotor 4 al 1 para iluminar el panel.
-- coi4-coi1: Posiciones iniciales de los rotores.
data Walzenlage = W Rotor Rotor Rotor Rotor deriving Show

-- Configura los rotores de la maquina con una posicion inicial
setRSW :: Walzenlage -> Ringstellung -> Walzenlage
setRSW (W r4 r3 r2 r1) (RS coi4 coi3 coi2 coi1) = W w4 w3 w2 w1 
    where
        w4 = setRSRotor r4 coi4
        w3 = setRSRotor r3 coi3
        w2 = setRSRotor r2 coi2
        w1 = setRSRotor r1 coi1

-- Encapsula un rotor y una letra. Este tipo de dato representara la posicion real de los rotores a partir
-- de un rotor recien insertado en la maquina (Rotor) y la letra que se muestra en la muesca (Char).
data RotorGS = RotorGS Rotor Char deriving Show

extractRotorFromRotorGS :: RotorGS -> Rotor
extractRotorFromRotorGS (RotorGS rotor character) = rotor

extractCharFromRotorGS :: RotorGS -> Char
extractCharFromRotorGS (RotorGS rotor character) = character

-- Se incluye a RotorGS a la clase TypesWithNormalization, implementando norm
-- La funcion norm mueve los diferentes los diferentes componentes a la posicion real que tienen en el rotor
instance TypesWithNormalization (RotorGS) where
 norm (RotorGS (Rotor rr n r w iw) c) = RotorGS (Rotor rr' n r w' iw') c
                   where
                     m   = charToIndex c rr     -- Busca la posicion en que se encuentra la letra actual en
                                                -- el alfabeto
                     rr' = rotateLev m rr       -- Rota el anillo para que la letra elegida sea la primera
                     w'  = rotateLev m w        -- Rota w y iw de la misma forma
                     iw' = rotateLev m iw


-- Tipo que aglutina el reflector y los cuatros rotores (en efecto, toda la circuiteria)
data RotorSet =  RotorSet Reflector RotorGS RotorGS RotorGS RotorGS deriving Show
--Metodos get
extractRefRotorSet :: RotorSet -> Reflector
extractRefRotorSet (RotorSet r rd4 rd3 rd2 rd1) = r

extractRGS4RotorSet :: RotorSet -> RotorGS
extractRGS4RotorSet (RotorSet r rd4 rd3 rd2 rd1) = rd4

extractRGS3RotorSet :: RotorSet -> RotorGS
extractRGS3RotorSet (RotorSet r rd4 rd3 rd2 rd1) = rd3

extractRGS2RotorSet :: RotorSet -> RotorGS
extractRGS2RotorSet (RotorSet r rd4 rd3 rd2 rd1) = rd2

extractRGS1RotorSet :: RotorSet -> RotorGS
extractRGS1RotorSet (RotorSet r rd4 rd3 rd2 rd1) = rd1

-- Incluye RotorSet en TypesWithNormalization, incluyendo en RotorSet la normalizacion de sus rotores
instance TypesWithNormalization (RotorSet) where
 norm (RotorSet r r4 r3 r2 r1) = RotorSet r (norm r4) (norm r3) (norm r2) (norm r1)

-- Montaje final con todos los elementos para crear RotorSet (se especifican que rotores se usan, la pos.
-- antes de la insercion de cada uno, la posicion actual y el reflector)
rotorSetup :: Walzenlage -> Ringstellung ->  Grundstellung -> Reflector -> RotorSet
rotorSetup w rs (GS c4 c3 c2 c1) r = norm(RotorSet r rgs4 rgs3 rgs2 rgs1)   --Colocacion de rotores (norm)
       where
         W w4 w3 w2 w1 = setRSW w rs    --Creacion de Walzenlage: eleccion y ordenacion de rotores.
         rgs4 = RotorGS w4 c4           --Montaje de cada tupla Rotor-PosicionActual
         rgs3 = RotorGS w3 c3
         rgs2 = RotorGS w2 c2
         rgs1 = RotorGS w1 c1

-- Pone los rotores en la posicion de comienzo especificada (pasar clave)
resetGrundstellung :: RotorSet ->  Grundstellung -> RotorSet
resetGrundstellung (RotorSet r r4 r3 r2 r1) (GS c4 c3 c2 c1) = norm(RotorSet r rgs4 rgs3 rgs2 rgs1)
    where
        rgs1 = RotorGS (extractRotorFromRotorGS r1) c1
        rgs2 = RotorGS (extractRotorFromRotorGS r2) c2
        rgs3 = RotorGS (extractRotorFromRotorGS r3) c3
        rgs4 = RotorGS (extractRotorFromRotorGS r4) c4

-- Tipo de dato diccionario, con clave y valor tipo char
-- No te haces una idea de la de tiempo que he tardado en darme cuenta de que esto se corresponde a
-- la conmutacion de letras que se puede realizar en la maquina. Hubiera venido bien haber comentado
-- un poco el codigo.
-- Pb = PlugBoard
type Pb = Map.Map Char Char

createPbSetAux :: Int -> String -> Pb
createPbSetAux 0 _  = Map.empty
createPbSetAux _ strng | length strng < 2 = Map.empty
createPbSetAux n (f:s:cs) = Map.insert f s (createPbSetAux (n-1) cs)  

-- Crea un diccionario de, como maximo, 10 tuplas de caracteres a partir de un string, eliminando espacios.
createPbSet :: String -> Pb
createPbSet strng = createPbSetAux 10 cs
            where
             cs = removeWhite(strng)

-- Dado un diccionario, conmuta las parejas clave - valor
invertBijection :: (Ord k, Ord v) => Map.Map k v -> Map.Map v k
invertBijection = Map.foldrWithKey (flip Map.insert) Map.empty

-- Busqueda en el diccionario
-- Realiza la labor del panel de conexiones, conmutando las parejas seleccionadas.
equivalentChar :: Char -> Pb -> Char
equivalentChar c pb
    | isinkey   =  pb Map.! c   --Devuelve el valor correspondiente a la clave
    | isinvalue = ipb Map.! c   --Devuelve la clave correspondiente al valor
    | otherwise = c             --Devuelve el mismo valor que se ha buscado
        where
            isinkey = Map.member c pb    --Devuelve true si encuentra en el diccionario el valor como clave
            isinvalue = Map.member c ipb --Devuelve true si encuentra en el diccionario el valor como valor
            ipb = invertBijection pb     --Diccionario pb invirtiendo parejas clave <-> valor
            
-- Ejercicio 3 - Mejora del codigo de la maquina

-- Introducimos los tipos de dato necesarios en foldable
--instance Foldable RotorSet

