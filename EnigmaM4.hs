module EnigmaM4 (module EnigmaM4) where

import Data.Char
import Auxiliars
import Types
import Rotors

-- Devuelve si el rotor ha llegado a la posicion del notch (momento en que gira el rotor siguiente)
isInPositionToCJump :: RotorGS -> Bool
isInPositionToCJump (RotorGS r c) = c  `elem` (extractNotch r)

-- Gira un rotor un paso
rotateRotorOne :: RotorGS -> RotorGS
rotateRotorOne (RotorGS (Rotor rr n r w iw) c) = RotorGS (Rotor (rotateLevOne rr) n r (rotateLevOne w) (rotateLevOne iw)) d
            where
              d = nextInAlphabet c

-- Se giran los rotores un paso
stepForward :: RotorSet -> RotorSet
stepForward (RotorSet r r4 r3 r2 r1)
  | (not i) && (not ii)   = RotorSet r r4 r3 r2 (rotateRotorOne r1)
  | (not i) && ii         = RotorSet r r4 (rotateRotorOne r3) (rotateRotorOne r2) (rotateRotorOne r1)
  |      i  && (not ii)   = RotorSet r r4 r3 (rotateRotorOne r2) (rotateRotorOne r1)
  |      i  && ii         = RotorSet r r4 (rotateRotorOne r3) (rotateRotorOne r2) (rotateRotorOne r1)
          where
              i   = isInPositionToCJump r1
              ii  = isInPositionToCJump r2

-- Descifra un caracter dada una configuracion de rotores y plugboard
-- Para ello, primero aplica la transformacion definida por el circuito formado por los rotores, considerando
-- cado transicion de letras entre estos con un plegado a derechas. Se sigue este orden:
-- Caracter -> w1 -> w2 -> w3 -> w4 -> r -> iw4 -> iw3 -> iw2 -> iw1 -> PlugBoard -> Caracter Cifrado
processAChar :: Pb -> RotorSet -> Char -> Char
processAChar pb rs c = equivalentChar (foldr t m [(iw1,rs1,gs1),(iw2,rs2,gs2),(iw3,rs3,gs3),(iw4,rs4,gs4),(r,0,0),(w4,rs4,gs4),(w3,rs3,gs3),(w2,rs2,gs2),(w1,rs1,gs1)]) pb
  where
    m   = equivalentChar c pb   --Caracter equivalente a c tras pasar por plugboard
    w1  = extractWiring.extractRotorFromRotorGS.extractRGS1RotorSet $ rs --Wiring de rotor 1
    a1  = extractRSW.extractRotorFromRotorGS.extractRGS1RotorSet $ rs    --Ringstellung (conf. previa a ins.)
    rs1 = 1 + a1    
    gs1 = charToIndexA.extractCharFromRotorGS.extractRGS1RotorSet $ rs
    w2  = extractWiring.extractRotorFromRotorGS.extractRGS2RotorSet $ rs
    a2  = extractRSW.extractRotorFromRotorGS.extractRGS2RotorSet $ rs
    rs2 = 1 + a2
    gs2 = charToIndexA.extractCharFromRotorGS.extractRGS2RotorSet $ rs
    w3  = extractWiring.extractRotorFromRotorGS.extractRGS3RotorSet $ rs
    a3  = extractRSW.extractRotorFromRotorGS.extractRGS3RotorSet $ rs
    rs3 = 1 + a3
    gs3 = charToIndexA.extractCharFromRotorGS.extractRGS3RotorSet $ rs
    w4 = extractWiring.extractRotorFromRotorGS.extractRGS4RotorSet $ rs
    a4 = extractRSW.extractRotorFromRotorGS.extractRGS4RotorSet $ rs
    rs4 = 1 + a4
    gs4 = charToIndexA.extractCharFromRotorGS.extractRGS4RotorSet $ rs
    r   = extractWiringRef.extractRefRotorSet $ rs
    iw4 = extractIWiring.extractRotorFromRotorGS.extractRGS4RotorSet $ rs
    iw3 = extractIWiring.extractRotorFromRotorGS.extractRGS3RotorSet $ rs
    iw2 = extractIWiring.extractRotorFromRotorGS.extractRGS2RotorSet $ rs
    iw1 = extractIWiring.extractRotorFromRotorGS.extractRGS1RotorSet $ rs

-- Funcion que, uniendo processAChar y stepForward, cifra un string con la configuracion dada
encodeTextEnigmaM4 :: RotorSet -> Pb -> String -> String
encodeTextEnigmaM4 rs pb str = zipWith (processAChar pb) (iterate stepForward nrsSf) str
                where
                  nrsSf   = stepForward.norm $ rs

-- Operacion que se aplica en el doblado. Cambia la "letra de entrada" a un rotor por la de "salida"
t :: (String,Int,Int) -> Char -> Char
t (wiring,rs,gs) c = alphabet !! (((charToIndexA (wiring !! (charToIndexA c))) - gs+rs) `mod` 26)


