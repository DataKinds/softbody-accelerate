module Main where

import Softbody
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Complex
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Color

hs_new_v = CPU.runN new_v

update_v :: C -> V -> Double -> Double -> V
update_v c v t d = hs_new_v c v (A.fromList A.Z $ [t :+ 0]) (A.fromList A.Z $ [d :+ 0])

---

display_v :: (V, C) -> Picture
display_v (v, c) = pictures [nodePictures, edgePictures]
  where
    complexToFloatTuple (real :+ imag) = (realToFrac real, realToFrac imag)
    nodeList = A.toList v

    rectangleAt x y = translate x y $ rectangleSolid 10 10
    nodePictures = pictures (map (\(pi, delta, alpha, rho) -> uncurry rectangleAt $ complexToFloatTuple pi) nodeList)

    nodePairsWithDistance = zip [(pi1, pi2) | (pi1, _, _, _) <- nodeList, (pi2, _, _, _) <- nodeList] (A.toList c)
    edgeColor n = mixColors 10 n black red
    edgePicture ((pi1, pi2), d) = color (edgeColor . realToFrac . abs $ (magnitude $ pi2 - pi1) - d) (line $ complexToFloatTuple <$> [pi1, pi2])
    edgePictures = pictures (edgePicture <$> nodePairsWithDistance) 

tick_v :: ViewPort -> Float -> (V, C) -> (V, C)
tick_v _ t (v, c) = (update_v c v (realToFrac t * 8) 1, c)

main :: IO ()
main = simulate
  (InWindow "https://aearnus.github.io/" (800, 600) (0, 0))
  (greyN 0.5)
  120
  (my_v, my_c)
  display_v
  tick_v
  
  
  
  
  
  
