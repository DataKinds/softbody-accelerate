module Main where

import Softbody
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.Data.Complex
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Color

hs_new_v = CPU.runN new_v

update_v :: C -> V -> Double -> Double -> V
update_v c v t d = hs_new_v c v (A.fromList A.Z $ [t :+ 0]) (A.fromList A.Z $ [d :+ 0])

---

display_v :: (V, C) -> Picture
display_v (v, c) = nodePictures
  where
    rectangleAt x y = translate x y $ rectangleSolid 10 10
    complexToFloatTuple (real :+ imag) = (realToFrac real, realToFrac imag)
    nodePictures = pictures (map (\(pi, delta, alpha, rho) -> uncurry rectangleAt $ complexToFloatTuple pi) (A.toList v))

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
  
  
  
  
  
  
