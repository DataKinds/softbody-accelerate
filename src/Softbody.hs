{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Softbody where

import Data.Array.Accelerate  as A
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.Data.Functor
import qualified Prelude      as P

type Coord = Complex Double
type Node = (Coord, Coord, Coord, Double)
type V = Vector Node
type C = Matrix Double

-- This is an embedding of a square in the c and v vectors
-- It looks like this:
{-
  0-------------1
  |             |
  |             |
  |             |
  |             |
  |             |
  |             |
  3-------------2
-}

my_c :: C
my_c = fromList (Z:.4:.4) list
  where
    sq2 = 1.41421356237
    list = P.fmap ((P.*) 100.0) [
      0,   1,   sq2, 1,
      1,   0,   1,   sq2,
      sq2, 1,   0,   1,
      1,   sq2, 1,   0
      ]

my_v :: V
my_v = fromList (Z:.4) [
  ((0.0 :+ 0.0), (0.0 :+ 0.0), (0.0 :+ 0.0), 1.0),
  ((10.0 :+ 0.0), (0.0 :+ 0.0), (0.0 :+ 0.0), 1.0),
  ((10.0 :+ (-10.0)), (0.0 :+ 0.0), (0.0 :+ 0.0), 1.0),
  ((0.0 :+ (-100.0)), (0.0 :+ 0.0), (0.0 :+ 0.0), 1.0)
  ]

-------------------------------------------------------------------------------
-- Coordinate geometry

-- specialization of `unlift` for our coord type
-- cunlift :: Exp Coord -> (Exp Double, Exp Double)
-- cunlift = unlift

nunlift :: Exp Node -> (Exp Coord, Exp Coord, Exp Coord, Exp Double)
nunlift = unlift

node_pi :: Exp Node -> Exp Coord
node_pi (nunlift -> (pi, _, _, _)) = pi

node_delta :: Exp Node -> Exp Coord
node_delta (nunlift -> (_, delta, _, _)) = delta

node_alpha :: Exp Node -> Exp Coord
node_alpha (nunlift -> (_, _, alpha, _)) = alpha

node_rho :: Exp Node -> Exp Double
node_rho (nunlift -> (_, _, _, rho)) = rho

--add_coord :: Exp Coord -> Exp Coord -> Exp Coord
--add_coord (cunlift -> (i, j)) (cunlift -> (x, y)) = lift (i + x, j + y)

--sub_coord :: Exp Coord -> Exp Coord -> Exp Coord
--sub_coord (cunlift -> (i, j)) (cunlift -> (x, y)) = lift (i - x, j - y)

--mul_coord :: Exp Double -> Exp Coord -> Exp Coord
--mul_coord x (cunlift -> (i, j)) = lift (i * x, j * x)

--mag_coord :: Exp Coord -> Exp Double
--mag_coord (cunlift -> (i, j)) = sqrt (i * i + j * j)

----------------------------------------------------------------------------------
-- C_offset

c_offset_ij :: Exp Double -- C_ij
            -> Exp Coord  -- pi_i
            -> Exp Coord  -- pi_j
            -> Exp Coord  -- C_offset_ij
c_offset_ij c_ij pi_i pi_j =
  let
    diff_vec = pi_i - pi_j
    scale_factor = (c_ij / (magnitude diff_vec)) 
  in
    (fmap ((*) scale_factor) diff_vec) - diff_vec

make_c_offset :: Acc C -> Acc V -> Acc (Matrix Coord)
make_c_offset c v =
  let
    replaceNaNs = map (\n -> ifThenElse (let mag = magnitude n in isNaN mag || isInfinite mag) (constant $ 0 :+ 0) n)
  in
    replaceNaNs $ imap
      (\sh c_ij ->
         let
           ij = unindex2 sh
           i = fst ij
           j = snd ij
         in
           c_offset_ij c_ij (node_pi $ v !! i) (node_pi $ v !! j)) c

------------------------------------------------------------------------------------
-- new_v_a

-- from https://hackage.haskell.org/package/accelerate-1.2.0.1/docs/Data-Array-Accelerate.html
-- matrix-vector multiplication
mvm :: (Elt a, Num a) => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
mvm mat vec =
  let Z :. rows :. cols = unlift (shape mat) :: Z :. Exp Int :. Exp Int
      vec'              = A.replicate (lift (Z :. rows :. All)) vec
  in
    A.fold (+) 0 ( A.zipWith (*) mat vec' )

new_v_a :: Acc (Matrix Coord) -> Acc V -> Acc (Vector Coord)
new_v_a c_offset v =
  let
    v_rho_recip = map ((\rho_recip -> lift $ rho_recip :+ 0) . (\rho -> 1 / rho) . node_rho) v
  in
    --map (\z -> z * (lift $ magnitude z :+ 0)) c_offset `mvm` v_rho_recip
    c_offset `mvm` v_rho_recip

------------------------------------------------------------------------------------
-- updating V
-- (this includes euler integration on pi, delta)

new_v :: Acc C
      -> Acc V
      -> Acc (Scalar (Complex Double)) -- time
      -> Acc (Scalar (Complex Double)) -- damping
      -> Acc V
new_v c v _t _d =
  let
    t = the _t
    d = the _d
    energyBleed = constant (0.99 :+ 0)
    c_offset = make_c_offset c v
    v_a' = new_v_a c_offset v
  in
    zipWith (\node new_a ->
               let
                 -- Old: new_delta = (node_delta node) + (t * new_alpha)
                 -- New:
                 -- We _need_ a way to bleed off energy, and so friction is the easiest.
                 -- TODO: make energyBleed depend on t
                 -- Other possible methods:
                 -- Deformation (plasticity -- I want to do this!), air resistance (less bleed when moving slower)
                 new_alpha = (node_alpha node) + d * (new_a * t - node_alpha node)
                 new_delta = energyBleed * ((node_delta node) + (t * new_alpha)) -- + (t**2 * new_alpha) 
                 new_pi    = (node_pi node)    + (t * new_delta) -- + (t**4 * new_alpha) 
               in
                 lift (new_pi, new_delta, new_alpha, node_rho node)) v v_a'
    
