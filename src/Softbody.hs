{-# LANGUAGE ViewPatterns #-}

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
my_c = fromList (Z:.4:.4) [
  0,   1,   sq2, 1,
  1,   0,   1,   sq2,
  sq2, 1,   0,   1,
  1,   sq2, 1,   0
  ]
  where
    sq2 = 1.41421356237

my_v :: V
my_v = fromList (Z:.4) [
  ((0.0 :+ 0.0), (0.0 :+ 0.0), (0.0 :+ 0.0), 1.0),
  ((1.0 :+ 0.0), (0.0 :+ 0.0), (0.0 :+ 0.0), 1.0),
  ((1.0 :+ (-1.0)), (0.0 :+ 0.0), (0.0 :+ 0.0), 1.0),
  ((0.0 :+ (-1.0)), (0.0 :+ 0.0), (0.0 :+ 0.0), 1.0)
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
    imap
      (\sh c_ij ->
         let
           ij = unindex2 sh
           i = fst ij
           j = snd ij
         in
           c_offset_ij c_ij (node_pi $ v !! i) (node_pi $ v !! j)) c

------------------------------------------------------------------------------------
-- new_v_a

new_v_a :: Acc (Matrix Coord) -> Acc V -> Acc (Vector Coord)
new_v_a c_offset v =
  let
    v_rho_recip = map ((\rho_recip -> lift $ rho_recip :+ 0) . (\rho -> 1 / rho) . node_rho) v
  in
    -- this is c_offset * v_rho_recip
    fold1 (+) (imap (\sh a -> v_rho_recip !! (indexHead sh)) c_offset)

------------------------------------------------------------------------------------
-- updating V

new_v :: C
      -> V
      -> Double -- time
      -> Double -- damping
      -> Acc V
new_v _c _v _t _d =
  let
    v = use _v
    c = use _c
    t = constant (_t :+ 0)
    d = constant (_d :+ 0)
    c_offset = make_c_offset c v
    v_a' = new_v_a c_offset v
  in
    zipWith (\node new_a -> lift
              (node_pi node,
               node_delta node,
               (node_alpha node) + d * (new_a * t - node_alpha node),
               node_rho node)) v v_a'
    
