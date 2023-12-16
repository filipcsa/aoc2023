import Prelude hiding (lookup)
import Data.Set (Set, empty, insert, member, toList, size, fromList)

data Dir = L | R | U | D deriving (Show, Eq, Ord)
type Pos = (Int, Int)
type Beam = (Pos, Dir)
type Layout = [[Char]]

simulateBeams :: Layout -> Set Beam
simulateBeams layout = beams where
  initBeam = ((0, 0), R)
  beams = simulate layout empty [initBeam]

simulate :: Layout -> Set Beam -> [Beam] -> Set Beam
simulate layout beamSet currBeams
  | containsAll currBeams beamSet = beamSet
  | otherwise = simulate layout updatedBeamSet newBeams where
    updatedBeamSet = foldl (flip insert) beamSet currBeams
    newBeams = getNewBeams layout updatedBeamSet currBeams

containsAll :: [Beam] -> Set Beam -> Bool
containsAll beams set = and [ member b set | b <- beams ]

getNewBeams :: Layout -> Set Beam -> [Beam] -> [Beam]
getNewBeams layout set beams = filter (isBeamValid layout set) propagatedBeams where 
  propagatedBeams = concatMap (propagateBeam layout) beams

isBeamValid :: Layout -> Set Beam -> Beam -> Bool
isBeamValid layout set beam = isBeamNew && isBeamInBounds where
  isBeamNew = not $ member beam set
  ((x, y), dir) = beam
  isBeamInBounds = x >= 0 && y >= 0 && x < length layout && y < length (head layout)

propagateBeam :: Layout -> Beam -> [Beam] 
propagateBeam layout beam = propagateBeam' tile beam where 
  ((x, y), dir) = beam
  tile = (layout !! x) !! y

propagateBeam' :: Char -> Beam -> [Beam]
propagateBeam' '.' (pos, R) = [goRight (pos, R)]
propagateBeam' '.' (pos, L) = [goLeft (pos, L)]
propagateBeam' '.' (pos, U) = [goUp (pos, U)]
propagateBeam' '.' (pos, D) = [goDown (pos, D)]

propagateBeam' '\\' (pos, R) = [goDown (pos, R)]
propagateBeam' '\\' (pos, U) = [goLeft (pos, U)]
propagateBeam' '\\' (pos, L) = [goUp (pos, L)]
propagateBeam' '\\' (pos, D) = [goRight (pos, D)]

propagateBeam' '/' (pos, R) = [goUp (pos, R)]
propagateBeam' '/' (pos, U) = [goRight (pos, U)]
propagateBeam' '/' (pos, L) = [goDown (pos, L)]
propagateBeam' '/' (pos, D) = [goLeft (pos, D)]

propagateBeam' '-' (pos, R) = [goRight (pos, R)]
propagateBeam' '-' (pos, L) = [goLeft (pos, L)]
propagateBeam' '-' (pos, D) = [goLeft (pos, D), goRight (pos, D)]
propagateBeam' '-' (pos, U) = [goLeft (pos, U), goRight (pos, U)]

propagateBeam' '|' (pos, D) = [goDown (pos, D)]
propagateBeam' '|' (pos, U) = [goUp (pos, U)]
propagateBeam' '|' (pos, L) = [goUp (pos, L), goDown (pos, L)]
propagateBeam' '|' (pos, R) = [goUp (pos, R), goDown (pos, R)]

goRight :: Beam -> Beam
goRight ((x,y),_) = ((x, y+1), R)

goLeft :: Beam -> Beam
goLeft ((x,y),_) = ((x, y-1), L)

goUp :: Beam -> Beam
goUp ((x,y),_) = ((x-1, y), U)

goDown :: Beam -> Beam
goDown ((x,y),_) = ((x+1, y), D)

countEnergizedTiles :: Set Beam -> Int
countEnergizedTiles set = numUniquePoss where
  poss = map fst (toList set)
  numUniquePoss = size (fromList poss)

main :: IO ()
main = do
  contents <- getContents
  let layout = lines contents :: Layout
  let beams = simulateBeams layout
  print $ countEnergizedTiles beams