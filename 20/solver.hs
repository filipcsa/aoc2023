import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import Data.Map (Map, fromList, lookup, insert, empty, elems)
import Data.Maybe (fromJust)

-- Base module
data Pulse = Low | High deriving (Show, Eq)
data BM = BM { mName :: String, dests :: [String] } deriving Show
data Module = FlipFlop { base :: BM, isOn :: Bool } |
  Conjunction { base :: BM, lastPulses :: Map String Pulse } |
  Broadcaster { base :: BM } | 
  Untyped |
  Button { base :: BM } deriving Show

data Signal = Signal { from :: String, to :: String, pulse :: Pulse } deriving Show
type ModulState = Map String Module
type WorldState = ((Int, Int), ModulState, [Signal])

button :: Module
button = Button (BM "button" ["broadcaster"])

parseModules :: [String] -> [Module]
parseModules lines = button : map parseModule lines

parseModule :: String -> Module
parseModule str = modul where
  [modulStr, destsStr] = splitOn " -> " str
  modul = case head modulStr of
    'b' -> Broadcaster (parseBM modulStr destsStr)
    '%' -> FlipFlop (parseBM (tail modulStr) destsStr) False
    '&' -> Conjunction (parseBM (tail modulStr) destsStr) empty

parseBM :: String -> String -> BM
parseBM mName destsStr = BM  mName dests where
  dests = splitOn ", " destsStr

initializeWorldState :: [Module] -> WorldState
initializeWorldState modules = ((0, 0), moduleState, []) where
  modules' = map (setupConjuction modules) modules
  moduleState = fromList [ (mName $ base m, m) | m <- modules' ]

setupConjuction :: [Module] -> Module -> Module
setupConjuction modules (Conjunction bm emptyMap) = Conjunction bm initMap where
  conjunctionName = mName bm
  inputs = [ mName $ base m | m <- modules, conjunctionName `elem` dests (base m)]
  initMap = fromList [(input, Low) | input <- inputs ]
setupConjuction _ m = m

pushButtonSeveralTimes :: WorldState -> Int -> WorldState
pushButtonSeveralTimes initState numPushes =
  iterate pushButton initState !! numPushes

pushButton :: WorldState -> WorldState
pushButton initState = propagateSignal pushedState where
  (numSignals, ms, _) = initState
  pushedState = (numSignals, ms, [Signal "button" "broadcaster" Low])

propagateSignal :: WorldState -> WorldState
propagateSignal (numSigs, ms, []) = (numSigs, ms, [])
propagateSignal (numSigs, ms, s:ss) = propagateSignal newState where 
  newState = processSignal (numSigs, ms, ss) s


processSignal :: WorldState -> Signal -> WorldState
processSignal (numSigs, ms, ss) signal =
  (updatedNumSigs, updatedModuleState, updatedSignals) where
    (Signal from dest pulse) = signal
    updatedNumSigs = updateNumSigs numSigs pulse
    maybeDestModule = lookup dest ms
    (updatedModule, newSignals) = case maybeDestModule of
      (Just destModule) -> processPulse destModule signal
      Nothing -> (Untyped, [])
    updatedModuleState = case updatedModule of
      Untyped -> ms
      mod -> insert dest mod ms
    updatedSignals = ss ++ newSignals

updateNumSigs :: (Int, Int) -> Pulse -> (Int, Int)
updateNumSigs (ls, hs) Low = (ls+1, hs)
updateNumSigs (ls, hs) High = (ls, hs+1)

processPulse :: Module -> Signal -> (Module, [Signal])
-- broadcaster
processPulse (Broadcaster bm) (Signal f t pulse) = (Broadcaster bm, newPulses) where
  name = mName bm
  newPulses = [ Signal name dest pulse | dest <- dests bm ]

-- flipflop high pulse
processPulse (FlipFlop bm isOn) (Signal _ _ High) = (FlipFlop bm isOn, [])
-- flipflop low pulse
processPulse (FlipFlop bm isOn) (Signal _ _ Low) = (FlipFlop bm newState, newPulses) where
  newState = not isOn
  name = mName bm
  newPulses = if newState
    then [Signal name dest High | dest <- dests bm]
    else [Signal name dest Low  | dest <- dests bm]

-- conjuction
processPulse (Conjunction bm lastPulses) (Signal from _ pulse) =
  (Conjunction bm updatedLastPulses, newSignals) where
    name = mName bm
    updatedLastPulses = insert from pulse lastPulses
    newSignals = if allHigh updatedLastPulses
      then [ Signal name dest Low | dest <- dests bm ]
      else [ Signal name dest High | dest <- dests bm ]

allHigh :: Map String Pulse -> Bool
allHigh mem = all (==High) (elems mem)

pulseProd :: WorldState -> Int 
pulseProd ((ls, hs), _, _) = ls * hs

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let modules = parseModules inputLines
  let initWorldState = initializeWorldState modules
  let finalWorldState = pushButtonSeveralTimes initWorldState 1000
  print $ pulseProd finalWorldState