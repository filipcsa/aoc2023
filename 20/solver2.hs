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
type WorldStateRX = (WorldState, Bool)

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

initializeWorldState :: [Module] -> WorldStateRX
initializeWorldState modules = (ws, False) where
  modules' = map (setupConjuction modules) modules
  moduleState = fromList [ (mName $ base m, m) | m <- modules' ]
  ws =  ((0, 0), moduleState, [])

setupConjuction :: [Module] -> Module -> Module
setupConjuction modules (Conjunction bm emptyMap) = Conjunction bm initMap where
  conjunctionName = mName bm
  inputs = [ mName $ base m | m <- modules, conjunctionName `elem` dests (base m)]
  initMap = fromList [(input, Low) | input <- inputs ]
setupConjuction _ m = m

pushButtonUntilRX :: WorldStateRX -> Int
pushButtonUntilRX initState = numPushes where
  states = takeWhile (\(ws, rx) -> not rx) $ iterate pushButton initState
  numPushes = length states

pushButton :: WorldStateRX -> WorldStateRX
pushButton (ws, rx) = propagateSignal pushedStateRX where
  (numSignals, ms, _) = ws
  pushedState = (numSignals, ms, [Signal "button" "broadcaster" Low])
  pushedStateRX = (pushedState, rx)


propagateSignal :: WorldStateRX -> WorldStateRX
propagateSignal ((numSigs, ms, []), _) = ((numSigs, ms, []), False)
propagateSignal ((numSigs, ms, s:ss), rx)
  | isLowPulseToRX s = ((numSigs, ms, []), True)
  | otherwise = propagateSignal (newState, False) where
      newState = processSignal (numSigs, ms, ss) s

isLowPulseToRX :: Signal -> Bool
isLowPulseToRX (Signal _ "rx" Low) = True
isLowPulseToRX _ = False

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

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let modules = parseModules inputLines
  let initWorldState = initializeWorldState modules
  let numPushes = pushButtonUntilRX initWorldState
  print numPushes