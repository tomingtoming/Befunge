import Befunge
import Befunge.MMap.Befunge
import Befunge.MMap.Field
import System.Environment

main :: IO ()
main = do
  (src:_) <- getArgs
  b <- newMMapBefunge 0 0 1 1024 "befunge.befunge"
  f <- srcMMapField src "befunge.field"
  steps b f
