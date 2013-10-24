import Befunge
import Befunge.ForkJoin
import Befunge.MMap.Befunge
import Befunge.MMap.Field
import System.Environment

main :: IO ()
main = do
  (src:_) <- getArgs
  fs <- newForkSet
  b <- newMMapBefunge 0 0 1 1024 "befunge.befunge" fs
  f <- srcMMapField src "befunge.field"
  fire b f
  joinAllWith fs
