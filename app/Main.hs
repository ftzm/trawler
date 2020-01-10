import Trawler
import Packets

main :: IO ()
main = do
  print =<< mkPortToProc
  runPackets
