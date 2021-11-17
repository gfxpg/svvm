module TestHelpers where

import System.Process (callCommand)

openDot :: String -> String -> IO ()
openDot name dot = do
  let tmpin = "/tmp/svvm-test-" <> name <> ".dot"
      tmpout = tmpin <> ".svg"
  writeFile tmpin dot
  callCommand $ "dot -Tsvg " <> tmpin <> " -o " <> tmpout <> " && xdg-open " <> tmpout
