
import qualified Ex01StaticString.StaticString
import qualified Ex02Echo.Echo
import qualified Ex03CaseMatch.CaseMatch
import qualified Ex04StringManipulation.StringManipulation

import qualified System.Environment

main :: IO ()
main = do
  n <- System.Environment.getArgs
  case n of
    ["1"] -> Ex01StaticString.StaticString.main
    ["2"] -> Ex02Echo.Echo.main
    ["3"] -> Ex03CaseMatch.CaseMatch.main
    _     ->
      putStrLn "Invalid exercise number. Provide number as a cli argument."
