
import qualified Ex01StaticString.StaticString
import qualified Ex02Echo.Echo
import qualified Ex03CaseMatch.CaseMatch
import qualified Ex04StringManipulation.StringManipulation
import qualified Ex05OnoffSwitch.OnoffSwitch

import qualified System.Environment

main :: IO ()
main = do
  n <- System.Environment.getArgs
  case n of
    ["01"] -> Ex01StaticString.StaticString.main
    ["02"] -> Ex02Echo.Echo.main
    ["03"] -> Ex03CaseMatch.CaseMatch.main
    ["04"] -> Ex04StringManipulation.StringManipulation.main
    ["05"] -> Ex05OnoffSwitch.OnoffSwitch.main
    _      ->
      putStrLn "Invalid exercise number. Provide number as a cli argument."
