
import qualified Ex01StaticString.StaticString
import qualified Ex02Echo.Echo
import qualified Ex03CaseMatch.CaseMatch
import qualified Ex04StringManipulation.StringManipulation
import qualified Ex05OnoffSwitch.OnoffSwitch
import qualified Ex06Counter.Counter
import qualified Ex07ShoppingCart.ShoppingCart
import qualified Ex08ShoppingCartV2.ShoppingCartV2
import qualified TodoMVC.Backend

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
    ["06"] -> Ex06Counter.Counter.main
    ["07"] -> Ex07ShoppingCart.ShoppingCart.main
    ["08"] -> Ex08ShoppingCartV2.ShoppingCartV2.main

    -- Extra exercises
    ["todomvc"] -> TodoMVC.Backend.main

    _ ->
      putStrLn "Invalid exercise number. Provide number as a cli argument."
