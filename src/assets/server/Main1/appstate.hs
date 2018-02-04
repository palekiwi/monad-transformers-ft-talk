myApp :: AppStateT ()
myApp = do
  addRoute "one" routeHandler1
  addRoute "two" routeHandler2

myServer :: AppStateT () -> IO ()
myServer myApp = do
  let appState = execState myApp AppState{routes=[]}
  userInputLoop appState

main :: IO ()
main = myServer myApp
