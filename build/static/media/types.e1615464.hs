type Request = String
type Response = String

type Application = Request -> IO Response
type Middleware = Application -> Application

type ActionT a = ExceptT ActionError (ReaderT Request (StateT Response IO)) a
type ActionError = String

newtype AppState = AppState { routes :: [Middleware] }
type AppStateT = State AppState
