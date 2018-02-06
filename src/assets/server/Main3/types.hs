type Request = String
type Response = String

type Application = Request -> Response
type Middleware = Application -> Application

type ActionT = Either ActionError Response
type ActionError = String

newtype AppState = AppState { routes :: [Middleware] }
type AppStateT = State AppState
