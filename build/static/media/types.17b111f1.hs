type Request = String
type Response = String

type Application = Request -> IO Response
type Middleware = Application -> Application

newtype ActionT a = ActionT { runAT :: ExceptT ActionError
                                      (RT.ReaderT Request
                                      (ST.StateT Response IO)) a }
                            deriving (Functor, Applicative, Monad,
                                      MonadIO, MonadReader Request,MonadState Response, MonadError ActionError)
type ActionError = String

newtype AppState = AppState { routes :: [Middleware] }
type AppStateT = ST.State AppState
