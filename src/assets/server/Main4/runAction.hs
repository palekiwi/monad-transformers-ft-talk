-- Running Actions ---------------------------------------------------
runAction :: ActionT () -> Request -> Response
runAction action request = flip execState ""
                           $ flip runReaderT request
                           $ runExceptT
                           $ action `catchError` errorHandler

errorHandler :: ActionError -> ActionT ()
errorHandler err = lift . lift $ modify (const $ "Oops: " ++ err)
----------------------------------------------------------------------
