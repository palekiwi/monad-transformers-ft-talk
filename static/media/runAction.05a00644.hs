-- Running Actions ---------------------------------------------------
runAction :: ActionT -> Request -> Response
runAction action request = either (const "Error") id
                           $ flip runReader request
                           $ runExceptT
                           $ action `catchError` errorHandler

errorHandler :: ActionError -> ActionT
errorHandler err = return $ "Oops: " ++ err
----------------------------------------------------------------------
