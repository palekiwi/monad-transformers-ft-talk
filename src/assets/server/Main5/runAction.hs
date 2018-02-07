-- Running Actions ---------------------------------------------------
runAction :: ActionT () -> Request -> IO Response
runAction action request = do
  (a,s) <- flip runStateT ""
           $ flip runReaderT request
           $ runExceptT
           $ action `catchError` errorHandler
  return $ either (const "Error") (const s) a

errorHandler :: ActionError -> ActionT ()
errorHandler err = lift . lift $ modify (const $ "Oops: " ++ err)
----------------------------------------------------------------------
