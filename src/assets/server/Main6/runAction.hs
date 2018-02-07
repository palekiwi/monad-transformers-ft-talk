-- Running Actions ---------------------------------------------------
runAction :: ActionT () -> Request -> IO Response
runAction action request = do
  (a,s) <- flip ST.runStateT ""
           $ flip RT.runReaderT request
           $ runExceptT
           $ runAT
           $ action `catchError` errorHandler
  return $ either (const "Error") (const s) a

errorHandler :: ActionError -> ActionT ()
errorHandler err = modify (const $ "Oops: " ++ err)
----------------------------------------------------------------------
