-------------------------------------------------------------------------
--- Some definitions added as an appendix to the main test module
--- generated by CurryCheck.

--- Runs a sequence of property tests. Outputs the messages of the failed tests
--- messages and returns exit status 0 if all tests are successful,
--- otherwise status 1.
runPropertyTests :: Bool -> [IO (Maybe String)] -> IO Int
runPropertyTests withcolor props = do
  failedmsgs <- sequenceIO props >>= return . Maybe.catMaybes
  if null failedmsgs
   then return 0
   else do putStrLn $ (if withcolor then AnsiCodes.red else id) $
                      line ++
                      "\nFAILURES OCCURRED IN SOME TESTS:\n" ++
                      unlines failedmsgs ++ line
           return 1
 where
   line = take 78 (repeat '=')

-------------------------------------------------------------------------