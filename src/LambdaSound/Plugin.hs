module LambdaSound.Plugin where

-- import GHC.Plugins
-- import GHC (lookupModule, coreModule)
-- import GHC.Types.TyThing (lookupId)
-- import Data.Maybe (fromJust)
-- import Control.Arrow (second)
-- import LambdaSound.Sound

-- plugin :: Plugin
-- plugin = defaultPlugin
--   { installCoreToDos = corePlugin
--   }

-- corePlugin :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
-- corePlugin _ todos = pure (CoreDoPluginPass "CacheLambdaSound" cachePass:todos)


-- getCacheFunctionBndr :: CoreM CoreBndr
-- getCacheFunctionBndr = do
--   lambdaSoundPlugin <- lookupModule (mkModuleName "LambdaSound.Plugin") Nothing
--   let guts = coreModule 
--   let bindings = flattenBinds (mg_binds guts)
--   pure $ head $ filter (\coreBndr -> "cacheLambdaSound" == showSDocUnsafe (ppr coreBndr)) $ fst <$> bindings

-- cachePass :: ModGuts -> CoreM ModGuts
-- cachePass guts = do
--   pure guts
  -- cacheFunctionBndr <- getCacheFunctionBndr
  -- let bindings = flattenBinds $ mg_binds guts -- modifiedBindings <- mapM modifyBinding bindings
  -- return (guts { mg_binds = mkBigCoreBinds modifiedBindings })
  -- liftIO $ mapM_ (putStrLn . showSDocUnsafe . ppr) (mg_binds guts)
  -- modifiedBindings <- mapM (\(bndr, expr) -> (bndr,) <$> applyCaching expr) bindings
  -- pure (guts { mg_binds = uncurry NonRec <$> modifiedBindings })  
  -- liftIO $ mapM_ (putStrLn . showSDocUnsafe . ppr) filteredBindings
  -- pure guts

-- modifyBinding :: Bind CoreBndr -> CoreM (Bind CoreBndr)
-- modifyBinding bind = do
--   let anns = bindAnn bind
--   if hasDesiredAnnotation anns
--     then do
--       modifiedExpr <- modifyExpression (bindBody bind)
--       return undefined -- (bind { bindBody = modifiedExpr })
--     else return bind

-- modifyExpression :: CoreExpr -> CoreM CoreExpr
-- modifyExpression expr = do
--   -- Modify the expression as needed
--   -- You can use functions like `substExpr` or `rebuildExpr` here
--   return expr

-- applyCaching :: Expr CoreBndr -> CoreM (Expr CoreBndr)
-- applyCaching expr@(App (Var f) arg) = if showSDocUnsafe (ppr f) == "cacheLambdaSound"
--   then pure expr
--     -- let cacheFunction = mkInternalName (getUnique $ mkVarOcc "cacheReplace") (mkVarOcc "cacheReplace") (UnhelpfulSpan UnhelpfulGenerated)
--     -- cfId <- lookupId cacheFunction 
--     -- pure (App (Var cfId) arg)

--     else pure expr
-- applyCaching expr = pure expr

-- cacheReplace :: Sound Pulse -> Sound Pulse
-- cacheReplace = id