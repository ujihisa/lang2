import qualified Parser as P
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import Control.Monad (forM_)

data Value = IntValue Int
  | Lambda [String] P.Expr (M.Map String Value)
  | Undefined
  deriving Show

main = do
  file <- readFile "sample.lisp"
  let expr = P.parse file
  print expr
  putStrLn $ P.formatExpr expr
  print =<< evaluate expr

evaluate :: P.Expr -> IO Value
evaluate expr = fst `fmap` S.runStateT (evaluate' expr) M.empty
evaluate' (P.Call func args) = do
  args' <- mapM evaluate' args
  case func of
       (P.Var "+") -> funcallArb "+" args'
       (P.Var "print") -> funcall1 "print" (head args')
       otherwise -> do
         func' <- evaluate' func
         lambdacall func' args'
evaluate' (P.Var name) =
  (maybe noVar return . M.lookup name) =<< S.get
  where noVar = do
          liftIO $ print $ "no var <" ++ name ++ ">"
          return $ Undefined
evaluate' (P.Val x) = return $ IntValue x
evaluate' expr@(P.List (P.Var x : xs)) = specialForm x xs

specialForm "begin" [] = error "empty begin -- must not happen"
specialForm "begin" xs = last `fmap` mapM evaluate' xs
specialForm "let" [P.Var name, val, body] = do
  env <- S.get
  let before = M.lookup name env
  after <- evaluate' val
  S.put $ M.insert name after env
  memo <- evaluate' body
  case before of
       Just x -> S.put $ M.insert name x env
       Nothing -> S.put $ M.delete name env
  return memo
specialForm "let" _ = error "let requires 3 params"
specialForm "lambda" [P.List names, body] = do
  env <- S.get
  return $ Lambda (map unVar names) body env
  where
    unVar (P.Var x) = x
    unVar _ = error "omg"
specialForm "lambda" _ = error "lambda requires 2 params"

funcallArb "+" args = do
  args' <- mapM f args
  return $ IntValue $ sum args'
  where
    f (IntValue x) = return x
    f x = do
      liftIO $ print $ "<" ++ show x ++ "> isn't int"
      return 0
funcall1 "print" x = liftIO $ print x >> return x
lambdacall (Lambda params body env) args = do
  backup <- S.get
  S.put env
  forM_ (zip params args) $ \(p, a) -> do
    S.modify (M.insert p a)
  retval <- evaluate' body
  S.put backup
  return retval
