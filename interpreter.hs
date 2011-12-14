import qualified Parser as P
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M

main = do
  file <- readFile "sample.lisp"
  let expr = P.parse file
  print expr
  putStrLn $ P.formatExpr expr
  print =<< evaluate expr

evaluate :: P.Expr -> IO Int
evaluate expr = fst `fmap` S.runStateT (evaluate' expr) M.empty
evaluate' (P.Begin []) = error "empty begin -- must not happen"
evaluate' (P.Begin xs) = last `fmap` mapM evaluate' xs
evaluate' (P.Let name val body) = do
  env <- S.get
  let before = M.lookup name env
  after <- evaluate' val
  S.put $ M.insert name after env
  memo <- evaluate' body
  case before of
       Just x -> S.put $ M.insert name x env
       Nothing -> S.put $ M.delete name env
  return memo
evaluate' (P.Call name args) = do
  args' <- mapM evaluate' args
  case name of
       "+" -> funcallArb "+" args'
       "print" -> funcall1 "print" (head args')
       otherwise -> error "not such function"
evaluate' (P.Var name) =
  (maybe noVar return . M.lookup name) =<< S.get
  where noVar = do
          liftIO $ print $ "no var <" ++ name ++ ">"
          return 0
evaluate' (P.Val x) = return x

funcallArb "+" args = return $ sum args
funcall1 "print" x = liftIO $ print x >> return x
