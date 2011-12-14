import qualified Parser as P
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import Control.Monad (forM_)

data Value = IntValue Int
  | Funcref String ([Value] -> IO Value)
  | Lambda [String] P.Expr (M.Map String Value)
  | Undefined
instance Show Value where
  show (IntValue i) = show i
  show (Funcref name _) = "<funcref " ++ name ++ ">"
  show (Lambda params expr env) = "<lambda " ++ show params ++ " " ++ show expr ++ " " ++ show env ++ ">"
  show Undefined = "undefined"

main = do
  file <- readFile "sample.lisp"
  let expr = P.parse file
  print expr
  putStrLn $ P.formatExpr expr
  print =<< evaluate expr

evaluate :: P.Expr -> IO Value
evaluate expr = fst `fmap` S.runStateT (evaluate' expr) M.empty

evaluate' (P.Atom "print") = return $ Funcref "print" builtinPrint
evaluate' (P.Atom "+") = return $ Funcref "+" builtinPlus
evaluate' (P.Atom "-") = return $ Funcref "-" builtinMinus
evaluate' (P.Atom "==") = return $ Funcref "-" builtinComp
evaluate' (P.Atom name) =
  (maybe noVar return . M.lookup name) =<< S.get
  where noVar = do
          liftIO $ print $ "no variable <" ++ name ++ ">"
          return $ Undefined
evaluate' (P.Val x) = return $ IntValue x
evaluate' (P.List (P.Atom x : xs))
  | x == "begin" || x == "let" || x == "define" || x == "lambda" || x == "if" || x == "comment" = specialForm x xs
evaluate' (P.List (func : args)) = do
  args' <- mapM evaluate' args
  func' <- evaluate' func
  call func' args'

specialForm "begin" [] = error "empty begin -- must not happen"
specialForm "begin" xs = last `fmap` mapM evaluate' xs
specialForm "let" [P.List [P.Atom name, val], body] = do
  env <- S.get
  let before = M.lookup name env
  after <- evaluate' val
  S.put $ M.insert name after env
  memo <- evaluate' body
  case before of
       Just x -> S.put $ M.insert name x env
       Nothing -> S.put $ M.delete name env
  return memo
specialForm "let" _ = error "let requires 2 params"
specialForm "comment" _ = return Undefined
specialForm "define" [P.Atom name, val] = do
  env <- S.get
  val' <- evaluate' val
  S.put $ M.insert name val' env
  return val'
  -- case val' of
  --      Lambda params expr env -> do
  --        let val'' = Lambda params expr $ M.insert name val' env
  --        S.put $ M.insert name val'' env
  --        return val''
  --      otherwise -> do
  --        S.put $ M.insert name val' env
  --        return val'
specialForm "define" _ = error "define requires 2 params"
specialForm "lambda" [P.List names, body] = do
  env <- S.get
  return $ Lambda (map unVar names) body env
  where
    unVar (P.Atom x) = x
    unVar _ = error "omg"
specialForm "lambda" _ = error "lambda requires 2 params"
specialForm "if" [cond, thenE, elseE] = do
  (IntValue cond') <- evaluate' cond
  if cond' /= 0
     then evaluate' thenE
     else evaluate' elseE
specialForm "if" _ = error "if requires 3 params"
specialForm x _ = error x

call (Funcref _ f) args = liftIO $ f args
call (Lambda params body env) args = do
  backup <- S.get
  S.put env
  forM_ (zip params args) $ \(p, a) -> do
    S.modify (M.insert p a)
  retval <- evaluate' body
  S.put backup
  return retval

builtinPrint [x] = do
  print x
  return x
builtinPlus xs = do
  args' <- mapM f xs
  return $ IntValue $ sum args'
  where
    f (IntValue x) = return x
    f x = do
      print $ "<" ++ show x ++ "> isn't int"
      return 0
builtinMinus xs = do
  (IntValue x : IntValue y : []) <- return xs
  return $ IntValue $ x - y
builtinComp xs = do
  (IntValue x : IntValue y : []) <- return xs
  if x == y
     then return $ IntValue 1
     else return $ IntValue 0
