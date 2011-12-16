import qualified Parser as P
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import Control.Monad (forM_)
import Control.Applicative ((<|>))
import Data.Maybe (fromJust) -- temporary

data Value = IntValue Int
  | Funcref String ([Value] -> IO Value)
  | LambdaValue Lambda
  | Undefined
instance Show Value where
  show (IntValue i) = show i
  show (Funcref name _) = "<funcref " ++ name ++ ">"
  show (LambdaValue (Lambda lambdaId params expr)) = "<lambda " ++ show lambdaId ++ " " ++ show params ++ " " ++ show expr ++ ">"
  show Undefined = "undefined"
data Lambda = Lambda Int [String] P.Expr deriving Show
instance Eq Lambda where
  (Lambda a _ _) == (Lambda b _ _) = a == b
instance Ord Lambda where
  (Lambda a _ _) < (Lambda b _ _) = a < b
type Env = M.Map String Value

main :: IO ()
main = do
  file <- readFile "sample2.lisp"
  let expr = P.parse file
  -- print expr
  -- putStrLn $ P.formatExpr expr
  --print =<< evaluate expr
  evaluate expr
  return ()

initialState :: ([Lambda], M.Map [Lambda] Env)
initialState = ([], M.insert [] M.empty M.empty)

getEnv :: [Lambda] -> S.StateT ([Lambda], M.Map [Lambda] Env) IO Env
getEnv lambda = do
  x <- snd `fmap` S.get
  return $ fromJust $ M.lookup lambda x

evaluate :: P.Expr -> IO Value
evaluate expr = fst `fmap` S.runStateT (evaluate' expr) initialState

evaluate' :: P.Expr -> S.StateT ([Lambda], M.Map [Lambda] Env) IO Value
evaluate' (P.Atom "print") = return $ Funcref "print" builtinPrint
evaluate' (P.Atom "+") = return $ Funcref "+" builtinPlus
evaluate' (P.Atom "-") = return $ Funcref "-" builtinMinus
evaluate' (P.Atom "==") = return $ Funcref "-" builtinComp
evaluate' (P.Atom name) = do
  result <- varLookup' name
  case result of
       Just x -> return x
       Nothing -> do
         liftIO $ print $ "no variable <" ++ name ++ ">"
         return $ Undefined
  where
    varLookup' :: String -> S.StateT ([Lambda], M.Map [Lambda] Env) IO (Maybe Value)
    varLookup' name = do
      closure <- fst `fmap` S.get
      env <- getEnv closure
      case M.lookup name env of
           Just x -> return $ Just x
           Nothing -> do
             env <- getEnv $ tail closure
             return $ M.lookup name env
    --varLookup' name = (foldl (<|>) Nothing . map (M.lookup name)) `fmap` S.get

evaluate' (P.Val x) = return $ IntValue x
evaluate' (P.List (P.Atom x : xs))
  | x `elem` ["begin", "let", "define", "lambda", "if", "comment"] = specialForm x xs
evaluate' (P.List (func : args)) = do
  args' <- mapM evaluate' args
  func' <- evaluate' func
  call func' args'

specialForm :: String -> [P.Expr] -> S.StateT ([Lambda], M.Map [Lambda] Env) IO Value
specialForm "begin" [] = error "empty begin -- must not happen"
specialForm "begin" xs = last `fmap` mapM evaluate' xs
specialForm "let" [P.List [P.Atom name, val], body] = do
  return $ IntValue 79
  -- env <- S.get
  -- val' <- evaluate' val
  -- S.put $ M.fromList [(name, val')] : env
  -- memo <- evaluate' body
  -- S.modify tail
  -- return memo
specialForm "let" _ = error "let requires 2 params"
specialForm "comment" _ = return Undefined
specialForm "define" [P.Atom name, val] = do
  val' <- evaluate' val

  (closure, x) <- S.get
  env <- getEnv closure
  --let toplevel = fromJust $ M.lookup [] x
  let env' = M.insert name val' env
  let x' = M.insert closure env' x
  S.put (closure, x')
  --S.put $ M.insert name val' e : env
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
  lambdaId <- getNewLambdaId
  let lambda = Lambda lambdaId (map unVar names) body
  (c, x) <- S.get
  S.put (c, M.insert [lambda] M.empty x)
  return $ LambdaValue $ lambda
  where
    getNewLambdaId :: S.StateT ([Lambda], M.Map [Lambda] Env) IO Int
    getNewLambdaId = do
      return 1 -- FIXME
      -- x <- snd `fmap` S.get
      -- return $ (+ 1) $ maximum $ map (fromLambda . head . fst) $ M.toList x
    fromLambda :: Lambda -> Int
    fromLambda (Lambda lambdaId _ _) = lambdaId
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
call (LambdaValue (Lambda lambdaId params body)) args = do
  --let env' = M.fromList (zip params args) `M.union` head envs
  --closure <- getClosure lmd
  (currentClosure, x) <- S.get
  closure <- getClosureByLID lambdaId
  env <- getEnv closure
  let env' = M.fromList (zip params args) `M.union` env
  let x' = M.insert closure env' x
  --liftIO $ print x'
  S.put (closure, x')
  --S.modify (env' :)
  retval <- evaluate' body
  --x'' <- snd `fmap` S.get
  S.put (currentClosure, x')
  --S.modify tail
  return retval
  where
    getClosureByLID :: Int -> S.StateT ([Lambda], M.Map [Lambda] Env) IO [Lambda]
    getClosureByLID lambdaId = do
      x <- snd `fmap` S.get
      return $ fst $ last $ M.toList x
      --return $ error "123"
      -- x <- snd `fmap` S.get
      -- return $ head $ head $ M.toList $ M.filter (\((Lambda i _ _):_) -> i == lambdaId) x

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
