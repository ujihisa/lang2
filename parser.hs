module Parser where

import qualified Text.Parsec as P
import Control.Applicative ((<|>), (*>), (<*))
import qualified Control.Monad.State as S
import Data.List (intersperse)

data Expr = List [Expr]
  | Call Expr [Expr]
  | Atom String
  | Val Int
  deriving (Show, Read)

join = concat . intersperse " "

formatExpr :: Expr -> String
formatExpr expr = fst $ S.runState (formatExpr' expr) 0

  (x:xs') <- liftDown $ mapM formatExpr' xs
  i <- indent
  case x of
     "begin" -> return $ i ++ "(begin" ++ "\n" ++ concat (intersperse "\n" (map (("  " ++ i) ++) xs')) ++ ")"
     "let" -> return $ "(let (" ++ join (take 2 xs') ++ ")\n" ++ last xs' ++ ")"
     "lambda" -> return $ "lambda " ++ head xs' ++ " " ++ last xs' ++ ")"
     otherwise -> return $ "(" ++ join (x:xs') ++ ")"
formatExpr' (Atom name) = return name
formatExpr' (Val x) = return $ show x
formatExpr' (Call func xs) = do
  func' <- liftDown $ formatExpr' func
  xs' <- liftDown $ mapM formatExpr' xs
  return $ "(" ++ func' ++ " " ++ join xs' ++ ")"

liftDown f = do
  S.modify (+ 2)
  x <- f
  S.modify (subtract 2)
  return x
indent = (`replicate` ' ') `fmap` S.get


main = do
  file <- readFile "sample.lisp"
  let expr = parse file
  print expr
  putStrLn $ formatExpr expr

parse file = either (error . show) id $ P.parse parseExpr "parseExpr" file
parseExpr = do
  P.skipMany P.space
  expr <- parenth (parseBegin <|> parseLambda <|> parseLet)
    <|> parseVal
    <|> parseVar
    <|> parenth parseCall
  return expr

parenth f = P.try $ P.char '(' *> f <* P.char ')'

parseBegin = P.try $ do
  P.string "begin"
  P.skipMany1 P.space
  xs <- parseExpr `P.sepBy1` P.many1 P.space
  return $ List $ Atom "begin" : xs

parseLambda = P.try $ do
  P.string "lambda"
  P.skipMany1 P.space
  names <- parenth $ P.many1 (P.noneOf " )(") `P.sepBy` P.spaces
  P.skipMany1 P.space
  expr <- parseExpr
  return $ List [Atom "lambda", List (map Atom names), expr]

parseLet = P.try $ do
  P.string "let"
  P.skipMany1 P.space
  (name, value) <- parenth $ do
    name <- P.many1 $ P.noneOf " )"
    P.skipMany1 P.space
    value <- parseExpr
    return (name, value)
  P.skipMany1 P.space
  body <- parseExpr
  return $ List [Atom "let", Atom name, value, body]

parseVal = P.try $ do
  n <- P.many1 P.digit
  return $ Val $ read n

parseVar = P.try $ do
  name <- P.many1 $ P.noneOf " )("
  return $ Atom name

parseCall = P.try $ do
  (x:xs) <- parseExpr `P.sepBy1` P.many1 P.space
  return $ Call x xs
