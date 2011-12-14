module Parser where

import qualified Text.Parsec as P
import Control.Applicative ((<|>))
import qualified Control.Monad.State as S
import Data.List (intersperse)

data Expr = Begin [Expr]
  | Let String Expr Expr
  | Call String [Expr]
  | Var String
  | Val Int
  deriving (Show, Read)

formatExpr :: Expr -> String
formatExpr expr = fst $ S.runState (formatExpr' expr) 0
formatExpr' (Begin xs) = do
  xs' <- liftDown $ mapM formatExpr' xs
  i <- indent
  return $ i ++ "(begin\n" ++ concat (intersperse "\n" (map (("  " ++ i) ++) xs')) ++ ")"
formatExpr' (Let name value expr) = do
  value' <- formatExpr' value
  expr' <- liftDown $ formatExpr' expr
  i <- indent
  return $ i ++ "(let (" ++ name ++ " " ++ value' ++ ")\n" ++ expr' ++ ")"
formatExpr' (Var name) = return name
formatExpr' (Val x) = return $ show x
formatExpr' (Call name xs) = do
  xs' <- liftDown $ mapM formatExpr' xs
  return $ "(" ++ name ++ " " ++ concat (intersperse " " xs') ++ ")"

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
  expr <- parseBegin <|> parseLet <|> parseVal <|> parseVar <|> parseCall
  return expr

parseBegin = P.try $ do
  P.string "(begin"
  P.skipMany1 P.space
  xs <- parseExpr `P.sepBy1` P.many1 P.space
  P.char ')'
  return $ Begin xs

parseLet = P.try $ do
  P.string "(let"
  P.skipMany1 P.space
  P.char '('
  name <- P.many1 $ P.noneOf " )"
  P.skipMany1 P.space
  value <- parseExpr
  P.char ')'
  P.skipMany1 P.space
  body <- parseExpr
  P.char ')'
  return $ Let name value body

parseVal = P.try $ do
  n <- P.many1 P.digit
  return $ Val $ read n

parseVar = P.try $ do
  name <- P.many1 $ P.noneOf " )("
  return $ Var name

parseCall = P.try $ do
  P.char '('
  name <- P.many1 $ P.noneOf " )"
  xs <- parseExpr `P.sepBy1` P.many1 P.space
  P.char ')'
  return $ Call name xs

