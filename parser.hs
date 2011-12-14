module Parser where

import qualified Text.Parsec as P
import Control.Applicative ((<|>), (*>), (<*))
import qualified Control.Monad.State as S
import Data.List (intercalate)

data Expr = List [Expr]
  | Atom String
  | Val Int
  deriving (Show, Read)

formatExpr :: Expr -> String
formatExpr expr = fst $ S.runState (formatExpr' expr) 0
formatExpr' (List xs) = do
  (x:xs') <- liftDown $ mapM formatExpr' xs
  i <- indent
  case x of
     "begin" -> return $ i ++ "(begin" ++ "\n" ++ intercalate "\n" (map (("  " ++ i) ++) xs') ++ ")"
     "let" -> return $ "(let " ++ head xs' ++ "\n" ++ last xs' ++ ")"
     "lambda" -> return $ "(lambda" ++ head xs' ++ "\n" ++ i ++ last xs' ++ ")"
     otherwise -> return $ "(" ++ unwords (x:xs') ++ ")"
formatExpr' (Atom name) = return name
formatExpr' (Val x) = return $ show x

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
  parseList <|> parseVal <|> parseAtom

parseList = List `fmap` parenth (parseExpr `P.sepBy1` P.skipMany1 P.space)
  where parenth f = P.try $ P.char '(' *> f <* P.char ')'

parseVal = P.try $ do
  n <- P.many1 P.digit
  return $ Val $ read n

parseAtom = P.try $ do
  name <- P.many1 $ P.noneOf " )(\n"
  return $ Atom name
