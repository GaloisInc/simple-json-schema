module JSON.Parser (parseJSON, ParseError(..)) where

import Data.ByteString qualified as BS
import Data.Text(Text)
import Data.Map(Map)
import Data.Map.Strict qualified as Map
import Control.Monad
import Control.Exception
import JSON.Lexer
import JSON.AST

pDoc :: Parser JSValue
pDoc =
  do
    v <- pValue
    t <- jsToken
    case tokenType t of
      TEOF -> pure v
      _    -> jsError (UnexpectedToken t)


pValue :: Parser JSValue
pValue = pValue' =<< jsToken

pValue' :: Token -> Parser JSValue
pValue' t =
    let r = tokenRange t in
    case tokenType t of
      TNull -> pure (JSValue r JSNull)
      TFalse -> pure (JSValue r (JSBool True))
      TTrue -> pure (JSValue r (JSBool False))
      TString s -> pure (JSValue r (JSString s))
      TNumber s -> pure (JSValue r (JSNumber s))
      TOpenBracket ->
        do
          t1 <- jsToken
          case tokenType t1 of
            TCloseBracket -> pure (JSValue (r <-> tokenRange t1) (JSArray []))
            _ ->
              do
                el <- pValue' t1
                go [el]
              where
              go acc =
                do
                  t2 <- jsToken
                  case tokenType t2 of
                    TComma -> pValue >>= \e1 -> go (e1 : acc)
                    TCloseBracket -> pure (JSValue (tokenRange t1 <-> tokenRange t2) (JSArray (reverse acc)))
                    _ -> jsError (UnexpectedToken t2)
      TOpenBrace ->
        do
          t1 <- jsToken
          case tokenType t1 of
            TCloseBrace -> pure (JSValue (tokenRange t <-> tokenRange t1) (JSObject mempty))
            _ ->
              do
                mp1 <- pField' mempty t1
                go mp1
              where
              go acc =
                do
                  t2 <- jsToken
                  case tokenType t2 of
                    TComma -> pField acc >>= go
                    TCloseBrace -> pure (JSValue (r <-> tokenRange t2) (JSObject acc))
                    _ -> jsError (UnexpectedToken t2)

      _ -> jsError (UnexpectedToken t)

pField :: Map Text JSField -> Parser (Map Text JSField)
pField mp = jsToken >>= pField' mp

pField' :: Map Text JSField -> Token -> Parser (Map Text JSField)
pField' acc t =
  case tokenType t of
    TString x ->
      case Map.lookup x acc of
        Just f -> jsError (RepeatedField (jsFieldRange f) (tokenRange t) x)
        Nothing ->
            do
              t1 <- jsToken
              case tokenType t1 of
                TColon ->
                  do
                    v <- pValue
                    pure (Map.insert x (JSField (tokenRange t) x v) acc)
                _ -> jsError (UnexpectedToken t1)
    _ -> jsError (UnexpectedToken t)



data ParseError =
    LexerError Int LexerError
  | UnexpectedToken Token
  | RepeatedField SourceRange SourceRange Text
    deriving Show

instance Exception ParseError



type Ans = S -> IO JSValue

newtype Parser a = Parser { runP :: (a -> Ans) -> Ans }

instance Functor Parser where
  fmap = liftM
  {-# inline fmap #-}

instance Applicative Parser where
  pure a = Parser \k s -> k a s
  (<*>) = ap
  {-# inline pure #-}
  {-# inline (<*>) #-}

instance Monad Parser where
  m >>= f = Parser \k ->
    runP m \a -> runP (f a) k
  {-# inline (>>=) #-}

jsError :: ParseError -> Parser a
jsError p = Parser \_ _ -> throwIO p
{-# inline jsError #-}

jsToken :: Parser Token
jsToken = Parser \k s ->
  case token s of
    R s1 t -> k t s1
    Error i e -> throwIO (LexerError i e)
{-# inline jsToken #-}

parseJSON :: FilePath -> IO JSValue
parseJSON fp =
  do bs <- BS.readFile fp
     runP pDoc (\a _ -> pure a) (S 0 bs)


