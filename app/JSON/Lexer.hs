module JSON.Lexer (token, Token(..), TokenType(..), LexerError(..), S(..), R(..), SourceRange(..), (<->)) where

import Data.Word
import Data.Text(Text)
import Data.Text.Encoding qualified as Text
import TextBuilder qualified as B
import Data.ByteString qualified as BS
import Data.Scientific(Scientific, scientific)
import Data.Bits
import PP


data SourceRange = SourceRange !Int !Int
  deriving Show

(<->) :: SourceRange -> SourceRange -> SourceRange
SourceRange x _ <-> SourceRange _ y = SourceRange x y

instance PP SourceRange where
  pp (SourceRange x y) = int x <.> "--" <.> int y

data Token = Token {
  tokenRange :: {-# UNPACK #-} !SourceRange,
  tokenType  :: !TokenType
} deriving Show

data TokenType =
    TNull
  | TFalse
  | TTrue
  | TComma
  | TColon
  | TOpenBracket
  | TCloseBracket
  | TOpenBrace
  | TCloseBrace
  | TString Text
  | TNumber Scientific
  | TEOF
    deriving Show

data LexerError =
    ExpectedTrue
  | ExpectedFalse
  | ExpectedNull
  | ExpectedDigit
  | ExpectedSignOrDigit
  | ExpecetdQuote
  | ExpecetdESC
  | ExpecetdHexDigit
  | InvaludUtf8
  deriving Show


data S = S {-# UNPACK #-} !Int {-# UNPACK #-} !BS.ByteString
  deriving Show

data R = R !S !Token | Error Int LexerError
  deriving Show

isSpace :: Word8 -> Bool
isSpace c =
  case c of
    {- \t  -} 0x09 -> True
    {- \n  -} 0x0A -> True
    {- \r  -} 0x0D -> True
    {- ' ' -} 0x20 -> True
              _    -> False

{-# inline token #-}
token :: S -> R
token (S offset0 bs0) =
  case BS.span isSpace bs0 of
    (as0,bs) ->
      let offset = offset0 + BS.length as0
      in
      case BS.uncons bs of
        Nothing -> R (S offset bs) (Token (SourceRange offset offset) TEOF)
        Just (b, rest) ->
          let next1   = S (offset + 1) rest
              emit1 x = R next1 (Token (SourceRange offset offset) x)
              post x y z =
                let n = BS.length x in
                case BS.splitAt n rest of
                  (as,rest1) | x == as ->
                    R (S (offset + 1 + n) rest1) (Token (SourceRange offset (offset + n)) y)
                  _ -> Error offset z
          in
          case b of  
            {- {   -} 123  -> emit1 TOpenBrace
            {- }   -} 125  -> emit1 TCloseBrace
            {- [   -} 91   -> emit1 TOpenBracket
            {- ]   -} 93   -> emit1 TCloseBracket
            {- ,   -} 44   -> emit1 TComma
            {- :   -} 58   -> emit1 TColon
     
            {- t   -} 116  -> post "rue" TTrue ExpectedTrue
            {- f   -} 102  -> post "alse" TFalse ExpectedFalse
            {- n   -} 110  -> post "ull" TNull ExpectedNull
            
            {- "   -} 34   -> lexString offset next1
    
            {- -   -} 45   -> lexNum next1 \x s1@(S end _) ->
                              R s1 (Token (SourceRange offset (end - 1)) (TNumber (-x)))

                      _    -> lexNum (S offset bs) \x s1@(S end _) ->
                              R s1 (Token (SourceRange offset (end - 1)) (TNumber x))

lexNum :: S -> (Scientific -> S -> R) -> R
lexNum s@(S off _) k =
  lexDec 0 s (Error off ExpectedDigit) \d s1 ->
    if d == 0 then lexFrac 0 0 s1 k
              else lexDigits d s1 \base s2 ->
                   lexFrac base 0 s2 k

lexFrac :: Integer -> Int -> S -> (Scientific -> S -> R) -> R
lexFrac base ex s@(S off bs) k =
  case BS.uncons bs of
    Just ({- . -} 46, xs) ->
      let next = S (off + 1) xs in
      lexDec base next (Error (off + 1) ExpectedDigit) \base1 s1 ->
      lexFracDigits base1 (-1) s1 k
    _ ->
      lexExp s \n s1 ->
      k (scientific base (ex + n)) s1

lexFracDigits :: Integer -> Int -> S -> (Scientific -> S -> R) -> R
lexFracDigits !base !ex s k =
  lexDec base s done \base1 s1 ->
  lexFracDigits base1 (ex - 1) s1 k
  where
  done = lexExp s \e -> k (scientific base (ex + e))
  
lexExp :: S -> (Int -> S -> R) -> R
lexExp s@(S off bs) k =
  case BS.uncons bs of
    Nothing -> k 0 s
    Just (x,xs) ->
      let next = S (off + 1) xs in
      case x of
        {- E -} 101 -> lexExpSign next k
        {- e -} 69  -> lexExpSign next k
                _   -> k 0 s

lexExpSign :: S -> (Int -> S -> R) -> R
lexExpSign s@(S off bs) k =
  case BS.uncons bs of
    Nothing -> Error off ExpectedSignOrDigit
    Just (x,xs) ->
      let next = S (off + 1) xs in
      case x of
        {- + -} 43 -> lexExpDigits1 next k
        {- - -} 45 -> lexExpDigits1 next \n s1 -> k (-n) s1
                _  -> lexExpDigits1 s k

lexExpDigits1 :: S -> (Int -> S -> R) -> R
lexExpDigits1 s@(S off _) k =
  lexDec 0 s (Error off ExpectedDigit) \acc s1 -> lexDigits acc s1 k

lexDigits :: Num n => n -> S -> (n -> S -> R) -> R
lexDigits acc s k =
  lexDec acc s (k acc s) (\acc1 s1 -> lexDigits acc1 s1 k)
{-# SPECIALISE lexDigits :: Int -> S -> (Int -> S -> R) -> R #-}
{-# SPECIALISE lexDigits :: Integer -> S -> (Integer -> S -> R) -> R #-}

lexDec :: Num n => n -> S -> R -> (n -> S -> R) -> R
lexDec acc (S off bs) kNo kYes =
  case BS.uncons bs of
    Just (x,xs)
      | 48 <= x && x <= 57 -> v `seq` kYes v (S (off + 1) xs)
        where v = 10 * acc + fromIntegral (x - 48)
    _ -> kNo 
{-# INLINE lexDec #-}
--------------------------------------------------------------------------------

lexString :: Int -> S -> R
lexString start s =
  lexSimpleChunk s \txt (S off bs) ->
    case BS.uncons bs of
      Nothing -> Error off ExpecetdQuote
      Just (sp,bs2) ->
        case sp of
          34 -> R (S (off + 1) bs2) (Token (SourceRange start off) (TString txt))
          92 -> lexEsc start (B.text txt) (S (off + 1) bs2)
          _  -> error "bug"

lexSimpleChunk :: S -> (Text -> S -> R) -> R
lexSimpleChunk (S off bs) k =
  case BS.span simple bs of
    (as,bs1) -> k (Text.decodeUtf8 as) (S (off + n) bs1)
      where n = BS.length as  
  where
  simple x = x /= 34 && x /= 92
  -- Not " and \


lexStringNonEsc :: Int -> B.TextBuilder -> S -> R
lexStringNonEsc start !out s =
  lexSimpleChunk s \txt s1 ->
  lexStringSpecial start (out <> B.text txt) s1

lexStringSpecial :: Int -> B.TextBuilder -> S -> R
lexStringSpecial start !out (S off bs) =
  case BS.uncons bs of
    Nothing -> Error off ExpecetdQuote
    Just (x,xs) ->
      let next = S (off + 1) xs in
      case x of
        {- " -} 34 -> txt `seq` R next (Token (SourceRange start off) (TString txt))
                    where txt = B.toText out
        {- \ -} 92 -> lexEsc start out next
                _  -> error "bug"
           
lexEsc :: Int -> B.TextBuilder -> S -> R
lexEsc start out (S off bs) =
  case BS.uncons bs of
    Nothing -> Error off ExpecetdESC
    Just (x,rest) ->
      let
        next1 = S (off + 1) rest
        k c = lexStringNonEsc start (out <> B.char c) next1
      in 
      case x of
        {- " -} 34  -> k '"'
        {- \ -} 92  -> k '\\'
        {- / -} 47  -> k '/'
        {- b -} 98  -> k '\b'
        {- f -} 102 -> k '\f'
        {- n -} 110 -> k '\n'
        {- r -} 114 -> k '\r'
        {- t -} 116 -> k '\t'
        {- u -} 117 ->
                      lexHex  0 next1 \v1 next2 ->
                      lexHex v1 next2 \v2 next3 ->
                      lexHex v2 next3 \v3 next4 ->
                      lexHex v3 next4 \v4 next5 ->
                      lexStringNonEsc start (out <> B.unicodeCodepoint v4) next5
                _ -> Error off ExpecetdESC

lexHex :: Int -> S -> (Int -> S -> R) -> R
lexHex acc (S off bs) k =
  case BS.uncons bs of
    Nothing -> Error off ExpecetdHexDigit
    Just (d,rest)
      | 48 <= d && d <= 57  -> digit (d - 48)
      | 97 <= d && d <= 102 -> digit (d - 87)
      | 65 <= d && d <= 70  -> digit (d - 55)
      | otherwise -> Error off ExpecetdHexDigit
      where
      digit v = acc1 `seq` k acc1 (S (off + 1) rest)  
        where acc1 = shiftL acc 4 .|. fromIntegral v 