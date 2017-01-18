{-# LANGUAGE OverloadedStrings #-}

module Toml where

import Control.Applicative ((<*>),
                            (*>),
                            (<$),
                            (<$>),
                            (<|>),
                            pure)
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.Text (Parser)
import Data.Scientific (Scientific)
import Data.Text as T
import System.Time

data TValue = TNumber Scientific
            | TBool Bool
            | TDateTime ClockTime
            | TString String
            | TArray [TValue]
            deriving (Eq, Show)

number :: Parser TValue
number = TNumber <$> A.scientific

integer :: Parser Int
integer = read <$> AC.many1 A.digit

bool :: Parser TValue
bool = A.string "true" *> pure (TBool True) <|> A.string "false" *> pure (TBool False)

datetime :: Parser TValue
datetime = do year <- integer
              A.char '-'
              month <- integer
              A.char '-'
              day <- integer
              A.char 'T'
              hour <- integer
              A.char ':'
              minute <- integer
              A.char ':'
              second <- integer
              A.char 'Z'
              return $ TDateTime $ toClockTime $ CalendarTime { ctYear = year
                                                              , ctMonth = toMonth month
                                                              , ctDay = day
                                                              , ctHour = hour
                                                              , ctMin = minute
                                                              , ctSec = second
                                                              , ctTZ = 0
                                                              , ctPicosec = 0 
                                                              , ctWDay = Monday
                                                              , ctYDay = 0
                                                              , ctTZName = ""
                                                              , ctIsDST = False
                                                              }
        where months = [ January, February, March, April, May, June, July, August, September, October, November, December ]
              toMonth m = months !! (m-1)

string :: Parser TValue
string = do A.char '\"'
            chars <- AC.manyTill charOrEscape (A.try $ A.char '\"')
            return $ TString chars
    where charOrEscape = (A.char '\\' *> escape) <|> A.satisfy (`notElem` ("\"\\" :: [Char]))
          escape = AC.choice (Prelude.zipWith decode "nrt\\\"0" ("\n\r\t\\\"\0" :: [Char]))
          decode c r = r <$ A.char c

array :: Parser TValue
array = do A.char '['
           A.skipSpace
           vals <- value `AC.sepBy` (A.skipSpace *> A.string "," *> A.skipSpace)
           A.skipSpace
           A.char '['
           A.skipSpace
           return $ TArray vals

value :: Parser TValue
value = datetime <|> number <|> bool <|> string <|> array
