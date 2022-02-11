{-# LANGUAGE CPP, FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 702 &&  __GLASGOW_HASKELL__ < 902
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Char
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Commonly used character parsers.
--
-----------------------------------------------------------------------------

module Text.Parsec.Char where

import Data.Char
import Text.Parsec.Pos
import Text.Parsec.Prim
#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((*>))
#endif
#if MIN_VERSION_base(4,16,0)
import GHC.Types (type (@), Total)
#endif

-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
--
-- >   vowel  = oneOf "aeiou"

oneOf :: (
#if MIN_VERSION_base(4,16,0)
        Total m,
#endif
  Stream s m Char) => [Char] -> ParsecT s u m Char
{-# INLINABLE oneOf #-}
oneOf cs            = satisfy (\c -> elem c cs)

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"

noneOf :: (
#if MIN_VERSION_base(4,16,0)
        Total m,
#endif
  Stream s m Char) => [Char] -> ParsecT s u m Char
{-# INLINABLE noneOf #-}
noneOf cs           = satisfy (\c -> not (elem c cs))

-- | Skips /zero/ or more white space characters. See also 'skipMany'.

spaces :: (
#if MIN_VERSION_base(4,16,0)
        Total m,
#endif
  Stream s m Char) => ParsecT s u m ()
{-# INLINABLE spaces #-}
spaces              = skipMany space        <?> "white space"

-- | Parses a white space character (any character which satisfies 'isSpace')
-- Returns the parsed character.

space :: (
#if MIN_VERSION_base(4,16,0)
        Total m,
#endif
  Stream s m Char) => ParsecT s u m Char
{-# INLINABLE space #-}
space               = satisfy isSpace       <?> "space"

-- | Parses a newline character (\'\\n\'). Returns a newline character.

newline :: (
#if MIN_VERSION_base(4,16,0)
        Total m,
#endif
  Stream s m Char) => ParsecT s u m Char
{-# INLINABLE newline #-}
newline             = char '\n'             <?> "lf new-line"

-- | Parses a carriage return character (\'\\r\') followed by a newline character (\'\\n\').
-- Returns a newline character.

crlf :: (
#if MIN_VERSION_base(4,16,0)
        Total m, ParsecT s u m @ (Char -> Char),
#endif
  Stream s m Char) => ParsecT s u m Char
{-# INLINABLE crlf #-}
crlf                = char '\r' *> char '\n' <?> "crlf new-line"

-- | Parses a CRLF (see 'crlf') or LF (see 'newline') end-of-line.
-- Returns a newline character (\'\\n\').
--
-- > endOfLine = newline <|> crlf
--

endOfLine :: (
#if MIN_VERSION_base(4,16,0)
        Total m, ParsecT s u m @ (Char -> Char),
#endif
  Stream s m Char) => ParsecT s u m Char
{-# INLINABLE endOfLine #-}
endOfLine           = newline <|> crlf       <?> "new-line"

-- | Parses a tab character (\'\\t\'). Returns a tab character.

tab :: (
#if MIN_VERSION_base(4,16,0)
        Total m,
#endif
  Stream s m Char) => ParsecT s u m Char
{-# INLINABLE tab #-}
tab                 = char '\t'             <?> "tab"

-- | Parses an upper case letter (according to 'isUpper').
-- Returns the parsed character.

upper :: (
#if MIN_VERSION_base(4,16,0)
        Total m,
#endif
  Stream s m Char) => ParsecT s u m Char
{-# INLINABLE upper #-}
upper               = satisfy isUpper       <?> "uppercase letter"

-- | Parses a lower case character (according to 'isLower').
-- Returns the parsed character.

lower :: (
#if MIN_VERSION_base(4,16,0)
        Total m,
#endif
  Stream s m Char) => ParsecT s u m Char
{-# INLINABLE lower #-}
lower               = satisfy isLower       <?> "lowercase letter"

-- | Parses a alphabetic or numeric Unicode characters
-- according to 'isAlphaNum'. Returns the parsed character.
--
-- Note that numeric digits outside the ASCII range (such as arabic-indic digits like e.g. \"٤\" or @U+0664@),
-- as well as numeric characters which aren't digits, are parsed by this function
-- but not by 'digit'.

alphaNum :: (
#if MIN_VERSION_base(4,16,0)
            Total m,
#endif
  Stream s m Char) => ParsecT s u m Char
{-# INLINABLE alphaNum #-}
alphaNum            = satisfy isAlphaNum    <?> "letter or digit"

-- | Parses an alphabetic Unicode characters (lower-case, upper-case and title-case letters,
-- plus letters of caseless scripts and modifiers letters according to 'isAlpha').
-- Returns the parsed character.

letter :: (
#if MIN_VERSION_base(4,16,0)
            Total m,
#endif
  Stream s m Char) => ParsecT s u m Char
{-# INLINABLE letter #-}
letter              = satisfy isAlpha       <?> "letter"

-- | Parses an ASCII digit. Returns the parsed character.

digit :: (
#if MIN_VERSION_base(4,16,0)
            Total m,
#endif
  Stream s m Char) => ParsecT s u m Char
{-# INLINABLE digit #-}
digit               = satisfy isDigit       <?> "digit"

-- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
-- \'f\' or \'A\' and \'F\'). Returns the parsed character.

hexDigit :: (
#if MIN_VERSION_base(4,16,0)
            Total m,
#endif
            Stream s m Char) => ParsecT s u m Char
{-# INLINABLE hexDigit #-}
hexDigit            = satisfy isHexDigit    <?> "hexadecimal digit"

-- | Parses an octal digit (a character between \'0\' and \'7\'). Returns
-- the parsed character.

octDigit :: (
#if MIN_VERSION_base(4,16,0)
            Total m,
#endif
  Stream s m Char) => ParsecT s u m Char
{-# INLINABLE octDigit #-}
octDigit            = satisfy isOctDigit    <?> "octal digit"

-- | @char c@ parses a single character @c@. Returns the parsed
-- character (i.e. @c@).
--
-- >  semiColon  = char ';'

char :: (
#if MIN_VERSION_base(4,16,0)
            Total m,
#endif
  Stream s m Char) => Char -> ParsecT s u m Char
{-# INLINABLE char #-}
char c              = satisfy (==c)  <?> show [c]

-- | This parser succeeds for any character. Returns the parsed character.

anyChar :: (
#if MIN_VERSION_base(4,16,0)
            Total m,
#endif
  Stream s m Char) => ParsecT s u m Char
{-# INLINABLE anyChar #-}
anyChar             = satisfy (const True)

-- | The parser @satisfy f@ succeeds for any character for which the
-- supplied function @f@ returns 'True'. Returns the character that is
-- actually parsed.

-- >  digit     = satisfy isDigit
-- >  oneOf cs  = satisfy (\c -> c `elem` cs)

satisfy :: (
#if MIN_VERSION_base(4,16,0)
            Total m,
#endif
  Stream s m Char) => (Char -> Bool) -> ParsecT s u m Char
{-# INLINABLE satisfy #-}
satisfy f           = tokenPrim (\c -> show [c])
                                (\pos c _cs -> updatePosChar pos c)
                                (\c -> if f c then Just c else Nothing)

-- | @string s@ parses a sequence of characters given by @s@. Returns
-- the parsed string (i.e. @s@).
--
-- >  divOrMod    =   string "div"
-- >              <|> string "mod"

string :: (
#if MIN_VERSION_base(4,16,0)
            Total m,
#endif
  Stream s m Char) => String -> ParsecT s u m String
{-# INLINABLE string #-}
string s            = tokens show updatePosString s
