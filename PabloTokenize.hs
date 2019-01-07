
module PabloTokenize where
import System.Environment
import System.IO
import Data.Char

data Token = Num Int| OR | XOR | AND | NEG | Lparen | Rparen | LangleB
 | RangleB | Lbrak | Rbrak | I Int | COLCOL | Lbrace | Rbrace
 | Kernel | Arrow | Com | BadToken Char | Special [Char] | IF | WHILE | Col | Eq deriving Show

tokenize [] = []
tokenize (' ':more) = tokenize more  -- ignore white space
tokenize ('\n':more) = tokenize more  -- ignore white space
tokenize ('|':more) = OR:(tokenize more)
tokenize ('^':more) = XOR: (tokenize more)
tokenize ('&':more) = AND: (tokenize more)
tokenize ('~':more) = NEG: (tokenize more)
tokenize ('(':more) = Lparen: (tokenize more)
tokenize (')':more) = Rparen: (tokenize more)
tokenize ('[':more) = Lbrak: (tokenize more)
tokenize (']':more) = Rbrak: (tokenize more)
tokenize ('{':more) = Lbrace: (tokenize more)
tokenize ('}':more) = Rbrace: (tokenize more)
tokenize (',':more) = Com: (tokenize more)
tokenize ('=':more) = Eq: (tokenize more)
tokenize ('w':'h':'i':'l':'e':more) = WHILE :(tokenize more)
tokenize ('i':'f':more) = IF:(tokenize more)
tokenize ('k':'e':'r':'n':'e':'l':more) = Kernel:(tokenize more)


tokenize ('i':(c:more))
   | isDigit c = let (I n: tokens) = getNumTokenI (digitToInt c) more
                   in ((I n): tokens)
   | otherwise = (Special "i"):(tokenize (c:more))

tokenize t@(c:more)
   | isDigit c = getNumToken (digitToInt c) more
   | isAlpha c = getAlphaNum [c] (span isAlphaNum more)
   | otherwise = getSpecial (span (\c -> elem c "+-*/<>=&|!@#$%?:_") t)


getSpecial ("", (c : more)) = BadToken c : (tokenize more)
getSpecial ("::<",more) = COLCOL:LangleB:(tokenize more)
getSpecial ("::",more) = COLCOL:(tokenize more)
getSpecial (":" , more) = Col: (tokenize more)
getSpecial ("-><",more) = Arrow:LangleB: (tokenize more)
getSpecial ("->",more) = Arrow:(tokenize more)
getSpecial ("<" ,more) = LangleB: (tokenize more)
getSpecial (">" ,more) = RangleB : (tokenize more)
getSpecial (specials, more) = (Special specials) : (tokenize more)
---

--helpers

getNumToken accum [] = [Num accum]
getNumToken accum (c:more)
   | isDigit c  = getNumToken (accum * 10 + (digitToInt c)) more
   | otherwise  = Num accum : (tokenize (c:more))
getNumTokenI accum [] = [I accum]
getNumTokenI accum (c:more)
   | isDigit c  = getNumTokenI (accum * 10 + (digitToInt c)) more
   | otherwise  = I accum : (tokenize (c:more))
getAlphaNum pfx (alphanum, s@('i':c:more))
   | isAlphaNum c   = getAlphaNum (pfx ++ alphanum ++ ['i',c]) (span isAlphaNum more)
   | otherwise      = Special (pfx ++ alphanum) : (tokenize s)
getAlphaNum pfx (alphanum, more) = Special (pfx ++ alphanum) : (tokenize more)
