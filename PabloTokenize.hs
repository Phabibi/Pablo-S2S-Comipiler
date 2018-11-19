import System.Environment
import System.IO
import Data.Char

data Token = OR | XOR | AND | NEG | Lparen | Rparen | LangleB
 | RangleB | Lbrak | Rbrak | I Int | COLCOL | Lbrace | Rbrace
 | Kernal [Char] | Arrow | Com | BadToken Char | Special [Char] | If | While | Col deriving Show

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
tokenize ('<':more) = LangleB: (tokenize more)
tokenize ('>':more) = RangleB : (tokenize more)
tokenize (',':more) = Com: (tokenize more)
tokenize (':':[]) = [Col] --useless but what only gets hit when theres only 1 thing in the array and its just a col
tokenize ('i':(c:more))
   | isDigit c = let (I n: tokens) = getNumToken (digitToInt c) more
                   in ((I n): tokens)
   | otherwise = (Special "i"):(tokenize (c:more))

tokenize (':':c:more)
   |  c == ':' = getSpecial ("::",more)
   | otherwise = Col: (tokenize (c:more))
tokenize t@(c:more)
   | isDigit c = getNumToken (digitToInt c) more
   | otherwise = getSpecial (span (\c -> elem c "+-*/<>=&|!@#$%?:") t)

-- the specials , not really sure what todo
getSpecial ("::",more) = COLCOL: (tokenize more)
getSpecial ("->",more) = Arrow: (tokenize more)

-- TODO: stuck on these ask
 
-- getSpecial ("while",more) = While: (tokenize more)
-- getSpecial ("if",more) = If: (tokenize more) not too sure how to handle these cases yet
-- getSpecial ("kernal",more)

---
getNumToken accum [] = [I accum]
getNumToken accum (c:more)
   | isDigit c  = getNumToken (accum * 10 + (digitToInt c)) more
   | otherwise  = I accum : (tokenize (c:more))
