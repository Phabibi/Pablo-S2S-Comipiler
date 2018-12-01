
module Pabloparser where
import PabloTokenize as T
{-
Implementations of a parser for the Pablo Language for the Parabix.

Author: Parsa Habibi

The language BNF∷

Expression Syntax

<expression> ::=  <term> | <expression> "|" <term> | <expression> "^" <term>
<term> ::=  <factor> | <term> "&" <factor>
<factor> ::= <primitive> | "~" <primitive> | "(" <primitive ")"
<primitive> ::= <literal> | <variable> | <function-call>
<literal> ::= <int> | "<" <int> ">"
<variable> ::= <identifier> | <identifier> "[" <int> "]"
<function-call> ::= <identifier> "(" <expression> {"," <expression>} ")"
<int> = [1-9][0-9]*

Statements

<block> ::= {<statement>}
<statement> ::= <assignment> | <if> | <while>
<assignment> ::= <variable> "=" <expression>
<if> ::= "if" <expression> ":" <block>
<while> ::= "while" <expression> ":" <block>

Types

<type> ::= <integer-type> | <stream-type> | <stream-set-type>
<integer-type> ::= "i" <int>
<stream_type> ::= "<" <integer-type> ">"
<stream_set-type> ::= <stream_type> "[" <integer> "]"

Kernels

<kernel> ::= "kernel" <identifier> "::" <signature> "{" block "}"
<signature> ::= <parameter_list> "->" <parameter_list>
<parameter_list> ::= [<parameter_spec> {"," <parameter_spec>} ]
<parameter_spec> ::= <type> <identifier> | <type> "(" <identifier> {"," <identifier>} ")"

{-Expression Syntax

<expression> ::=  <term> | <expression> "|" <term> | <expression> "^" <term>
<term> ::=  <factor> | <term> "&" <factor>
<factor> ::= <primitive> | "~" <primitive> | "(" <primitive ")"
<primitive> ::= <literal> | <variable> | <function-call>
<literal> ::= <int> | "<" <int> ">"
<variable> ::= <identifier> | <identifier> "[" <int> "]"
<function-call> ::= <identifier> "(" <expression> {"," <expression>} ")"
<int> = [1-9][0-9]*
-}
-}

{-Parser-}
type Identifier = [Char]
type INT = Int
data PabloExpression =Pabint INT |Variable Identifier|ElementAt Identifier Int |Literal PabloExpression
 |FuncCall Identifier [PabloExpression]| Not PabloExpression | Group PabloExpression | And PabloExpression PabloExpression
 | Or PabloExpression PabloExpression | Xor  PabloExpression PabloExpression deriving Show

parsePabloEXP tokens =
  case parseTerm tokens of
    Just(p ,  more) ->  extendExpr(p,  more)
    _-> Nothing
extendExpr(p1,T.OR:tokens) =
  case parseTerm tokens of
    Just(p2 , more) -> extendExpr(Or p1 p2 , more)
    _->Nothing

extendExpr(p1,T.XOR:tokens) =
  case parseTerm tokens of
    Just(p2 , more) -> extendExpr(Xor p1 p2 , more)
    _->Nothing

extendExpr(p , more) = Just (p , more)

parseTerm tokens =
  case parseFact tokens of
    Just(p , more) -> extendTerm(p,more)
    _-> Nothing
extendTerm(p , T.AND:tokens) =
  case parseFact tokens of
    Just (p2 , more) ->  extendTerm (And p p2, more)
    _-> Nothing
extendTerm (p , more) = Just (p , more)

parseFact (Lparen:tokens) =
  case parsePrim tokens of
    Just(e ,Rparen:more)->Just(Group e , more)
    _-> Nothing

parseFact (T.NEG:tokens) =
  case parsePrim tokens of
    Just(e ,more)->Just(Not e , more)
    _-> Nothing

parseFact tokens =
  case  parsePrim tokens of
    Just (e, more) -> Just (e , more)
    _-> Nothing

parsePrim (Special s:Lparen:more) = parseFuncCall s [] more
parsePrim  (LangleB:more) = parseLit more
parsePrim (Special s:Lbrak:more) = parsePabloVar s more
parsePrim (Special s : more) = Just(Variable s,more)
parsePrim (Num n:more) = Just (Pabint n , more)
parsePrim _ =Nothing
parsePabloVar s tokens =
  case parsePabloEXP tokens of
    Just (n , Rbrak:more) -> Just( ElementAt  s (getPabint n) , more)
    Just (n , more) ->  Just (Variable s, more)
    _-> Nothing

getPabint (Pabint n) =  n

parseFuncCall s args token =
  case parsePabloEXP token of
    Just(e,(Com:yet_more)) -> parseFuncCall s (args++[e]) yet_more
    Just(e,(Rparen: yet_more)) -> Just (FuncCall s (args++[e]), yet_more)
    _->Nothing

parseLit token =
  case parsePabloEXP token of
    Just (num, RangleB:more) -> Just (Literal num , more)
    Just (num, more) -> Just (Literal num , more)
    _->Nothing

{--------}

{-Parse Statements-}
-- <block> ::= {<statement>}
-- <statement> ::= <assignment> | <if> | <while>
-- <assignment> ::= <variable> "=" <expression>
-- <if> ::= "if" <expression> ":" <block>
-- <while> ::= "while" <expression> ":" <block>


data PabloStatement = Block [PabloStatement] | If PabloExpression PabloStatement| While PabloExpression PabloStatement
 | Equality PabloExpression PabloExpression deriving Show

parsePabloBlock tokens =
  case parsePabloStatement tokens of
    Just (s , more) -> extendBlock (s , more)
    _-> Nothing

extendBlock (p , []) = Just (p , [])
extendBlock (p , tokens) =
  case parsePabloStatement tokens of
    Just (p2 , more) -> extendBlock (extBLCK p p2, more)
    _-> Nothing
extBLCK (Block a) b  = Block (a++[b])
extBLCK a b = Block [a,b]

parsePabloStatement (WHILE:tokens) = parsePabloWhileStatement tokens
parsePabloStatement (IF:tokens) = parsePabloIfStatement tokens
parsePabloStatement tokens = parsePabloAssignment tokens

parsePabloAssignment tokens =
  case parsePrim tokens of
    Just (v , Eq:more) -> case parsePabloEXP  more of
      Just (e, more_t) -> Just (Equality v e , more_t)
      _->Nothing
    _-> Nothing

parsePabloWhileStatement tokens =
  case parsePabloEXP(tokens) of
    Just(p , Col:more) -> case parsePabloBlock more of
      Just (p2 ,more_t) -> Just (While p p2 , more_t)
      _-> Nothing
    _-> Nothing

parsePabloIfStatement tokens =
  case parsePabloEXP(tokens) of
    Just(p , Col:more) -> case parsePabloBlock more of
      Just (p2 ,more_t) -> Just (If p p2 , more_t)
      _-> Nothing
    _-> Nothing

{-----------------}

-- TYPES
--
-- <TYPE> ::= <INTEGER-TYPE> | <STREAM-TYPE> | <STREAM-SET-TYPE>
-- <INTEGER-TYPE> ::= "I" <INT>
-- <STREAM_TYPE> ::= "<" <INTEGER-TYPE> ">"
-- <STREAM_SET-TYPE> ::= <STREAM_TYPE> "[" <INTEGER> "]"

data PabloType = IntType INT | StreaMtype PabloType | StreaMSet PabloType PabloExpression deriving Show

parsePabloType (LangleB:more) = parsePabloStreaMtype (more)
parsePabloType (I n: more) = parseIntType (I n : more)
parsePabloType _ = Nothing

parsePabloStreaMtype tokens =
  case parseIntType tokens of
    Just (p , RangleB:Lbrak:Num n:more) -> case parsePrim (Num n:more) of
      Just (n1 ,Rbrak:more_t) -> Just (StreaMSet p n1 , more_t)
      _-> Nothing
    Just (p , RangleB:more) -> Just (StreaMtype p , more)
    _-> Nothing

parseIntType (I n:tokens) = Just (IntType n, tokens)
parseIntType _= Nothing

{--------------}

-- Kernels
--
-- <kernel> ::= "kernel" <identifier> "::" <signature> "{" block "}"
-- <signature> ::= <parameter_list> "->" <parameter_list>
-- <parameter_list> ::= [<parameter_spec> {"," <parameter_spec>} ]
-- <parameter_spec> ::= <type> <identifier> | <type> "(" <identifier> {"," <identifier>} ")
data PabloKernel = PKernel Identifier PabloKernel PabloStatement | Signature2 PabloKernel PabloKernel | Signature1 PabloKernel
 | ParamL PabloKernel [PabloKernel] | ParamSpec PabloType Identifier | ParamSpecL PabloType Identifier [Identifier] deriving Show

parsePabloKernel (Kernel:Special s:COLCOL:tokens) =
  case parsePabloSignature tokens of
    Just (ps , Lbrace:more) -> case parsePabloBlock (init more) of
      Just (pb , more) -> Just (PKernel s ps pb , more)
      _->Nothing
    _->Nothing

parsePabloSignature tokens =
  case parsePabloParamL tokens of
    Just(pl , Arrow:more) -> case parsePabloParamL more of
      Just(pl2 , more) -> Just(Signature2 pl pl2 , more)
      _-> Nothing
    Just(pl , more) -> Just(Signature1 pl, more )
    _ -> Nothing
parsePabloParamL tokens =
  case parsePabloParamSpec tokens of
    Just (sp ,Com:more) -> extendParamL sp [] more
    Just (sp , more) -> Just(ParamL sp [] , more)
    _-> Nothing
extendParamL sp pabk tokens =
  case parsePabloParamSpec(tokens) of
    Just (sp2 , Com:more_t) -> extendParamL sp (pabk++[sp2]) more_t
    Just (sp2, more_t) -> Just(ParamL sp (pabk++[sp2]), more_t )
    _->Nothing

parsePabloParamSpec tokens =
  case parsePabloType tokens of
    Just (p, Special s:Lparen:more) -> spechelper p s [] more
    Just (p , Special s:more) -> Just (ParamSpec p s, more)
    _-> Nothing
spechelper p s args more =
  case parsePrim more of
    Just (e ,(Com:yet_more)) -> spechelper p s (args++[paramIdent(e)]) yet_more
    Just (e , (Rparen: yet_more)) -> Just(ParamSpecL p s (args++[paramIdent(e)]) , yet_more)
    _ -> Nothing

paramIdent (Variable s) = s

-- Finally...

parsePab srctxt =
  case parsePabloKernel (tokenize srctxt) of
    Just (e,[]) -> Just e
    _ -> Nothing
