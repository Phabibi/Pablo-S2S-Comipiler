
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


SMALL LISP :
<expression> 	::=	<identifier> | <numeric-atom-expr> | <symbolic-atom-expr> | <list-expr> | <fn-call> | <cond-expr> | <let-expr>
<identifier> 	::=	<id-name:symbolic atom>
<numeric-atom-expr> 	::=	<numeric-val:numeric atom>
<symbolic-atom-expr> 	::=	( quote <symbolic-val:symbolic-atom> )
<list-expr> 	::=	( quote <list-val:list> )
<fn-call> 	::=	( <callee:identifier> <arguments:expression+> )
<cond-expr> 	::=	( cond <clauses:clause+> )
<clause> 	::=	( <predicate:expression> <result:expression> )
<let-expr> 	::=	( let ( <local-defs:local-def+> ) <final-expr:expression> )
<local-def> 	::=	( <local-var:variable> <local-val:expression> )
-}


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

{-Parser-}
type Identifier = [Char]
type INT = Int
data PabloExpression =Pabint INT |Variable Identifier|ElementAt Identifier Int |Literal PabloExpression
 |FuncCall Identifier [PabloExpression]| Not PabloExpression | Group PabloExpression deriving Show

parsePabloEXP (Special s:Lparen:more) = parseFuncCall s [] more
parsePabloEXP (LangleB:more) = parseLit more
parsePabloEXP (Special s:Lbrak:more) = parsePabloVar s more
parsePabloEXP (Special s : more) = Just(Variable s,more)
parsePabloEXP (Num n:more) = Just (Pabint n , more)
--TODO : Not really sure about this section
parsePabloEXP tokens = parseFact tokens --NOTE:  this is actually wrong change it when you asked.

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

parsePrim tokens =
  case parsePabloEXP tokens of
    Just (r , more) -> Just (r , more)
    _->Nothing

{----------------------}
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

parsePab srctxt =
    case parsePabloEXP (tokenize srctxt) of
        Just (e, []) -> Just e
        _ -> Nothing

{--------}
