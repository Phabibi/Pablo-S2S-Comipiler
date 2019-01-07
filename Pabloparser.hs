
module Pabloparser where
import PabloTokenize as T
import Data.List
import Data.Char
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

type Block = [PabloStatement]
data PabloStatement = If PabloExpression [PabloStatement]| While PabloExpression [PabloStatement]
 | Equality PabloExpression PabloExpression deriving Show

parsePabloBlock tokens =
  case parsePabloStatement tokens of
    Just (s , more) -> extendBlock ([s] , more)
    _-> Nothing

extendBlock (p , []) = Just (p , [])
extendBlock (p , tokens) =
  case parsePabloStatement tokens of
    Just (p2 , more) -> extendBlock ( p ++ [p2] , more)
    _-> Nothing

parsePabloStatement (WHILE:tokens) = parsePabloWhileStatement tokens
parsePabloStatement (IF:tokens) = parsePabloIfStatement tokens
parsePabloStatement tokens = parsePabloAssignment tokens

parsePabloAssignment tokens =
  case parsePabloEXP tokens of
    Just (Variable v , Eq:more) -> case parsePabloEXP  more of
      Just (e, more_t) -> Just (Equality (Variable v) e , more_t)
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

data PabloType = IntType Int | StreaMtype PabloType | StreaMSet PabloType PabloExpression deriving Show

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
data PabloKernel = PKernel Identifier PabloKernel [PabloStatement] | Signature2 PabloKernel PabloKernel | Signature1 PabloKernel
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
  case parsePabloEXP more of
    Just (e ,(Com:yet_more)) -> spechelper p s (args++[paramIdent(e)]) yet_more
    Just (e , (Rparen: yet_more)) -> Just(ParamSpecL p s (args++[paramIdent(e)]) , yet_more)
    _ -> Nothing

paramIdent (Variable s) = s
showpab (Pabint x) = show x

-- Finally...

parsePab srctxt =
  case parsePabloKernel (tokenize srctxt) of
    Just (e,[]) -> Just e
    _ -> Nothing
 --

 --NOTE : trying to extract names:

showparamSpec (ParamSpec _ id) =  [id]
showparamSpec (ParamSpecL _ id ids) = [id]++ ids
showparamList (ParamL pspec []) = showparamSpec(pspec)
showparamList (ParamL pspec (c:moreps)) = showparamSpec(pspec) ++ showparamList(ParamL c moreps)
showSignature2Input (Signature2 p1 _) = showparamList(p1)
showSignature2Output (Signature2 _ p2) = showparamList(p2)
showSignature2 (Signature2 p1 p2) = showparamList(p1) ++  showparamList(p2)


extractparamtype (IntType a) = []
extractparamspec (ParamSpec (IntType a) _ ) = []   -- we dont need these yet
extractparamspec (ParamSpec (StreaMtype a) _ ) = extractparamtype(a)
extractparamspec (ParamSpec (StreaMSet _ b) _) = showpab(b)
extractparamlist (ParamL a []) = extractparamspec (a) : []
extractparamlist (ParamL a (s:rest))= [extractparamspec (a)] ++ extractparamlist(ParamL s  rest)
extractSignatureInput (Signature2 p1 _) = extractparamlist(p1)
extractSignatureOutput (Signature2 _ p2) = extractparamlist(p2)
-- intercalate (",") (filter (/= "") (fmap (map (\x -> "for (unsigned i = 0; i < " ++ x  ++ "; i++ ) {\n" ++ "pb.createAssign(pb.createExtract(getOutpuStreamVar(" ))
-- zipNamesOut params = filterNamesIn(zip (extractSignatureOutput(params)  showSignature2Output(params))) --error here
-- zipNamesIn  params = filterNamesIn (zip (extractSignatureInput(params)  showSignature2Input(params))) --error here


 {----------------------}


 -- This is a mess .. but it works ..

showKernelHeader(PKernel id params more)  =
 "class " ++ (id) ++ "Kernel final: public pablo::PabloKernel {\n"
 ++ "public:\n" ++"    " ++ (id) ++
 "Kernel(const std::unique_ptr<kernel::KernelBuilder> & b,\n"
 ++ intercalate (",\n") (fmap (map (\x -> "StreamSet*  "++  x  )) showSignature2(params)) ++ ");\n"
 ++ " bool isCachable() const override { return true; } \n"
 ++ " bool hasSignature() const override {return false;}\n"
 ++ " void generatePabloMethod() override;\n"
 ++ "};\n"
 ++ (id)++"Kernel::"++(id)++"Kernel(const std::unique_ptr<kernel::KernelBuilder> & b ,"

 ++  intercalate (",\n") (fmap (map (\x -> "{Binding{"++  show x ++ "," ++ x ++ "}}"  )) showSignature2(params)) ++ ")\n\n"
 ++ "Void" ++ id ++ "Kernel::generatePabloMethod(){\n"
 ++ "PabloBuilder main(getEntryScope());\n"
 ++ intercalate ("\n") (fmap (map (\x -> "std::vector <PabloAST*>" ++ x ++ "= " ++ "getInputStreamSet("++show x ++");"  )) showSignature2Input(params))++"\n"
 ++fmap (\x -> "PabloAST * " ++ x ) head (showSignature2Output(params)) ++ "["++intercalate ("") (filter (/= "")(fmap (map (\x -> x)) head[(extractSignatureOutput(params))])) ++ "]" ++ ";\n"
 ++ intercalate ("\n")  (fmap (map (\x -> "PabloAST * " ++ x   ))  tail (showSignature2Output(params)))++"\n"
 ++ printStatements(more , "main")   -- input numbers
 ++ "for (unsigned i = 0 ; i < "
 ++  intercalate (",") (filter (/= "")(fmap (map (\x -> x)) extractSignatureOutput(params)))
 ++ ";i++) {\n pb.createAssign(pb.createExtract(getOutputStreamVar("++ (fmap (\x ->show x ) head (showSignature2Output(params)))++")"
 ++ "pb.getInteger(i)), "++ "["++intercalate (",") (filter (/= "")(fmap (map (\x -> x)) head[(extractSignatureOutput(params))])) ++ "]);\n}\n"
 ++ intercalate ("\n") (fmap (map (\x -> "pb.createAssign(pb.createExtract(getOutputStreamVar(" ++ show x ++ ")," ++ "pb.getInteger(0)),"++ x)) tail (showSignature2Output(params)))
 ++"}"

makeEX ((Pabint x),scope) = scope++"."++"getInteger("++ show x++ ")"
makeEX ((Variable a), scope) = a
makeEX ((ElementAt x y),scope) = x ++ scope++"."++"createExtract("++show x++","++show y++")"
makeEX ((Not x),scope )= scope++"."++"createNot("++makeEX (x , scope) ++ ") "
makeEX ((And x y),scope) = scope++"."++"createAnd("++makeEX (x , scope) ++ "," ++ makeEX (y , scope) ++ ")"
makeEX ((Or x y),scope) = scope++"."++"createOr("++ makeEX(x , scope) ++ ","++ makeEX(y,scope) ++")"
makeEX ((Xor x y),scope) = scope++"."++"createXor("++makeEX(x, scope) ++ "," ++ makeEX(y , scope )++")"
makeEX ((Literal d), scope) = scope++"."++"createRepeat("++  showpab(d) ++ ")"
makeEX ((FuncCall x y) , scope)= scope ++ "." ++ "create" ++x ++ "(" ++ init(makeEXarr(y, scope)) ++  ")"

--simple recurssion over the array of expression

makeEXarr ([], scope) = []
makeEXarr ((f:r),scope) =  makeEX(f , scope) ++","++ makeEXarr(r ,  scope)

-- not sure what todo for these
-- makeEX (Group x) scope = x
printStatements ([], currentScope) = []
printStatements((If expr nestedBlock):more,  currentScope) =
  "{\n" ++       -- open C++ scope
  "auto " ++ newScope ++ " = " ++  currentScope ++ ".createScope();\n"++
  currentScope ++ ".createif(" ++ (show expr) ++ "," ++ newScope ++ ");\n" ++
  printStatements(nestedBlock, newScope) ++
  "}\n" ++     -- close C++ scop
  printStatements (more, currentScope)
  where newScope = currentScope ++ "_" ++(show $ length more )


printStatements((While expr nestedBlock):more,  currentScope) =
  "{\n" ++       -- open C++ scope
  "auto " ++ newScope ++ " = " ++  currentScope ++ ".createScope();\n"++
  currentScope ++ ".createWhile(" ++ (show expr) ++ "," ++ newScope ++ ");\n" ++
  printStatements(nestedBlock,  currentScope) ++
  "}\n" ++     -- close C++ scop
  printStatements (more, currentScope)
  where newScope =  currentScope ++"_" ++( show $ length more)

printStatements ((Equality  (Variable a ) b):more, currentScope) =
  "{\n" ++       -- open C++ scope
  "auto " ++ a ++ " = " ++  currentScope ++ ".createVar(" ++ show a ++ ");\n"++
  currentScope ++ ".createAssign( "
  ++ a
  ++ ","  ++ makeEX(b , currentScope)
  ++ ");\n}\n"      -- close C++ scopprintStatements (more, currentScope)
  ++ printStatements (more, currentScope)
  where newScope =  currentScope  ++ "_" ++  (show (length more ))



justconvert (Just e) = e
showHead srctext = putStrLn (showKernelHeader(justconvert(parsePab(srctext))))
