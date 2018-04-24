{-
Module : ExprParser
Description: A module using Parsec to build a parser for the Expr class
Copyright: (c) aryafara @ 2018
License : BSD
Stability : experimental (not at all)
Portability : MSDOS (will be ported to Linux)

This module contains everything used to parse a string into an expression.

Binary Operator Functionality:

{expr}'!+'{expr}: makes a left associative 'Add' Expression 
                  (right associativity wouldn't be worthwhile for the level of simplicity)
useful example: \[1+1+1\]
ghci>> let m = parseExprI "1!+1!+1"
ghci>> show m
    Add

{expr}'!*'{expr}: makes a left associative 'Mult' Expression 
                  (actually a good base for the other forms of multiplication if I extended this)
useful example: \[3*4*5\]
ghci>> let m = parseExprI "3!*4!*5"
ghci>> show m
    Mult

{expr}'!^'{expr}: makes a left associative 'Exp' Expression 
                  (tfw your operator has all the properties it needs, feelsgoodman)
useful example: \[3^{3^3}\] (3 power tower 3)
ghci>> let m = parseExprI "3!^3!^3"
ghci>> show m
    Exp

Operation Order:
Parentheses (functions) => Exponentiation => Multiplication => Addition
useful examples: \[1+5x^2\] and \[{1+5x}^2\]

Unary Operator Functionality: (basically transcendentals lol)

cos{expr} : parsed cosine expression
useful example: \[cosx\]

sin{expr} : parsed sine expression
useful example: \[sinx\]

ln{expr} : parsed natural logarithm expression (comes in handy later too!)
useful example: \[lnx\]

e^{expr} : parsed natural exponential function expression
useful example: \[e^x\]

Other important things to note:
'member how I talked bout how we had an almost minimal set of type combinators, this is where those useful ones come in handy!

Division:
    using our  Cons a, and Exp (Expr a) (Expr a) and Mult (Expr a) (Expr a) allows us to implement somewhat messy division
    such that: Mult {Numerator} (Exp {Divisor} (Cons (-1)))
    (dont worry kiddos it'll get simpler by the time ExprPretty is finished)

Logs of arbitrary base:
    Using our division expression above we can just add Ln (Expr a) to make an easy change of base formula:
    Mult (ln {resultant}) (Exp (ln {base}) (Cons (-1)))

-}
module ExprParser (parseExprD,parseExprF) where

import           ExprType

import           Text.Parsec
import           Text.Parsec.String


-- //Foreword//

{-
 Since any calculus worth its weight in salt doesn't have a need for modular arithmetic
 of any kind there will be only two parsers for todays weaksauce parser
 parseExrD, for all your imperfect needs
 parseExprI, for when you want a special kind of hell
 both come with their own rules that will break the calculus we're trynna use but 
 parseExprD will work well enough for engineers, and parseExprI could definitely be used for
 Diophantine equations (assuming your cofactors are nice c:) 
 alas someday we will have a quantum lib that will give us the continuous numbersets computer
 science needs.
-}

-- \\Foreword\\

parseExprD :: String -> Expr Double 
--simple helper for later combinators// this variant is for strings with doubles
parseExprD ss = case parse exprD "" ss of
                  Left err   -> error "This is an invalid string :("
                  Right expr -> expr

parseExprI :: String -> Expr Integer
--simple helper for later combinators// this variant is for strings with integers
parseExprF ss = case parse exprI "" ss of
                  Left err   -> error "This is an invalid string :("
                  Right expr -> expr

{-
 Parser Combinators credits to chenc, his documentation is 
 the only reason this will be finished lol
-}

-- /Double Combinators, taken from chenc's {type}Factor notation and {type abbrev.}Const notation
exprD :: Parser (Expr Double)
exprD = try (parens $ expr exprD) <|> try conD <|> var

conD = do {d <- double;
             return (Cons d)}

-- /Integer Combinators, taken from chenc's {type}Factor notation and {type abbrev.}Const notation
exprI :: Parser (Expr Integer)
exprI = try (parens $ expr exprI) <|> try constI <|> var

conI = do {i <- integer;
            return (Cons i)}

-- /Utility Parsers for the above

--simple variable parsing using identity in case of multi-letter/number varnames. varnames must be spacepadded
var :: Parser (Expr a)
var = do {spaces; --notation in: (where __ represents space) _{*however much you want}_{varname}_{*however much you want}_
          ss <- ident;
          spaces;
          return (Var ss)}

--simple multi-letter/number parser
ident :: Parser String
ident = many1 alphaNum

--note: much of the below simple type combinator parsers work not so simply, I'll try to convey it as best as I can within my implementation

--parse a multiplication operation consisting of the symbol !*
multOp :: Parser (Expr a -> Expr a -> Expr a)
multOp = do {symbol "!*";
            return (Mult)}

--parse an addition operation consisting of the symbol !+
addOp :: Parser (Expr a -> Expr a -> Expr a)
addOp = do {symbol "!+";
            return (Add)}

--parse an exponentiation operation consisting of the symbol !^
expOp :: Parser (Expr a -> Expr a -> Expr a)
expOp = do {symbol "!^";
            return (Exp)}

--parse an unuary function, sooo ln, sin, cos, etc.
uniFn :: String -> (Expr a -> Expr a) -> Parser (Expr a) -> Parser (Expr a)
uniFn name construct interior = do {symbol name;
                                expr <- interior;
                                return (construct interior)}

--parsing works in this kind of chaining process to the highest order of operation (functions!)
expr :: Parser (Expr a) -> Parser (Expr a) --an expression is just a set of chained additions of terms 
expr factor = (term factor) `chainl1` addOp--which lead to...

term :: Parser (Expr a) -> Parser (Expr a) --a term is just a set of chained multiplications of functions/expressions
term factor = (expo factor) `chainl1` multOp--which lead to..

expo :: Parser (Expr a) -> Parser (Expr a) --this is kinda a side point (since its a function itself)
expo factor = (try (fn factor)) `chainl1` expOp--but yeah moving on...

fn :: Parser (Expr a) -> Parser (Expr a) -- a set of arbitrary (transcendental and identity) functions make up a term!
fn factor  = uniFn "sin" Sin factor 
        <|>  uniFn "cos" Cos factor
        <|>  uniFn "e^"  Nat factor
        <|>  uniFn "ln"  Ln  factor
        <|>  uniFn "~"   N   factor 
        --I think tilda works better for negation in this case,
        --both cause its lazy and it works with 
        --boolean logic ideas (similar to the complement hat)

-- /Special formula parsers

parseLBR :: Parser (Expr a) -> Parser (Expr a) -- Using base change laws with logarithm laws to efficiently (in algebra) parse data
parseLogBase obj = do {symbol "log"; -- notation in: log{base}({resultant}) notation out: ln{resultant}/ln{base}
                          base <- obj;
                          symbol "(";
                          resultant <- obj;
                          symbol ")";
return (Mult (Ln resultant) (Exp (Ln base) (Cons (-1))}

parseDiv :: Parser (Expr a) -> Parser (Expr a)
parseDiv obj = do {numerator <- obj;
                   symbol "!/";
                   denominator <- obj;
                   return (Mult (numerator) (Exp (denominator) (Cons (-1))}

--general utilities

parens :: Parser a -> Parser a
parens p = do { symbol "(";
                cs <- p;
                symbol ")";
                return cs }

symbol :: String -> Parser String
symbol ss = let
  symbol' :: Parser String
  symbol' = do { spaces;
                 ss' <- string ss;
                 spaces;
                 return ss' }
  in try symbol'

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                 dig <- digits ;
                 return (neg ++ dig) }

dec :: Parser String
dec = do {
            char '.';
            a <- digits;
            return ("."++a)}

mantissa :: Parser String
mantissa = do {
            char 'e';
            e <- (try negDigits<|>digits);
            return ("e"++e);
            }

nilStr :: Parser String
nilStr = do {return ""}

decimal :: Parser String 
decimal = do {r <- digits;
                a <- (try dec<|>nilStr);
                e <- (try mantissa<|>nilStr);
            return (r++a++e)}

negDecimal :: Parser String
negDecimal = do {neg <- symbol "-";
                dec <- decimal;
return (neg++dec)}

--Number parsers
double :: Parser Double
double = fmap read $ try negDecimal <|> decimal

integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits