import Data.Char
import Data.List
import System.Random
import Test.QuickCheck

type Name = Int

data Form = Prop Name
 | Neg Form
 | Cnj [Form]
 | Dsj [Form]
 | Impl Form Form
 | Equiv Form Form
 deriving (Eq,Ord)

instance Show Form where
 show (Prop x) = show x
 show (Neg f) = '-' : show f
 show (Cnj fs) = "*(" ++ showLst fs ++ ")"
 show (Dsj fs) = "+(" ++ showLst fs ++ ")"
 show (Impl f1 f2) = "(" ++ show f1 ++ "==>" ++ show f2 ++ ")"
 show (Equiv f1 f2) = "(" ++ show f1 ++ "<=>" ++ show f2 ++ ")"

propNames :: Form -> [Name]
propNames = sort.nub.pnames where
 pnames (Prop name) = [name]
 pnames (Neg f) = pnames f
 pnames (Cnj fs) = concatMap pnames fs
 pnames (Dsj fs) = concatMap pnames fs
 pnames (Impl f1 f2) = concatMap pnames [f1,f2]
 pnames (Equiv f1 f2) = concatMap pnames [f1,f2]

allVals :: Form -> [Valuation]
allVals = genVals . propNames

type Valuation = [(Name,Bool)]
genVals :: [Name] -> [Valuation]
genVals [] = [[]]
genVals (name:names) =
 map ((name,True) :) (genVals names)
 ++ map ((name,False):) (genVals names)



showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs



data Token = TokenNeg
 | TokenCnj
 | TokenDsj
 | TokenImpl
 | TokenEquiv
 | TokenInt Int
 | TokenOP
 | TokenCP
 deriving (Show,Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
 | isDigit c = lexNum (c:cs)
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('*':cs) = TokenCnj : lexer cs
lexer ('+':cs) = TokenDsj : lexer cs
lexer ('-':cs) = TokenNeg : lexer cs
lexer ('=':'=':'>':cs) = TokenImpl : lexer cs
lexer ('<':'=':'>':cs) = TokenEquiv : lexer cs
lexer (x:_) = error ("unknown token: " ++ [x])

lexNum cs = TokenInt (read num) : lexer rest
 where (num,rest) = span isDigit cs

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed x xs = [(x,xs)]
parseForm :: Parser Token Form
parseForm (TokenInt x: tokens) = [(Prop x,tokens)]
parseForm (TokenNeg : tokens) = [ (Neg f, rest) | (f,rest) <- parseForm tokens ]
parseForm (TokenCnj : TokenOP : tokens) = [ (Cnj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenDsj : TokenOP : tokens) = [ (Dsj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenOP : tokens) = [ (Impl f1 f2, rest) | (f1,ys) <- parseForm tokens, (f2,rest) <- parseImpl ys ]
 ++
 [ (Equiv f1 f2, rest) | (f1,ys) <- parseForm tokens, (f2,rest) <- parseEquiv ys ]
parseForm tokens = []

parseForms :: Parser Token [Form]
parseForms (TokenCP : tokens) = succeed [] tokens
parseForms tokens =
 [(f:fs, rest) | (f,ys) <- parseForm tokens, (fs,rest) <- parseForms ys ]


parseImpl :: Parser Token Form
parseImpl (TokenImpl : tokens) =
 [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseImpl tokens = []
parseEquiv :: Parser Token Form
parseEquiv (TokenEquiv : tokens) =
 [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseEquiv tokens = []


parse :: String -> [Form]
parse s = [ f | (f,_) <- parseForm (lexer s) ]


arrowfree :: Form -> Form 
arrowfree (Prop x) = Prop x 
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) = 
  Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) = 
  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
  where f1' = arrowfree f1
        f2' = arrowfree f2

nnf :: Form -> Form 
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)

----------------------------------------------------------
-- 1)

----------------------------------------------------------
-- 2)


----------------------------------------------------------
-- 3)

toCNF :: Form -> Form
toCNF f = (nnf . arrowfree) f

stringToCNF :: String -> Form
stringToCNF s = (toCNF . head . parse) s




----------------------------------------------------------
-- 4)





main = print(stringToCNF "(1 <=> +(2 -3))")