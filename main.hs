module Main where
import Data.List
import Data.Maybe
import Data.List.Split
import System.IO.Unsafe
import GHC.Float

{- 	Garry Sharp
	0801585s
	Haskell Interpreter for a Small Custom Language (Garry Script)
	
	The following code/documentation is entirely my own work. Other than consulting wiki's or discussing the exercise with peers for general 	 ideas/approaches, the work in its entirity is my own. 
	
	IMPORTANT INFORMATION BEFORE COMPILING

	Before Running please install the Data.List.Split library via cabal (cabal install split), this may require a cabal update to be done first 		(cabal update)

	################################
	Program State
	################################

	The program is running fully as expected and most bugs are known. For convenience, here is a list of things that I know will break it:
		- Using parenthesis around expressions, parenthesis can only be placed around the main block of a boolean expression
		- Placing comments in certain areas of the code (see below for details)
		- Infinite loops in the code will result in NO DATA OUTPUT
		- Embedded begin statements will not work past 2 tiers. This is overcome by using better programming style, ie. 
		     using private procedures

	################################
	Program Grammar
	################################

	There are a few pre-requisites for the language which will be detailed below:

		- The main function must have the word main written above the begin statement. See example programs for usages.
		- Each boolean expression must be kept between parenthesis eg. (x + y < 6/x). Boolean expressions are whitespace insensitive
		- Operation keywords must be in the lower case.
		- All read and write expressions must be followed by a semi-solon.
		- Variable names may contain numbers but cannot start with them eg. var5 is okay but 5thVar is not.
		- Assignment operators can be written as either = or :=
		- Comments must be surrounded by '#' They may carry onto new lines.
		- Comments cannot be placed within a statement eg while (x < 7) # comment # begin is not allowed. It must be after each of the 				
		    statements args. In this case: while (x < 7) begin #do something#. This is because internally comments are treated as part of 				
		    a sequence of statements (They are a statement in their own right.
		- Any statement/s that are part of a while/ifthenelse statement must be surrounded by begin/end blocks
		- Variables that are declared at the top of the begin block.
		- If a variable name is reused and declared within a begin block then a new variable of that name is added for that scope. 
		    Making the original variable unreachable until the program leaves the begin block.
		- Unline normal boolean operations, in Garry Script syntax the operations for &&, || and not are &, | and ! respectively

	################################
	Extensions
	################################

	Here are a list of the extensions I've added:
	
		- Include comments in the code.
		- Doubles values are now supported.
		- Print out sting literals.
		- Cross type calculations (multiply an int with an int will return an int. Any operation using a double will always return a double)
		- The same variable can be equal to two different types
		- Some syntactic sugar x *= 3 for instace (equivalent to x = x * 3)
		- Less than OR EQUAL TO and Greater than OR EQUAL TO
		- Modulus functions and power functions have been added
		- Values true, false, and, or & not are now implemented, they are read from left to right (with the right most statements 
		    becoming the leaf expressions.
		- Functions/Procedures can be called. They must be defined at the top of the code in the form: def FunctionName begin ... end
		
	################################
	Sample Programs
	################################
	
	------------------------------------------------
	
	main
	begin [a,b]
	#Basic Comparator#
	
	read value_1;
	read value_2;
	a = 0;
			if (value_1 < value_2) 
				then begin
					printLn "value_1 was less than the value_2 ;)"
				end
				else begin
					printLn "value_1 was greater than the value_2 ;)"
				end
	end;
	
	--------------------------------------------
	
	main
	begin [a,f,b,c]
	#Calculate the value of x^y#
	
	read x;
	read y;
	print "The value of x^y is: "
	write x^y;
	end;
	
	----------------------------------------------
	
	main
	begin 
	#Alternative to the above (might only work with integer values. Try changing the code..)#
	
	read x;
	read y;
	counter = 0;
	newval = 1;
	while (counter < y)
		begin
			newval *= x;
			counter += 1;
		end
	print "The value of x^y is: "
	write newval;
	end;
	
	-----------------------------------------------------------
	
	def countup 
		begin
	
		#Count up/Count down program#

		printLn "What would you like to count up to?"
		read count;
		i = 0;
			while (i < count)
				begin
					i += 1;
					write i;
				end
		end
	endproc

	def countdown
		begin
		printLn "What would you like to count down from?"
		read count;
			while (count > 0)
				begin
					count -= 1;
					write count;
				end
		end
	endproc

	main 
	begin
		printLn "user_choice, 1 = count_up, all else is count down"
		read user_choice
		if (!user_choice == 1)
			then
				begin
					execute countdown;
				end 
			else
				begin
					execute countup;
				end
	end
	
	---------------
	Enjoy
	---------------
	
-}

type NewVar = String
data Val = Int | Double
type Var = String
data Decl = MakeDecl String deriving (Read, Show)

data IntExp
  	= IVar String
  	| ICon Int
	| IDoub Double 
  	| Add IntExp IntExp
  	| Sub IntExp IntExp
  	| Mul IntExp IntExp
  	| Div IntExp IntExp
	| Mod IntExp IntExp
	| Pow IntExp IntExp
  	deriving (Read, Show)

data BoolExp
	 = ILT IntExp IntExp
	 | ILTEQ IntExp IntExp
	 | IEQ IntExp IntExp
	 | IGT IntExp IntExp
	 | IGTEQ IntExp IntExp
	 | And BoolExp BoolExp
	 | Or BoolExp BoolExp
	 | Not BoolExp
	 | BooleanLiteral Bool
	 deriving (Read, Show)

data Proc = MakeProc String Stmt

data Stmt
	 = Begin [String] [Stmt]		-- Change String to Decl
	 | Assign Var IntExp
	 | Read Var
	 | Write IntExp
	 | While BoolExp Stmt			-- Orginially a single statement (a begin one I Imagine)
	 | IfThenElse BoolExp Stmt Stmt
	 | Print String
	 | Procedure String
	 deriving (Read, Show) 


-- ################################################ --
--                 Tree Builder                     --
-- ################################################ --

-- Building up function trees

{- buildFunctionEnvironment [KeyWord "def",Identifier "y",KeyWord "begin",KeyWord "write",Number 7,EndOfLine ";",KeyWord "end",KeyWord "endproc"] =
	[("y",Begin [] [Write (ICon 7)])]

-}
buildFunctionEnvironment :: [Tokens] -> ProcEnv
buildFunctionEnvironment ((KeyWord "def"):xs) =
	 funcBuilder ((KeyWord "def"):xs) : buildFunctionEnvironment(getArgsFrom(KeyWord "endproc") xs)
buildFunctionEnvironment xs = []

{-
funcBuilder [KeyWord "def",Identifier "y",KeyWord "begin",KeyWord "write",Number 7,EndOfLine ";",KeyWord "end",KeyWord "endproc"] =
("y",Begin [] [Write (ICon 7)]) -}
funcBuilder :: [Tokens] -> (String,Stmt)
funcBuilder ((KeyWord "def"):(Identifier x):xs) =
	(x,statementBuilder xs)

{-
existFunctions [KeyWord "def",Identifier "y",KeyWord "begin",KeyWord "write",Number 7,EndOfLine ";",KeyWord "end",KeyWord "endproc"] =
True -}
existFunctions :: [Tokens] -> Bool
existFunctions xs =
	if elem (KeyWord "def") xs
		then True
		else False

-- ******************** --
-- Building up BoolExps --
-- ******************** --

{- let toks = [Parens "(",Identifier "x",MathematicalOperator "+",Number 6,SpecialBoolean "&",BooleanConst "true",Parens ")"]
getBoolExpArgs toks =
[Parens "(",Identifier "x",MathematicalOperator "+",Number 6] -}
getBoolExpArgs :: [Tokens] -> [Tokens]
getBoolExpArgs (x:xs)
	| x == (SpecialBoolean "|") 
		|| x == (SpecialBoolean "&") 
		|| x == (SpecialBoolean "!") 
		|| x == (Parens ")") = []
	| otherwise = x:(getBoolExpArgs xs)

{-
boolExpEvalHelper "<=" (IVar "a") (IVar "b") = ILTEQ (IVar "a") (IVar "b") -}
getBoolExpArgsCount :: [Tokens] -> Int
getBoolExpArgsCount (x:xs)
	| x == (SpecialBoolean "|") 
		|| x == (SpecialBoolean "&") 
		|| x == (SpecialBoolean "!") 
		|| x == (Parens ")") = 1
	| otherwise = 1 + (getBoolExpArgsCount xs)

{- isSimple toks = false -}
isSimple :: [Tokens] -> Bool
isSimple xs 
	= if elem (SpecialBoolean "&") xs
	  || elem (SpecialBoolean "!") xs
	  || elem (SpecialBoolean "|") xs
		then False
		else True

{-getSpecialBoolean toks = "&" -}
getSpecialBoolean :: [Tokens] -> String
getSpecialBoolean (x:xs)
	| x == (SpecialBoolean "&") =  "&"
	| x == (SpecialBoolean "|") = "|"
	| otherwise = getSpecialBoolean xs

{- let toks = toks [Parens "(",Identifier "x",MathematicalOperator "+",Number 6,BooleanOperator "<",Number 10,SpecialBoolean "&",BooleanConst "true",Parens ")"]
boolExpEval toks = And (ILT (Add (IVar "x") (ICon 6)) (ICon 10)) (BooleanLiteral True) -}
boolExpEval :: [Tokens] -> BoolExp
boolExpEval (x:xs)
	| isSimple xs = boolExpEvalSimple $ getBoolExpArgs xs
	| (head xs) == (SpecialBoolean "!") = Not (boolExpEval xs)
	| getSpecialBoolean xs == "&" = And (boolExpEvalSimple $ getBoolArgs xs) (boolExpEval $ drop ((getBoolArgsHelper xs) - 1) xs)
	| getSpecialBoolean xs == "|" = Or (boolExpEvalSimple $ getBoolArgs xs) (boolExpEval $ drop ((getBoolArgsHelper xs) - 1) xs)

{-boolExpEvalSimple [Identifier "x",MathematicalOperator "+",Number 6,BooleanOperator "<",Number 10,Parens ")"] =
ILT (Add (IVar "x") (ICon 6)) (ICon 10) -}
boolExpEvalSimple :: [Tokens] -> BoolExp -- Boolean Expressions must be between parenthesis. 2 IntExp's separated by a BooleanOperator
boolExpEvalSimple (BooleanConst "true":_) = BooleanLiteral True
boolExpEvalSimple (BooleanConst "false":_) = BooleanLiteral False
boolExpEvalSimple xs =
	boolExpEvalHelper (getBoolOperator xs) (intExpEval(getBoolIntExpToks1 xs)) (intExpEval(dropLast (getBoolIntExpToks2 xs)))

{- boolExpEvalHelper "<=" (ICon 7) (ICon9) =
boolExpEvalHelper :: String -> IntExp -> IntExp -> BoolExp-}
boolExpEvalHelper op x y 
	| op == "<" = ILT x y
	| op == "<=" = ILTEQ x y
	| op == "==" = IEQ x y
	| op == ">=" = IGTEQ x y
	| op == ">" = IGT x y

-- ******************* --
-- Building up Stmts --
-- ******************* --

{-statementBuilder [KeyWord "begin",Comment "a comment",KeyWord "end"] = Begin [] []-}
statementBuilder :: [Tokens] -> Stmt
statementBuilder ((KeyWord "begin"):(Array x):xs) =
	(Begin x (statementArrayBuilder xs))
statementBuilder ((KeyWord "begin"):xs) =
	(Begin [] (statementArrayBuilder xs))


{-
statementArrayBuilder [KeyWord "read",Identifier "x",EndOfLine ";",KeyWord "write",Identifier "x",EndOfLine ";",KeyWord "end"] =
[Read "x",Write (IVar "x")]-}
statementArrayBuilder :: [Tokens] -> [Stmt]
statementArrayBuilder ((KeyWord "end"):xs) = []
statementArrayBuilder ((KeyWord "begin"):xs) =
	(statementBuilder ((KeyWord "begin"):xs)) : (statementArrayBuilder (getArgsFrom (KeyWord "end") xs))

statementArrayBuilder ((KeyWord "read"):(Identifier x): xs) = 				-- For a Read Statement
	(Read x) : statementArrayBuilder xs 	--Maybe remove + 1 ??

statementArrayBuilder ((KeyWord "write"):xs) =  			-- For a Write Statement
	(Write (intExpEval (getLineArgs xs))) : statementArrayBuilder (drop ((getLineArgsHelper xs) + 1) xs)

statementArrayBuilder ((KeyWord "execute"):(Identifier x):xs) =
	(Procedure x) : statementArrayBuilder xs

statementArrayBuilder ((Identifier x):(AssignmentOperator y):xs) = 	-- For an Assign Operation
	(Assign x (intExpEval (getLineArgs xs))) : statementArrayBuilder (drop ((getLineArgsHelper xs) + 1) xs)

statementArrayBuilder ((Identifier x):(IncOperator y):xs) =		-- For an Inc Operator
	(Assign x (intExpEval((Identifier x) : (MathematicalOperator y) :(getLineArgs xs)))) : statementArrayBuilder (drop ((getLineArgsHelper xs) + 1) xs)

statementArrayBuilder ((KeyWord "while"):xs) =				-- Had to change Parens ")" to KeyWord "begin"
	(While (boolExpEval (getArgsTo (KeyWord "begin") xs)) (statementBuilder (getArgsFrom (Parens ")") xs))) : (statementArrayBuilder (getArgsFrom 	 (KeyWord "end") xs))

statementArrayBuilder ((KeyWord "if"):xs) =				-- For an If then else statement
	(IfThenElse (boolExpEval (getArgsTo (KeyWord "begin") xs)) (statementBuilder (getArgsFrom (KeyWord "then") xs)) 
	(statementBuilder (getArgsFrom (KeyWord "else") xs))) : (statementArrayBuilder (getArgsFrom (KeyWord "end") (getArgsFrom (KeyWord "end") xs)))

statementArrayBuilder ((KeyWord "print"):(StringConst x):xs) =
	(Print x) : (statementArrayBuilder xs)
statementArrayBuilder ((KeyWord "printLn"):(StringConst x):xs) =
	(Print ("\n" ++ x ++ "\n")) : (statementArrayBuilder xs)

statementArrayBuilder ((Comment _):xs) =				-- For a comment
	statementArrayBuilder xs

statementArrayBuilder ((EndOfLine _):xs) =				-- For an EOL char
	statementArrayBuilder xs


-- ******************* --
-- Building up IntExps --
-- ******************* --

	-- When given a list of tokens (all of which make up an IntExp) will return an IntExp

{- intExpEval [Number 7,MathematicalOperator "+",Number 9,MathematicalOperator "*",Number 8] = Mul (Add (ICon 7) (ICon 9)) (ICon 8) -}
intExpEval :: [Tokens] -> IntExp
intExpEval (x:[]) = intExpLeaf x
intExpEval ((MathematicalOperator "-"):x:[]) = intExpEval ((Number 0):(MathematicalOperator "-"):x:[])
intExpEval (x:xs) 
	| (head xs) == (MathematicalOperator "*") = intExpEvalHelper (Mul (intExpLeaf x) (intExpLeaf (xs!!1))) (drop 2 xs)
	| (head xs) == (MathematicalOperator "/") = intExpEvalHelper (Div (intExpLeaf x) (intExpLeaf (xs!!1))) (drop 2 xs)
	| (head xs) == (MathematicalOperator "-") = intExpEvalHelper (Sub (intExpLeaf x) (intExpLeaf (xs!!1))) (drop 2 xs)
	| (head xs) == (MathematicalOperator "+") = intExpEvalHelper (Add (intExpLeaf x) (intExpLeaf (xs!!1))) (drop 2 xs)
	| (head xs) == (MathematicalOperator "%") = intExpEvalHelper (Mod (intExpLeaf x) (intExpLeaf (xs!!1))) (drop 2 xs)
	| (head xs) == (MathematicalOperator "^") = intExpEvalHelper (Pow (intExpLeaf x) (intExpLeaf (xs!!1))) (drop 2 xs)

	-- Helper to the above function (allows for intExp's to be part of a bigger intExp) NB. As of yet this doesn't support order of operations

{- intExpEvalHelper (Mul (Add (ICon 7) (ICon 9)) (ICon 8)) [MathematicalOperator "/", Number 6] =
Div (Mul (Add (ICon 7) (ICon 9)) (ICon 8)) (ICon 6)-}
intExpEvalHelper :: IntExp -> [Tokens] -> IntExp
intExpEvalHelper y [] = y
intExpEvalHelper y ((MathematicalOperator "*"):xs) = intExpEvalHelper (Mul y (intExpLeaf (head xs))) (drop 1 xs)
intExpEvalHelper y ((MathematicalOperator "/"):xs) = intExpEvalHelper (Div y (intExpLeaf (head xs))) (drop 1 xs)
intExpEvalHelper y ((MathematicalOperator "-"):xs) = intExpEvalHelper (Sub y (intExpLeaf (head xs))) (drop 1 xs)
intExpEvalHelper y ((MathematicalOperator "+"):xs) = intExpEvalHelper (Add y (intExpLeaf (head xs))) (drop 1 xs)
intExpEvalHelper y ((MathematicalOperator "%"):xs) = intExpEvalHelper (Mod y (intExpLeaf (head xs))) (drop 1 xs)
intExpEvalHelper y ((MathematicalOperator "^"):xs) = intExpEvalHelper (Pow y (intExpLeaf (head xs))) (drop 1 xs)

	-- Used for evaluating lead nodes in an intExp tree (determines whether a value is either a number or a variable

{- intExpLeaf (Number 7) = ICon 7 -}
intExpLeaf :: Tokens -> IntExp
intExpLeaf (Identifier x) = IVar x
intExpLeaf (Number x) = ICon x
intExpLeaf (Floating x) = IDoub x



-- ******************* --(drop ((getLineArgsHelper xs) + 1) xs)
--   General Helpers   --
-- ******************* --

	-- Helper function that returns a small list of tokens (all the tokens up to an end of line character)
{- 
getLineArgs [Identifier "x",AssignmentOperator "=",Number 9,MathematicalOperator "+",Number 6,EndOfLine ";",Identifier "s",AssignmentOperator "=",Number 6,MathematicalOperator "+",Number 2,EndOfLine ";"] =
[Identifier "x",AssignmentOperator "=",Number 9,MathematicalOperator "+",Number 6] -}
getLineArgs :: [Tokens] -> [Tokens]
getLineArgs toks =
	take (getLineArgsHelper toks) toks

	-- Helper function to the above helper function (returns a take index)
{-
getLineArgsHelper  [Identifier "x",AssignmentOperator "=",Number 9,MathematicalOperator "+",Number 6,EndOfLine ";",Identifier "s",AssignmentOperator "=",Number 6,MathematicalOperator "+",Number 2,EndOfLine ";"] = 5 -}
getLineArgsHelper :: [Tokens] -> Int
getLineArgsHelper (EndOfLine _:_) = 0
getLineArgsHelper (_:xs) = 1 + getLineArgsHelper xs

{- getParensArgs [Number 4,BooleanOperator "<",Number 6,Parens ")"] = [Number 4,BooleanOperator "<",Number 6] -}
getParensArgs :: [Tokens] -> [Tokens]
getParensArgs toks =
	take (getParensArgsHelper toks) toks

{- getParensArgsHelper [Number 4,BooleanOperator "<",Number 6,Parens ")"] = 3 -}
getParensArgsHelper :: [Tokens] -> Int
getParensArgsHelper (Parens _:_) = 0
getParensArgsHelper (_:xs) = 1 + getParensArgsHelper xs

{-getBoolArgs [Number 4,BooleanOperator "<",Number 6,SpecialBoolean "|",BooleanConst "true",Parens ")"] = 
[Number 4,BooleanOperator "<",Number 6,SpecialBoolean "|"] -}
getBoolArgs :: [Tokens] -> [Tokens]
getBoolArgs toks =
	take (getBoolArgsHelper toks) toks

{-getBoolArgsHelper [Number 4,BooleanOperator "<",Number 6,SpecialBoolean "|",BooleanConst "true",Parens ")"] = 4  -}
getBoolArgsHelper :: [Tokens] -> Int				--Gets the NUMBER of tokens up to the boolean operator (used in drop)
getBoolArgsHelper ((SpecialBoolean _):_) = 1			
getBoolArgsHelper (_:xs) = 1 + getBoolArgsHelper xs

{-getBoolOperator [Number 4,BooleanOperator "<",Number 6,SpecialBoolean "|",BooleanConst "true",Parens ")"] = "<" -}
getBoolOperator :: [Tokens] -> String				--Gets string representation of the boolean operator
getBoolOperator ((BooleanOperator x):xs) = x
getBoolOperator (_:xs) = getBoolOperator xs

{- getBoolIntExpToks1 [Number 4,BooleanOperator "<",Number 6,SpecialBoolean "|",BooleanConst "true",Parens ")"] = [Number 4] -}
getBoolIntExpToks1 :: [Tokens] -> [Tokens]			--Gets intExp relevant tokens before boolean operator
getBoolIntExpToks1 ((BooleanOperator _):_) = []
getBoolIntExpToks1 (x:xs) =
	x : getBoolIntExpToks1 xs 

{- getBoolIntExpToks2 [Number 4,BooleanOperator "<",Number 6,SpecialBoolean "|",BooleanConst "true",Parens ")"] = 
[Number 6,SpecialBoolean "|",BooleanConst "true",Parens ")"] -}
getBoolIntExpToks2 :: [Tokens] -> [Tokens]			--Gets intExp relevant tokens after boolean operator
getBoolIntExpToks2 ((BooleanOperator _):xs) = xs
getBoolIntExpToks2 ((Parens ")"):xs) = xs
getBoolIntExpToks2 (x:xs) = getBoolIntExpToks2 xs

{- dropLast [Number 6,SpecialBoolean "|",BooleanConst "true",Parens ")"] = [Number 6] -}
dropLast :: [Tokens] -> [Tokens]
dropLast [] = []
dropLast ((Parens ")"):xs) = []
dropLast ((SpecialBoolean _):xs) = []
dropLast (x:xs) = x : dropLast xs

{- getArgsFrom (SpecialBoolean "|") [Number 6,SpecialBoolean "|",BooleanConst "true",Parens ")"] = [BooleanConst "true",Parens ")"] -}
getArgsFrom :: Tokens -> [Tokens] -> [Tokens]
getArgsFrom y (x:xs) 
	| x == y = xs
	| otherwise = (getArgsFrom y xs)

{- getArgsTo (BooleanConst "true") [Number 6,SpecialBoolean "|",BooleanConst "true",Parens ")"] = [Number 6,SpecialBoolean "|"] -}
getArgsTo :: Tokens -> [Tokens] -> [Tokens]
getArgsTo y (x:xs)
	| x == y = []
	| otherwise = x : (getArgsTo y xs)


-- ################################################ --
--                 Lexical Analyser                 --
-- ################################################ --

data Tokens
	= KeyWord String
	| Array [String]	 												--putStr (show xs)
	| BooleanOperator String
	| SpecialBoolean String
	| BooleanConst String
	| MathematicalOperator String
	| AssignmentOperator String
	| IncOperator String
	| Number Int
	| Floating Double
	| Comment String
	| Identifier String
	| EndOfLine String
	| Parens String
	| StringConst String
	deriving (Eq, Read, Show)

{- lexicalAnalyser ["begin","[","a","]","execute","foo",";","end"] = [KeyWord "begin",Array ["a"],KeyWord "execute",Identifier "foo",EndOfLine ";",KeyWord "end"] -}
lexicalAnalyser :: [String] -> [Tokens]
lexicalAnalyser [] = []
lexicalAnalyser (x:xs)
	| elem x ["begin","read","write","end","while","do","if","then","else"] = (KeyWord x) : (lexicalAnalyser xs) 		--KeyWord
	| elem x ["printLn","print","def","endproc","main","execute"] = (KeyWord x) : (lexicalAnalyser xs) 			--KeyWord
	| elem x ["true","false"] = (BooleanConst x) : (lexicalAnalyser xs)							--Boolean Constant
	| x == "|" || x == "&" || x == "!" = (SpecialBoolean x)	: (lexicalAnalyser xs)						--Special Boolean
	| elem x ["=","<",">"] && nextIsEquals (head xs) = (BooleanOperator (x ++ (head xs))) : (lexicalAnalyser (skip xs))	--BooleanOperater
	| elem x ["<",">"] = (BooleanOperator x) : (lexicalAnalyser xs)								--BooleanOperater
	| elem x ["*","-","+","/"] && nextIsEquals (head xs) = (IncOperator x) : (lexicalAnalyser (skip xs)) 			--Operators like +=
	| elem x ["*","-","+","/","%","^"] = (MathematicalOperator x) : (lexicalAnalyser xs)					--MathematicalOperater
	| x == ":" && nextIsEquals (head xs) = (AssignmentOperator (x ++ (head xs))) : (lexicalAnalyser (skip xs))		--AssignmentOperater
	| x == "=" = (AssignmentOperator x) : (lexicalAnalyser xs)								--AssignmentOperater
	| x == ";" = (EndOfLine x) : (lexicalAnalyser xs)									--End of Line Char
	| x == ")" || x == "(" = (Parens x) : (lexicalAnalyser xs)								--Parenthesis
	| x == "#" =  Comment (unwords (take ((commentDrop xs) - 1) xs)) : lexicalAnalyser (drop (commentDrop xs) xs)		--Comments
	| x == "\"" = StringConst (unwords (take ((stringDrop xs) - 1) xs)) : lexicalAnalyser (drop (stringDrop xs) xs)		--String Constants
	| x == "[" = Array (buildArray xs) : lexicalAnalyser (drop (arrayDrop xs) xs)						--Array declarations
	| isNumber x 0 && elem '.' x = (Floating (read x :: Double)) : (lexicalAnalyser xs)					--Floating Point
	| isNumber x 0 = (Number (read x :: Int)) : (lexicalAnalyser xs)							--Numbers
	| otherwise = (Identifier x) : (lexicalAnalyser xs)

{- isNumber "0.234" 0 = True-}
isNumber :: String -> Int -> Bool
isNumber [] x = 
	if (x <= 1)
		then True
		else False
isNumber (x:xs) y =
	if elem x ['0'..'9'] 
		then isNumber xs y
		else
			if x == '.' 
				then isNumber xs (y+1)
				else False
				
-- Helpers
{- buildArray ["[","a","b","c"] = ["a","b","c"] -}
buildArray :: [String] -> [String]
buildArray ("]":_) = []
buildArray (x:xs) = x : buildArray xs

{- arrayDrop ["[","a","b","c"] = 4 -}
arrayDrop :: [String] -> Int
arrayDrop (x:xs) =
	if x == "]"
		then 1
		else 1 + arrayDrop xs

{- commentDrop ["this","is","a","comment","#"] = 5 -}
commentDrop :: [String] -> Int
commentDrop (x:xs) =
	if x == "#"
		then 1
		else 1 + commentDrop xs

{- stringDrop ["this","is","a","string","\""] = 5 -}
stringDrop :: [String] -> Int
stringDrop (x:xs) =
	if x == "\""
		then 1
		else 1 + stringDrop xs
{-nextIsEquals "=" = True-}
nextIsEquals :: String -> Bool
nextIsEquals xs =
	if (head xs) == '=' then True else False

{- skip ["a","b","c"] = ["b","c"] -}
skip :: [String] -> [String]
skip (x:xs) =
	xs

{- int2string 5 = "5" -}
int2string :: Int -> String
int2string x =
	show x

{- myDelimiter "hello world 	, 3" = ["hello","world","3"] -}
myDelimiter :: String -> [String]
myDelimiter xs =
	-- Delimit the program (removing the delimiters listed in the first argument of splitOneOf and filtering out black (""))
	-- Filter (\x -> x /= "")-- 
	filter (\x -> (not (elem x ["\n","\t","\r"," ",",",""]))) (split (oneOf "#<:=>^+-&|!\"()[]/%,*;\n\r\t ") xs)
	
-- ******************* --
--     Interpreter     --
-- ******************* --

type ProcEnv = [(String,Stmt)]
type Env = [SubEnv]
type SubEnv = [(NewVar, MyVal)]
data MyVal 
	= IntVal Int 
	| DoubVal Double 
	deriving (Read, Show)

--Interpret Statements (for begin blocks)
{- processDecsHelper ["a","b","c"] = [("a",IntVal 0),("b",IntVal 0),("c",IntVal 0)] -}
processDecsHelper :: [String] -> SubEnv
processDecsHelper [] = []
processDecsHelper (x:xs) =
	(x,IntVal 0) : processDecsHelper xs

{- interpret (Begin ["x"] [Write (IVar "x")]) [] [] = 0 [] -}
interpret :: Stmt -> Env -> ProcEnv -> IO Env
interpret (Begin decs stmts) [] procenv =
	interpretStatements stmts ((processDecsHelper decs) : []) procenv
interpret (Begin decs stmts) env procenv =
	interpretStatements stmts ((processDecsHelper decs) : env) procenv

{- interpretStatements ([Write (IDoub 3.4)]) [] [] = 3.4 [] -}

interpretStatements :: [Stmt] -> Env -> ProcEnv -> IO Env
interpretStatements [] env _= do
	return $ drop 1 env

interpretStatements ((Write x):stmts) env procenv= do	
	let x' = reduceIntExp x env in							-- For a write Statement
		if (isJust x')
			then do
				if isDoub (fromJust x')
					then putStrLn (show (getDoub(fromJust x')))
					else putStrLn (show (getInt(fromJust x')))
				interpretStatements stmts env procenv
			else do 
				putStrLn "Error in write statement"
				return env

interpretStatements ((Read x):stmts) env procenv = do								-- For a read Statement
	putStrLn ("Variable "++(show x)++ " is equal to: ")
	y <- getLine
	putStrLn ""
	let intExpY = intExpEval(lexicalAnalyser(myDelimiter y))
	if isJust(reduceIntExp intExpY env)
		then interpretStatements stmts (myupdate x (fromJust(reduceIntExp intExpY env)) env) procenv
		else do
			putStrLn ("Error in read statement") 
			return (myupdate x (fromJust(reduceIntExp intExpY env)) env)

interpretStatements ((Assign x y):stmts) env procenv = do						-- For an assign Statement (same as read atm)
	if (isJust (reduceIntExp y env))
		then 
			interpretStatements stmts (myupdate x (fromJust(reduceIntExp y env)) env) procenv
		else do
			putStrLn ("Error in assign statement")
			return (myupdate x (fromJust(reduceIntExp y env)) env) 

interpretStatements ((Begin x y):stmts) (z:zs) procenv= do
	--putStrLn "Env : " ++ (show (unsafePerformIO (interpret (Begin x y) (z:zs))))
	interpretStatements stmts (unsafePerformIO (interpret (Begin x y) (z:zs) procenv)) procenv

interpretStatements ((IfThenElse boolexp stmt1 stmt2):stmts) env procenv= do
	if isJust(reduceBoolExp boolexp env)
		then
			if (fromJust(reduceBoolExp boolexp env))
				then do
					interpret stmt1 env procenv
					interpretStatements stmts env procenv
				else do 
					interpret stmt2 env procenv
					interpretStatements stmts env procenv
		else do
			putStrLn ("Error in If Then-Else-statement")
			return env

interpretStatements ((While boolexp st):stmt) env procenv= do
	if isJust(reduceBoolExp boolexp env)
		then
			if (fromJust (reduceBoolExp boolexp env))
				then do 
					interpretStatements ((While boolexp st):stmt) (unsafePerformIO (interpret st env procenv)) procenv --updated env
				else interpretStatements stmt env procenv--updated env
		else do
			putStrLn ("Error in while loop")
			return env

interpretStatements ((Print x):stmts) env procenv = do								-- For a write Statement
	putStr x
	interpretStatements stmts env procenv

interpretStatements ((Procedure x):stmts) env procenv = do
	let proc = procedureLoad x procenv
	if isNothing proc
		then interpretStatements stmts env procenv
		else do 
			interpret (fromJust proc) env procenv
			interpretStatements stmts env procenv



--Reduce a BoolExp
{- 
let u = [("a",IntVal 8)] ::SubEnv
let w = (u : []) ::Env
reduceBoolExpHelper (Add (ICon 2) (IVar "a")) (Add (ICon 7) (ICon 90)) w = True -}
reduceBoolExpHelper :: IntExp -> IntExp -> Env -> Bool
reduceBoolExpHelper x y env
	|(isNothing (reduceIntExp x env)) == True = False
	|(isNothing (reduceIntExp y env)) == True = False
	|otherwise = True

{-reduceBoolExp (Not (IEQ (IVar "a") (ICon 2))) w = Just True -}
reduceBoolExp :: BoolExp -> Env -> Maybe Bool
reduceBoolExp (BooleanLiteral True) _ = Just True
reduceBoolExp (BooleanLiteral False) _ = Just False

reduceBoolExp (And x y) env =
	if isJust (reduceBoolExp x env) && isJust (reduceBoolExp y env)
		then Just $ (fromJust (reduceBoolExp x env)) && (fromJust (reduceBoolExp y env))
		else Nothing

reduceBoolExp (Or x y) env =
	if isJust (reduceBoolExp x env) || isJust (reduceBoolExp y env)
		then Just $ (fromJust (reduceBoolExp x env)) || (fromJust (reduceBoolExp y env))
		else Nothing

reduceBoolExp (Not x) env =
	if isJust (reduceBoolExp x env)
		then Just $ not (fromJust (reduceBoolExp x env))
		else Nothing

reduceBoolExp (ILT x y) env = do
	if (reduceBoolExpHelper x y env) == True
		then Just ((getDoub(fromJust (reduceIntExp x env))) < (getDoub(fromJust (reduceIntExp y env))))
		else Nothing
reduceBoolExp (ILTEQ x y) env = do
	if (reduceBoolExpHelper x y env) == True
		then Just ((getDoub(fromJust (reduceIntExp x env))) <= (getDoub(fromJust (reduceIntExp y env))))
		else Nothing
reduceBoolExp (IEQ x y) env = do
	if (reduceBoolExpHelper x y env) == True
		then Just ((getDoub(fromJust (reduceIntExp x env))) == (getDoub(fromJust (reduceIntExp y env))))
		else Nothing
reduceBoolExp (IGT x y) env = do
	if (reduceBoolExpHelper x y env) == True
		then Just ((getDoub(fromJust (reduceIntExp x env))) > (getDoub(fromJust (reduceIntExp y env))))
		else Nothing
reduceBoolExp (IGTEQ x y) env = do
	if (reduceBoolExpHelper x y env) == True
		then Just ((getDoub(fromJust(reduceIntExp x env))) >=  (getDoub(fromJust (reduceIntExp y env))))
		else Nothing

--Reduce an IntExp
{-reduceIntExp (Pow (Add (Add (Add (IVar "a") (ICon 6)) (ICon 8)) (ICon 10)) (IDoub 3.2)) w = Just (DoubVal 65536.00000000004) -}
reduceIntExp :: IntExp -> Env -> Maybe MyVal
reduceIntExp (ICon x) _ = Just (IntVal x)
reduceIntExp (IDoub x) _ = Just (DoubVal x)
reduceIntExp (IVar x) myenv = 
	if isNothing(mylookup x myenv)
		then Nothing
		else mylookup x myenv
		
reduceIntExp (Pow x y) myenv = do
	let x' = reduceIntExp x myenv
	let y' = reduceIntExp y myenv
		in
			if (isNothing x') || (isNothing y')
				then Nothing
				else 
					if ( (isDoub (fromJust x')) || (isDoub (fromJust y')))	--If either value is a double then return a double
						then Just (DoubVal ((getDoub (fromJust(x'))) ** (getDoub (fromJust(y')))))
						else Just (IntVal ((getInt (fromJust(x'))) ^ (getInt (fromJust(y')))))

reduceIntExp (Mul x y) myenv = do
	let x' = reduceIntExp x myenv
	let y' = reduceIntExp y myenv
		in
			if (isNothing x') || (isNothing y')
				then Nothing
				else 
					if ( (isDoub (fromJust x')) || (isDoub (fromJust y')))	--If either value is a double then return a double
						then Just (DoubVal ((getDoub (fromJust(x'))) * (getDoub (fromJust(y')))))
						else Just (IntVal ((getInt (fromJust(x'))) * (getInt (fromJust(y')))))

reduceIntExp (Add x y) myenv = do
	let x' = reduceIntExp x myenv
	let y' = reduceIntExp y myenv
		in
			if (isNothing x') || (isNothing y')
				then Nothing
				else 
					if ( (isDoub (fromJust x')) || (isDoub (fromJust y')))	--If either value is a double then return a double
						then Just (DoubVal ((getDoub (fromJust(x'))) + (getDoub (fromJust(y')))))
						else Just (IntVal ((getInt (fromJust(x'))) + (getInt (fromJust(y')))))

reduceIntExp (Div x y) myenv = do
	let x' = reduceIntExp x myenv
	let y' = reduceIntExp y myenv
		in
			if (isNothing x') || (isNothing y')
				then Nothing
				else Just (DoubVal ((getDoub(fromJust(x'))) / (getDoub(fromJust(y')))))

reduceIntExp (Sub x y) myenv = do
	let x' = reduceIntExp x myenv
	let y' = reduceIntExp y myenv
		in
			if (isNothing x') || (isNothing y')
				then Nothing
				else 
					if ( (isDoub (fromJust x')) || (isDoub (fromJust y')))	--If either value is a double then return a double
						then Just (DoubVal ((getDoub (fromJust(x'))) - (getDoub (fromJust(y')))))
						else Just (IntVal ((getInt (fromJust(x'))) - (getInt (fromJust(y')))))

reduceIntExp (Mod x y) myenv = do
	let x' = reduceIntExp x myenv
	let y' = reduceIntExp y myenv
		in
			if (isNothing x') || (isNothing y')
				then Nothing
				else 
					if ( (isDoub (fromJust x')) || (isDoub (fromJust y')))
						then Nothing
						else Just (IntVal (mod (getInt(fromJust(x'))) (getInt(fromJust(y')))))
{- getInt (IntVal 62) = 62 -}
getInt :: MyVal -> Int
getInt (IntVal x) = x
{- getDoub (DoubVal 3.7) = 3.7 -}
getDoub :: MyVal -> Double
getDoub (DoubVal x) = x
getDoub (IntVal x) = int2Double x
{- isDoub (DoubVal 3.7) = True -}
isDoub :: MyVal -> Bool
isDoub (IntVal _) = False
isDoub (DoubVal _) = True

-- Loads a function/procedure
{-procedureLoad "foo" [("foo",Begin [] [Print "\nhello\n"])] = Just (Begin [] [Print "\nhello\n"]) -}
procedureLoad :: String -> ProcEnv -> Maybe Stmt
procedureLoad _ [] = Nothing
procedureLoad x (y:ys) =
	if x == (fst y)
		then Just (snd y)
		else procedureLoad x ys

		
-- Get a value from the environment
{- mylookup "a" w = Just (IntVal 8) -}
mylookup :: String -> Env -> Maybe MyVal
mylookup x [] = Nothing
mylookup x ([]:zs) =
	mylookup x zs
mylookup x ((y:ys):zs) 
	| x == (fst y) = Just (snd y)
	| otherwise = mylookup x (ys:zs)
	
-- Update the environment
{- myupdate "a" (DoubVal 3.6) w = [[("a",DoubVal 3.6)]] -}
myupdate :: String -> MyVal -> Env -> Env
myupdate x y (z:zs) =
	if isNothing(mylookup x (z:zs))				-- Checks to see if it needs to add or update a variable/
		then (myupdateNew x y z):zs
		else myupdateExisting x y (z:zs)
	
{- myupdateNew "b" (IntVal 4) u = [("b",IntVal 4),("a",IntVal 8)] -}
myupdateNew :: String -> MyVal -> SubEnv -> SubEnv		-- Adds a new variable to the current scope (head of the environment)
myupdateNew x y z =
	(x,y):z

{- myupdateExisting "a" (DoubVal 6.4) w =  [[("a",DoubVal 6.4)]] -}
myupdateExisting :: String -> MyVal -> Env -> Env		-- Updates the first variable that it comes across (most recent scope)
myupdateExisting x y [] = [[(x,y)]] 
myupdateExisting x y (z:zs) = 
	if (localEnvExists x z)					-- Necessary to stop all variables in all scopes being changes is x used several times.
		then ((myupdateExistingHelper x y z) : zs)
		else (z : (myupdateExisting x y zs))

{- localEnvExists "a" u = True -}
localEnvExists :: String -> SubEnv -> Bool		
localEnvExists x [] = False
localEnvExists x (y:ys) =
	if x == (fst y)
		then True
		else localEnvExists x ys

{- myupdateExistingHelper "a" (IntVal 17) u = [("a",IntVal 17)] -}
myupdateExistingHelper :: String -> MyVal -> SubEnv -> SubEnv		-- Loops through the scopes (SubEnv) and returns an updated scope (SubEnv)
myupdateExistingHelper x y [] = []
myupdateExistingHelper x (IntVal y) (z:zs)
	| x == (fst z) = (x,(IntVal y)) : zs
	| otherwise = z : (myupdateExistingHelper x (IntVal y) zs)
myupdateExistingHelper x (DoubVal y) (z:zs)
	| x == (fst z) = (x,(DoubVal y)) : zs
	| otherwise = z : (myupdateExistingHelper x (DoubVal y) zs)

-- ################################################ --
-- ################################################ --
-- ################################################ --

-- IntVar's --

{- These may be either a variable name, an integer or 
arithmetic operation (that should reduce to an integer -}
{- isIntExp "7 + 2" = True -}
isIntExp :: String -> Bool
isIntExp str =
  if elem '+' str || elem '-' str || elem '/' str || elem '*' str
       then True
       else False
    
main = do
	 putStrLn "\nWelcome to the Garry Script Interpreter v.1.0\n"
	 putStrLn "Please type in the filename you would like to interpret:"
	 filename <- getLine
	 x <- readFile filename
	 putStrLn "\tFile Read..\n"
	 putStrLn x
	 let xs = (lexicalAnalyser (myDelimiter x))
	 putStr ("\nProgram Tokenised...\n\n" ++ (show xs) ++ "\n")
	 let funcEnv = buildFunctionEnvironment xs
	 putStr("\nFunction/s AST/s Built...\n\n" ++ (show funcEnv) ++ "\n\n")
	 let testStmt = statementBuilder $ getArgsFrom (KeyWord "main") xs
	 putStr("\nMain AST Built...\n\n" ++ (show testStmt) ++ "\n\n")
	 --putStrLn ".......\n.......\n"
	 putStrLn "\nInitial Environment set to null..."
	 putStrLn "Preparing to execute...\n"
	 interpret testStmt [] funcEnv
	 putStrLn "Finished interpreting, would you like to interpret again? y/n"
	 resp <- getLine
	 if (resp == "y")
		then main
		else do
			putStrLn "\nThank you for using Garry Script v1.0. Have a wonderful day!\n"
			return()




