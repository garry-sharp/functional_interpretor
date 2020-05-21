data IntExp
  	= Ivar String
  	| Icon Int
  	| Add IntExp IntExp
  	| Sub IntExp IntExp
  	| Mul IntExp IntExp
  	| Div IntExp IntExp
  	deriving (Read, Show)

data Tokens
	= KeyWord String
	| BooleanOperator String
	| MathematicalOperator String
	| AssignmentOperator String
	| Number Int
	| Comment String
	| Identifier String
	deriving (Read, Show)
{-
processIntExp [Tokens] -> IntExp
processIntExp xs =
	intExpThreeHelper (xs!!0) (xs!!1) (xs!!2)

intExpThreeHelper Tokens -> Tokens -> Tokens -> IntExp
intExpThreeHelper x y z 
	| y == "*" = Mul (IntExp x) (IntExp y)
	| y == "/" = Div (IntExp x) (IntExp y)
	| y == "-" = Sub (IntExp x) (IntExp y)
	| y == "+" = Add (IntExp x) (IntExp y)
	
-- [Identifier "x", MathematicalOperater "*", Number 7] --

-}

makeIntExp :: String -> IntExp
makeIntExp xs =
	read ("Mul 9 x")
