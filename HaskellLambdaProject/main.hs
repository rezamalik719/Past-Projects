import PA1Helper
import System.Environment (getArgs)

-- Haskell representation of lambda expression
-- data Lexp = Atom String | Lambda String Lexp | Apply Lexp  Lexp 

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO ()

-- This is the identity function for the Lexp datatype, which is
-- used to illustrate pattern matching with the datatype. "_" was
-- used since I did not need to use bound variable. For your code,
-- however, you can replace "_" with an actual variable name so you
-- can use the bound variable. The "@" allows you to retain a variable
-- that represents the entire structure, while pattern matching on
-- components of the structure.
id' :: Lexp -> Lexp
id' v@(Atom _) = v
id' lexp@(Lambda _ _) = lexp
id' lexp@(Apply _ _) = lexp 

-- TODO: etaReduce
reducer :: Lexp -> Lexp
reducer lexp = etaConvert (betaReduce lexp)

-- etaConvert Function: Performs eta-conversion on a Lambda Calculus Expression
-- Lexp of type Atom: No eta-conversion necessary
-- Lexp of type Apply: Recursively eta-convert both expressions
-- Lexp of type Abstr: Recursively eta-convert the eta-converted expression if x is not free in M
etaConvert :: Lexp -> Lexp
etaConvert var@(Atom _) = var
etaConvert exp@(Apply exp1 exp2) = Apply (etaConvert exp1) (etaConvert exp2)
etaConvert exp@(Lambda var (Apply exp1 exp2))
    | (not (elem (Atom var) (getFreeVariables exp1))) && ((Atom var) == exp2) = etaConvert exp1
    | otherwise = exp
etaConvert (Lambda var exp) =  (Lambda var (etaConvert exp))


-- Recursive beta-reduction function
-- Lexp of type Atom: No beta reduction can be done for a lone variable
-- Lexp of type Lambda: Beta reduce the expression in the function abstraction
-- Lexp of type Apply: Recursively beta reduce both expressions, alpha-renaming if necessary
betaReduce :: Lexp -> Lexp
betaReduce var@(Atom _) = var
betaReduce abstr@(Lambda var exp) = Lambda var (betaReduce exp)
betaReduce app@(Apply exp1 exp2) = 
    let
        exp3 = betaReduce exp1
        exp4 = betaReduce exp2
        -- Alpha-rename variables in exp3 if necessary
        exp5 = alphaRename exp3 ((getAllVariables exp3) ++ getFreeVariables exp4)
        in
        case exp5 of (Lambda var exp) -> betaReduce (replaceAll (Atom var) exp4 exp)
                     otherwise -> (Apply exp5 exp4)

-- Recursive function to identify free variables in the given expression
-- Lexp of type Atom: A lone variable is itself a free variable
-- Lexp of type Lambda: Recursively identify free variables by first removing the bound variable 
-- Lexp of type Apply: Recursively combine free variables of both expressions
getFreeVariables :: Lexp -> [Lexp]
getFreeVariables var@(Atom _) = [var]
getFreeVariables lexp@(Lambda var exp) = removeFromList (getFreeVariables exp) (Atom var)
getFreeVariables lexp@(Apply exp1 exp2) = (getFreeVariables exp1) ++ (getFreeVariables exp2)

-- Recursive function to extract all variables in an expression
-- Lexp of type Atom: Self-explanatory (a variable is a variable)
-- Lexp of type Lambda: Recursively combine the bound variable with any variables in the expression
-- Lexp of type Apply: Recursively combine variables of both expressions
getAllVariables :: Lexp -> [Lexp]
getAllVariables v@(Atom _) = [v]
getAllVariables lexp@(Lambda var exp) = [(Atom var)] ++ (getAllVariables exp)
getAllVariables lexp@(Apply exp1 exp2) = (getAllVariables exp1) ++ (getAllVariables exp2)

-- Replace Function: replaces all occurences of 'arg' with 'val' in expression 'exp'
-- Lexp of type Atom: Replace or don't replace the one variable
-- Lexp of type Lambda: Recursively replace all occurrences of arg in the expression portion of the abstraction
-- Lexp of type Apply: Replace occurrences of arg in both expressions of application
replaceAll :: Lexp -> Lexp -> Lexp -> Lexp
replaceAll (Atom arg) val (Atom exp)
    | exp == arg = val
    | otherwise = Atom exp
replaceAll (Atom arg) val exp1@(Lambda var exp2) = Lambda var (replaceAll (Atom arg) val exp2)
replaceAll (Atom arg) val expr@(Apply exp1 exp2) = Apply (replaceAll (Atom arg) val exp1) (replaceAll (Atom arg) val exp2)

-- List Removal Function: Removes all occurences of an item from the list
-- If empty, just return the list
-- If the first element is equivalent to removed value, pop the first element and recurse down the rest of the list
-- Otherwise, recurse down the rest of the list, keeping the first element
removeFromList :: (Eq a) => [a] -> a -> [a]
removeFromList [] _ = []
removeFromList list@(head:tail) x
  | x == head = (removeFromList tail x)
  | otherwise = (head:(removeFromList tail x))


-- TODO: comments, rename, and understand
-- Alpha-Renaming Function: Performs alpha renaming of a lambda calculus expression
-- Lexp of type Atom: No alpha-renaming necessary for one or more free variables
-- Lexp of type Apply: Alpha-renaming is not performed on the function application (yet to identify which variable to rename)
-- Lexp of type Lambda: Replace bounded variable in function abstraction and recursively alpha-rename the rest of the expression
alphaRename :: Lexp -> [Lexp] -> Lexp
alphaRename v@(Atom _) _ = v
alphaRename lexp@(Apply exp1 exp2) _ = (Apply exp1 exp2)
alphaRename lexp@(Lambda var exp) avoidList =
    let
    x1 = find (Atom var) avoidList
    in
    Lambda (show x1) (alphaRename (replaceAll (Atom var) x1 exp) (avoidList ++ [x1]))

-- TODO: comments, rename, and understand
-- Find Function: Determines whether we can use the bounded variable or if we have to rename
-- Renaming is done by adding the "0" character to the current variable
find :: Lexp -> [Lexp] -> Lexp
find (Atom var) list
    | not (elem (Atom var) list) = (Atom var)
    | otherwise =  find (Atom (var ++ "0")) list

-- TEST ToDo: Check if we can eta-convert / beta-reduce / continue alpha-renaming a previously alpha-renamed variable

-- Entry point of program
main = do
    args <- getArgs
    let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
    let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram inFile outFile reducer
