-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający rozwiązanie.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko} gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module ImieNazwisko (typecheck, eval) where

-- Importujemy moduły z definicją języka oraz typami potrzebnymi w zadaniu
import AST
import DataTypes
import Control.Applicative (liftA2, (<$>), (<|>))
import Data.Maybe (fromJust)
import qualified Data.Map as Map


-------------------------------------------------------------------------------
-- SPRAWDZANIE TYPÓW
-------------------------------------------------------------------------------


-- Środowisko typów dla funkcji
type FTMap = Map.Map FSym (Var, Type, Type)
-- Środowisko typów dla zmiennych
type VTMap = Map.Map Var Type

-- Tworzy środowisko typów funkcji
makeFTMap :: [FunctionDef p] -> FTMap
makeFTMap = Map.fromList . (map (\f -> (funcName f, (funcArg f, funcArgType f, funcResType f))))
-- Tworzy środowisko typów zmiennych
makeVTMap :: [Var] -> VTMap
makeVTMap = Map.fromList . (map (\x -> (x, TInt)))

-- Funkcja sprawdzająca typy
-- Dla wywołania typecheck vars e zakładamy, że zmienne występujące
-- w vars są już zdefiniowane i mają typ int, i oczekujemy by wyrażenia e
-- miało typ int
typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p
typecheck funs vars exp =
  case typecheck' funs vars exp of
    Right () -> Ok
    Left (p, er) -> Error p er

-- Zadanie typecheck' jest takie samo jak typcheck, z tym, że typecheck' zwraca monadę, więc
-- pozwala na użycie w ciele funkcji eleganckiej notacji monadowej.
-- Funkcja typecheck jest teraz tylko nakładką na typecheck', mapującą monadę na TypeCheckResult p.
typecheck' :: [FunctionDef p] -> [Var] -> Expr p -> Either (p, ErrorMessage) ()
typecheck' funs vars e = do
  let fenv = makeFTMap funs
      env  = makeVTMap vars
  mapM_ (funcheck fenv) funs -- sprawdzamy, czy definicje funkcji są poprawne
  t <- evalType fenv env e   -- sprawdzamy poprawność typów ciała programu
  if (t == TInt)             -- na kończu oczekujemy integer
    then return ()
    else Left (getData e, "int expected but given " ++ show t)

-- Sprawdza, czy zwracany typ jest zgodny z deklarowanym typem funkcji
funcheck :: FTMap -> FunctionDef p -> Either (p, ErrorMessage) ()
funcheck fenv f = do
  t <- evalType fenv (Map.fromList [(funcArg f, funcArgType f)]) (funcBody f)
  if (t == funcResType f)
    then return ()
    else Left (funcPos f, "Function " ++ show (funcName f) ++ " should return type " ++ show (funcResType f) ++
      " but type " ++ show t ++ " given")


-- Oblicza typ wyrażenia
evalType :: FTMap -> VTMap -> Expr p -> Either (p, ErrorMessage) Type
evalType _ env (EVar p x) = case Map.lookup x env of
  Just t -> return t
  _      -> Left (p, "Variable " ++ show x ++ " undefined")
evalType _ _ (ENum _ _) = return TInt
evalType _ _ (EBool _ _) = return TBool

evalType fenv env (EUnary p UNot e) = do
  t <- evalType fenv env e
  case t of
    TBool -> return TBool
    t     -> Left (p, show TBool ++ " expected but given " ++ show t)

evalType fenv env (EUnary p UNeg e) = do
  t <- evalType fenv env e
  case t of
    TInt -> return TInt
    t    -> Left (p, show TInt ++ " expected but given " ++ show t)

evalType fenv env (EBinary p op e1 e2) = do
  t1 <- evalType fenv env e1
  t2 <- evalType fenv env e2
  let (typeIn, typeOut) = opTypes op
  if (all (== typeIn) [t1, t2])
    then return typeOut
    else Left (p, show typeIn ++ " expected but given " ++ show t1 ++ " and " ++ show t2)

evalType fenv env (ELet p x e1 e2) = do
  t1 <- evalType fenv env e1
  evalType fenv (Map.insert x t1 env) e2

evalType fenv env (EIf p e1 e2 e3) = do
  t1 <- evalType fenv env e1
  t2 <- evalType fenv env e2
  t3 <- evalType fenv env e3
  if (t1 == TBool)
    then if (t2 == t3)
      then return t2
      else Left (p, "incostinstent types in if statement: " ++ show t2 ++ ", " ++ show t3)
    else Left (p, "bool expected in if statement but given " ++ show t1)

evalType fenv env (EApp p f e) = case Map.lookup f fenv of
  Nothing -> Left (p, "function " ++ show f ++ " undefined")
  Just (x, typeIn, typeOut) -> do
    t <- evalType fenv env e
    if (t == typeIn)
      then return typeOut
      else Left (p, "couldn't match types " ++ show t ++ " and " ++ show typeIn)

evalType _ _ (EUnit _) = return TUnit

evalType fenv env (EPair _ e1 e2) = do
  t1 <- evalType fenv env e1
  t2 <- evalType fenv env e2
  return (TPair t1 t2)

evalType fenv env (EFst p e) = do
  t <- evalType fenv env e
  case t of
    TPair t1 _ -> return t1
    t'          -> Left (p, "pair expected, but given " ++ show t')

evalType fenv env (ESnd p e) = do
  t <- evalType fenv env e
  case t of
    TPair _ t2 -> return t2
    t'          -> Left (p, "pair expected, but given " ++ show t')
  
evalType _ _ (ENil p t) = return t

evalType fenv env (ECons p e1 e2) = do
  t1 <- evalType fenv env e1
  t2 <- evalType fenv env e2
  if (t2 == TList t1)
    then return t2
    else Left (p, "not expected types in array constructor: " ++ show t1 ++ ", " ++ show t2)

evalType fenv env (EMatchL p e e1 (x, xs, e2)) = do
  t <- evalType fenv env e
  case t of
    TList t' -> do
      let env' = Map.insert x t' (Map.insert xs t env)
      t1 <- evalType fenv env  e1
      t2 <- evalType fenv env' e2
      if (t1 == t2)
        then return t1
        else Left (p, "couldn't match types: " ++ show t1 ++ " and " ++ show t2)
    t'      -> Left (p, "list expected but given " ++ show t')
      

-- Zwraca parę (typ artumentów, typ wyjściowy) dla danego operatora binarnego
opTypes :: BinaryOperator -> (Type, Type)
opTypes op
  | elem op [BAnd, BOr] = (TBool, TBool)
  | elem op [BEq, BNeq, BLt, BGt, BLe, BGe] = (TInt, TBool)
  | otherwise = (TInt, TInt)


-------------------------------------------------------------------------------
-- WYLICZANIE WARTOŚCI
-------------------------------------------------------------------------------


-- Typ zwracany przez funkcję evalValue
type EvalValueResult p = Either (p, ErrorMessage) RValue
-- Środowisko funkcji
type FEMap p = Map.Map FSym (Var, Expr p)
-- Środowisko zmiennych
type VVMap = Map.Map Var RValue

-- Wartości obliczone
data RValue
  = RNum Integer
  | RBool Bool
  | RUnit
  | RPair RValue RValue
  | RList [RValue]
  deriving (Show)

-- Tworzy środowisko funkcji
makeFEMap :: [FunctionDef p] -> FEMap p
makeFEMap = Map.fromList . (map (\f -> (funcName f, (funcArg f, funcBody f))))
-- Tworzy środowisko zmiennych
makeVVMap :: [(Var,Integer)] -> VVMap
makeVVMap = Map.fromList . (map (\(x,n) -> (x, RNum n)))

-- Funkcja obliczająca wyrażenia
-- Dla wywołania eval input e przyjmujemy, że dla każdej pary (x, v)
-- znajdującej się w input, wartość zmiennej x wynosi v.
-- Możemy założyć, że wyrażenie e jest dobrze typowane, tzn.
-- typecheck (map fst input) e = Ok
eval :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> EvalResult
eval funs vars e =
  case eval' funs vars e of
    Right n -> Value n
    Left (p, er) -> RuntimeError

-- Zadanie eval' jest takie samo jak typcheck, z tym, że eval' zwraca monadę, więc
-- pozwala na użycie w ciele funkcji eleganckiej notacji monadowej.
-- Funkcja eval jest teraz tylko nakładką na eval', mapującą monadę na TypeCheckResult p.
eval' :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> Either (p, ErrorMessage) Integer
eval' funs vars e = do
  let fenv = makeFEMap funs
      env  = makeVVMap vars
  v <- evalValue fenv env e
  case v of
    RNum n -> return n
    _        -> Left (getData e, "int value should be output of the program but given " ++ show v)

-- Oblicza wartość wyrażenia
evalValue :: FEMap p -> VVMap -> Expr p -> EvalValueResult p
evalValue _ env (EVar p x) = return $ fromJust $ Map.lookup x env
evalValue _ _ (ENum _ n) = return $ RNum n
evalValue _ _ (EBool _ b) = return $ RBool b
evalValue fenv env (EUnary p UNot e) = do
  RBool b <- evalValue fenv env e
  return $ RBool (not b)
evalValue fenv env (EUnary _ UNeg e) = do
  RNum n <- evalValue fenv env e
  return $ RNum (-n)
evalValue fenv env (EBinary _ BAnd e1 e2) = do
  RBool b1 <- evalValue fenv env e1
  if (not b1)
    then return (RBool b1)
    else evalValue fenv env e2
evalValue fenv env (EBinary _ BOr e1 e2) = do
  RBool b1 <- evalValue fenv env e1
  if b1
    then return (RBool b1)
    else evalValue fenv env e2
evalValue fenv env (EBinary _ BEq e1 e2) = evalBinary2 fenv env e1 e2 (==)
evalValue fenv env (EBinary _ BNeq e1 e2) = evalBinary2 fenv env e1 e2 (/=)
evalValue fenv env (EBinary _ BGt e1 e2) = evalBinary2 fenv env e1 e2 (>)
evalValue fenv env (EBinary _ BGe e1 e2) = evalBinary2 fenv env e1 e2 (>=)
evalValue fenv env (EBinary _ BLt e1 e2) = evalBinary2 fenv env e1 e2 (<)
evalValue fenv env (EBinary _ BLe e1 e2) = evalBinary2 fenv env e1 e2 (<=)
evalValue fenv env (EBinary _ BAdd e1 e2) = evalBinary3 fenv env e1 e2 (+)
evalValue fenv env (EBinary _ BSub e1 e2) = evalBinary3 fenv env e1 e2 (-)
evalValue fenv env (EBinary _ BMul e1 e2) = evalBinary3 fenv env e1 e2 (*)
evalValue fenv env (EBinary p BDiv e1 e2) = evalBinary4 fenv env e1 e2 (div) p
evalValue fenv env (EBinary p BMod e1 e2) = evalBinary4 fenv env e1 e2 (mod) p
evalValue fenv env (ELet _ x e1 e2) = do
  v1 <- evalValue fenv env e1
  evalValue fenv (Map.insert x v1 env) e2
evalValue fenv env (EIf p e1 e2 e3) = do
  RBool b <- evalValue fenv env e1
  if b
    then evalValue fenv env e2
    else evalValue fenv env e3
evalValue fenv env (EApp p f e) = do
  v <- evalValue fenv env e
  let (x, eRet) = fromJust $ Map.lookup f fenv
  evalValue fenv (Map.fromList [(x, v)]) eRet
evalValue _ _ (EUnit _) = return RUnit
evalValue fenv env (EPair _ e1 e2) = do
  v1 <- evalValue fenv env e1
  v2 <- evalValue fenv env e2
  return $ RPair v1 v2
evalValue fenv env (EFst _ e) = do
  RPair v _ <- evalValue fenv env e
  return v
evalValue fenv env (ESnd _ e) = do
  RPair _ v <- evalValue fenv env e
  return v
evalValue fenv env (ENil _ _) = return $ RList []
evalValue fenv env (ECons _ e1 e2) = do
  v <- evalValue fenv env e1
  RList l <- evalValue fenv env e2
  return $ RList (v:l)
evalValue fenv env (EMatchL _ e e1 (x, xs, e2)) = do
  RList l <- evalValue fenv env e
  case l of
    []   -> evalValue fenv env e1
    v:vs -> evalValue fenv (Map.insert x v (Map.insert xs (RList vs) env)) e2


-- Funkcje pomocnicze do obliczania wyrażeń binarnych, które...

-- ... przyjmują integery, zwracają bool
evalBinary2 :: FEMap p -> VVMap -> Expr p -> Expr p -> (Integer -> Integer -> Bool) -> EvalValueResult p
evalBinary2 fenv env e1 e2 op = do
  RNum n1 <- evalValue fenv env e1
  RNum n2 <- evalValue fenv env e2
  return $ RBool (op n1 n2)
-- ... przyjmują integery, zwracają integer
evalBinary3 :: FEMap p -> VVMap -> Expr p -> Expr p -> (Integer -> Integer -> Integer) -> EvalValueResult p
evalBinary3 fenv env e1 e2 op = do
  RNum n1 <- evalValue fenv env e1
  RNum n2 <- evalValue fenv env e2
  return $ RNum (op n1 n2)
-- ... przyjmują integery, zwracają integer; dla operatorów, gdzie drugi argument nie może być zerem
evalBinary4 :: FEMap p -> VVMap -> Expr p -> Expr p -> (Integer -> Integer -> Integer) -> p -> EvalValueResult p
evalBinary4 fenv env e1 e2 op p = do
  RNum n1 <- evalValue fenv env e1
  RNum n2 <- evalValue fenv env e2
  if (n2 /= 0)
    then return $ RNum (op n1 n2)
    else Left (p, "Division by 0 exception")
