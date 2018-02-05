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

-- Funkcja sprawdzająca typy
-- Dla wywołania typecheck vars e zakładamy, że zmienne występujące
-- w vars są już zdefiniowane i mają typ int, i oczekujemy by wyrażenia e
-- miało typ int
typecheck :: [Var] -> Expr p -> TypeCheckResult p
typecheck env exp = case (expectInt env exp) of
  Left (p, em) -> Error p em
  Right _ -> Ok

-- Funkcja oczekująca wyrażenia typu Int.
-- W przypadku powodzenia zwraca wartość Right True,
-- w przypadku niepowodzenia zwraca wartość Left (p, opis błędu)
expectInt :: [Var] -> Expr p -> Either (p, ErrorMessage) Bool
expectInt env (EVar p var)
  | elem var env = Right True
  | otherwise = Left(p, "Variable not defined")
expectInt env (ENum _ _) = Right True
expectInt env (EBool p _) = Left (p, "Integer expected, but given bool value.")
expectInt env (EUnary p UNot _) = Left (p, "Integer expected, but given boolean operator.")
expectInt env (EUnary _ UNeg e) = expectInt env e
expectInt env (EBinary p op e1 e2)
  | elem op [BAdd, BSub, BMul, BDiv, BMod] = expectInt env e1 >> expectInt env e2
  | otherwise = Left(p, "Integer expected, but given boolean operator.")
expectInt env (ELet p var e1 e2) = expectInt env e1 >> expectInt (var:env) e2
expectInt env (EIf p e1 e2 e3) =
  expectBool env e1 >> expectInt env e2 >> expectInt env e3

-- Funkcja oczekująca wyrażenia typu Bool. Działa analogicznie do expectInt.
expectBool :: [Var] -> Expr p -> Either (p, ErrorMessage) Bool
expectBool env (EVar p _) = Left (p, "Bool expected, but variable given.")
expectBool env (ENum p _) = Left (p, "Bool expected, but integer given.")
expectBool env (EBool _ b) = Right b 
expectBool env (EUnary _ UNot e) = expectBool env e
expectBool env (EUnary p UNeg _) = Left(p, "Bool expected, but given integer operator")
expectBool env (EBinary p op e1 e2)
  | elem op [BAnd, BOr] = expectBool env e1 >> expectBool env e2
  | elem op [BEq, BNeq] =
    case expectInt env e1 >> expectInt env e2 of
      Left _ -> expectBool env e1 >> expectBool env e2
      x -> x
  | elem op [BLt, BGt, BLe, BGe] = expectInt env e1 >> expectInt env e2
  | otherwise = Left(p, "Bool expected, but given integer operator")
expectBool env (ELet p var e1 e2) = expectInt env e1 >> expectBool (var:env) e2
expectBool env (EIf _ e1 e2 e3) =
  expectBool env e1 >> expectBool env e2 >> expectBool env e3

-- Funkcja obliczająca wyrażenia
-- Dla wywołania eval input e przyjmujemy, że dla każdej pary (x, v)
-- znajdującej się w input, wartość zmiennej x wynosi v.
-- Możemy założyć, że wyrażenie e jest dobrze typowane, tzn.
-- typecheck (map fst input) e = Ok
eval :: [(Var,Integer)] -> Expr p -> EvalResult
eval env exp
  | ret == Nothing = RuntimeError
  | otherwise = Value $ fromJust ret
  where
    ret = evalInt env exp

-- Oblicza wyrażanie typu Int
-- W przypadku błędu (dzielenie przez 0 lub błądy typu) zwraca Nothing.
--
-- Dla wejścia funkcji eval jest przyjęte założenie, że nie wystąpią błędy typu, jednak
-- wykorzystanie alternatywy (<|>) w liniach (*1) i (*2) dopuszcza wystąpienie niezgodności typu.
-- Sytuacja taka wystąpi np. dla wyrażenia "true = false".
evalInt :: [(Var,Integer)] -> Expr p -> Maybe Integer
evalInt env (EVar _ var) = lookup var env
evalInt _ (ENum _ n) = Just n
evalInt env (EUnary _ UNeg e) = negate <$> evalInt env e
evalInt env (EBinary _ BAdd e1 e2) = liftA2 (+) (evalInt env e1) (evalInt env e2)
evalInt env (EBinary _ BSub e1 e2) = liftA2 (-) (evalInt env e1) (evalInt env e2)
evalInt env (EBinary _ BMul e1 e2) = liftA2 (*) (evalInt env e1) (evalInt env e2)
evalInt env (EBinary _ BDiv e1 e2) =
  liftA2 (div) (evalInt env e1) (evalInt env e2 >>= expectNonZeroInt)
evalInt env (EBinary _ BMod e1 e2) =
  liftA2 (mod) (evalInt env e1) (evalInt env e2 >>= expectNonZeroInt)
evalInt env (ELet _ var e1 e2) =
  evalInt env e1 >>= \v1 -> evalInt ((var,v1):env) e2
evalInt env (EIf _ e1 e2 e3) =
  evalBool env e1 >>= \b1 -> case b1 of
    True -> evalInt env e2
    False -> evalInt env e3
evalInt _ _ = Nothing


-- Oblicza wyrażenie typu Bool
-- (Tutaj błąd typu nie występuje)
evalBool :: [(Var,Integer)] -> Expr p -> Maybe Bool
evalBool _ (EBool _ v) = Just v
evalBool env (EUnary _ UNot exp) = not <$> evalBool env exp
evalBool env (EBinary _ BAnd e1 e2) = liftA2 (&&) (evalBool env e1) (evalBool env e2)
evalBool env (EBinary _ BOr e1 e2) = evalBool env e1 >>= \b -> case b of
  True -> Just True
  False -> evalBool env e2
evalBool env (EBinary _ BLt e1 e2) = liftA2 (<) (evalInt env e1) (evalInt env e2)
evalBool env (EBinary _ BLe e1 e2) = liftA2 (<=) (evalInt env e1) (evalInt env e2)
evalBool env (EBinary _ BGt e1 e2) = liftA2 (>) (evalInt env e1) (evalInt env e2)
evalBool env (EBinary _ BGe e1 e2) = liftA2 (>=) (evalInt env e1) (evalInt env e2)
evalBool env (EBinary _ BEq e1 e2) =
  liftA2 (==) (evalInt env e1) (evalInt env e2) <|> liftA2 (==) (evalBool env e1) (evalBool env e2) -- (*1)
evalBool env (EBinary _ BNeq e1 e2) =
  liftA2 (/=) (evalInt env e1) (evalInt env e2) <|> liftA2 (/=) (evalBool env e1) (evalBool env e2) -- (*2)
evalBool env (EIf _ e1 e2 e3) =
  evalBool env e1 >>= \b1 -> case b1 of
    True -> evalBool env e2
    False -> evalBool env e3
evalBool _ _ = Nothing

-- Funkcje pomocnicze
expectNonZeroInt :: Integer -> Maybe Integer
expectNonZeroInt 0 = Nothing
expectNonZeroInt x = Just x
