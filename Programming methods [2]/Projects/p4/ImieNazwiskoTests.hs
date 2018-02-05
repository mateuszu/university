-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający testy.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko}Tests gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module ImieNazwiskoTests(tests) where

-- Importujemy moduł zawierający typy danych potrzebne w zadaniu
import           DataTypes

-- Lista testów do zadania
-- Należy uzupełnić jej definicję swoimi testami

tests :: [Test]
tests =
   [ Test "inc"      (SrcString "input x in x + 1") (Eval [42] (Value 43))
  , Test "undefVar" (SrcString "x")                TypeError
  , Test "noInput" (SrcString "15") (Eval [] (Value 15))
  , Test "simpleExpr" (SrcString "input x y in x + y - 15") (Eval[1,5] (Value (-9)))
  , Test "divByZero" (SrcString "15 mod 0") (Eval [] RuntimeError)
  , Test "wrongTypes" (SrcString "true div false") TypeError
  , Test "smplTest18" (SrcString "if 4 > 2 then if true or false then 2 else 0 else 1") (Eval [] (Value 2))
  , Test "ifTest" (SrcString "if true and false then 0 else 1") (Eval [] (Value 1))
  , Test "ifTest2" (SrcString "if 2 <> 1 and 3>=3 and 2=2 then 1 else 0") (Eval [] (Value 1))
  , Test "ifTest3" (SrcString "if true or false and true then 1 else 0") (Eval [] (Value 1))
  , Test "badInput1" (SrcString "input x in x + y") TypeError
  , Test "firstIf" (SrcString "input x y in if x < y then 20 else 10") (Eval [2, 8] (Value 20))
  , Test "badIf" (SrcString "input x y z in if x then y else z") TypeError
  , Test "secondIf"  (SrcString "input x y in if true then x else y") (Eval [3, 4] (Value 3))
  , Test "firstLet"  (SrcString "input x in let x = 3 * 7 in x + 5") (Eval [56] (Value 26))
  , Test "secondLet" (SrcString "input x y z in let z = y div 4 in x + 42*z") (Eval [78, 16, 4] (Value 246))
  , Test "thirdIf" (SrcString "input x in if x >= y then 20 else 6")    TypeError
  , Test "badExpr1" (SrcString "input a b c in a + b + true div c")   TypeError
  , Test "negNum" (SrcString "-17") (Eval [] (Value (-17)))
  , Test "ifAndLet" (SrcString "input x y z in if x = y then let z = 55 in 147 else 73") (Eval [3, 3, 17] (Value 147))
  , Test "thirdLet" (SrcString "input x in let x = 5 in x*x") (Eval [3] (Value 25))
  , Test "badExpr2" (SrcString "78 * true")    TypeError
  , Test "badIf" (SrcString "input x in if 7 then true else 98")    TypeError
  , Test "badInput2" (SrcString "input x in true or false")             TypeError
  , Test "ifandAnd" (SrcString "input a b in if a >= 7 and b < 8 then a div b else a - b") (Eval[3,9] (Value (-6)))
  , Test "ifInIf" (SrcString "input x y in if x <> y then if y = 455 then x mod 10 else x - y else 5 * y") (Eval [3,3] (Value 15))
  , Test "ifTrueOrFalseAndTrue"      (SrcString "if true or false and true then 42 else 100") (Eval [] (Value 42))
  , Test "lotsOfLets" (SrcString "input x in let y = x in let z = y in let k = z in if x = z and y = z then 42 else 123  ") (Eval [123] (Value 42))
  , Test "unaryOnBool1"      (SrcString "- true")         TypeError
  , Test "unarnyOnBool2"      (SrcString "- false")       TypeError
  , Test "or1" (SrcString "3 or 5") TypeError
  , Test "or2" (SrcString "3 or false") TypeError
  , Test "or3" (SrcString "true or 2") TypeError
  , Test "and1" (SrcString "3 and 5") TypeError
  , Test "and2" (SrcString "3 and false") TypeError
  , Test "and3" (SrcString "true and 2") TypeError
  , Test "eq1" (SrcString "true > false") TypeError
  , Test "eq2" (SrcString "true > 0") TypeError
  , Test "eq3" (SrcString "true = false") TypeError
  , Test "eq4" (SrcString "true > false") TypeError
  , Test "eq5" (SrcString "true > 0") TypeError
  , Test "eq6" (SrcString "true <> false") TypeError
  , Test "eq7" (SrcString "true < 0") TypeError
  , Test "eq8" (SrcString "true < false") TypeError
  , Test "eq9" (SrcString "true <= false") TypeError
  , Test "eq10" (SrcString "true >= 0") TypeError
  -- NOWE TESTY
  , Test "badType1" (SrcString "fun bad_type(u:unit):int = true input x in bad_type()") TypeError
  , Test "badType2" (SrcString "fun badtype(n:bool):int = n - 3 input n in badtype(n)") TypeError
  , Test "sumOfList" (SrcString "fun sum_list(l:int list):int = match l with | [] -> 0 | x::xs -> x + sum_list xs input x in sum_list([1,2,3]:int list)") (Eval[1] (Value 6))
  , Test "compareBools" (SrcFile "simplelist.pp5")    TypeError
  , Test "minimum" (SrcFile "minimum.pp5") (Eval [52,41] (Value 41))
  , Test "multiplicationOfPair" (SrcFile "pairmult.pp5") (Eval [32,7] (Value 224))
  , Test "sumOfpair" (SrcFile "pairsum.pp5") (Eval[1,2,3,4] (Value 7))
  , Test "badTypeOfList" (SrcFile "badtypeoflist.pp5") TypeError
  , Test "errorBoolInTheEnd" (SrcString "fun boo(a:int):bool = if a > 2 then true else false input x in boo(x)") TypeError
  , Test "division" (SrcString "fun f(u:int):int = u div 0 input a in f(a) ") (Eval [10] (RuntimeError))
  , Test "length" (SrcFile "length.pp5") (Eval[1,2,3,4] (Value 4))
  , Test "firstTest" (SrcString "if fst (2,3) = 2 then 4 else 5") (Eval[] (Value 4))    
  , Test "errorBoolToInt" (SrcString "fun boo(a:int):bool = if a > 2 then 1 else 0 in boo(2)") TypeError
  , Test "wrongArgType" (SrcString "fun boo(a:bool):int = if a=true then 1 else 0 in boo(2)") TypeError
  , Test "functionUnit" (SrcString "fun boo(u:unit):int = 1 in boo()") (Eval[] (Value 1))
  , Test "factorialFunction" (SrcString "fun fac(a:int):int = if a = 1 then 1 else a * fac(a-1) input a in fac(a)") (Eval[3] (Value 6))
  , Test "simpleFunction1" (SrcString "fun function(n : int) : int = if n < 10 then (n-1)*5 else function(n-3) * function(n-2) input n in function(n)") (Eval[5](Value 20))
  , Test "simpleFunction2" (SrcFile "justfunction.pp5") (Eval [17] (Value 24))
  ]
