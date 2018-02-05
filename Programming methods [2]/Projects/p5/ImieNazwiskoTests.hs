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
  [ Test "test1" (SrcString "let x=17 in (fn(y:int)-> x*y) ") TypeError
  , Test "test2" (SrcString "fun f(x : int): int = x input x in let f = x in f f") TypeError
  , Test "test3" (SrcString "fun f(x : int): int = x input x in let f = x in f") (Eval[34] (Value 34))
  , Test "test4" (SrcString "fun function(functiooon:int->int): int = functiooon(8) in let x = fn(x:int) -> x+1 in function(x)") (Eval[] (Value 9))
  , Test "test5" (SrcString "fun g(a:int):int = a-3 input b in if b<9 then g b else let d=fn(c:int) -> g(c)+8 in d b") (Eval[18] (Value 23))
  , Test "test6" (SrcString "fun function(a:int):int = a - 3 input y in let x = fn(b:int) -> function(b)+5 in x y") (Eval[10] (Value 12))
  , Test "test7" (SrcString "fun g(a:int):int = let c = fn(b:int) -> b+5 in c a input d in g d") (Eval[3] (Value 8))
  , Test "test8" (SrcString "let b = 107 in let f = fn(a:int) -> a-b in g 77") TypeError
  , Test "test9"      (SrcString "input x in fn(x:int)->x mod 2") (Eval [7] (Value 1))
  , Test "test10" (SrcString "input x in let y = if x > 0 then true else false in let z = fn(y:bool) -> not y in if z y then 6 else 4") (Eval[3] (Value 4))
  , Test "test11" (SrcString "fun g(a:int):int = a*8 input b in if b<5 then g b else let d=fn(c:int) -> g(c)-88 in d b") (Eval[3] (Value 24))
  , Test "test12" (SrcString "let x=35 in (fn(y:int)-> x-y) 5") (Eval [] (Value 30))
  , Test "test13" (SrcString "let y = 22 in let function = fn(x:int) -> x+y in function 23") (Eval[] (Value 45))
  , Test "test14" (SrcString "fun foo(u:unit):int->int = fn (x:int)-> x mod 3 input d in foo() d") (Eval[10] (Value 1))
  , Test "test15" (SrcString "input x in if 2 then true else 5") TypeError
  ]
