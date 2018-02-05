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
{-
tests :: [Test]
tests =
  [ Test "inc"      (SrcString "input x in x + 1") (Eval [42] (Value 43))
  , Test "undefVar" (SrcString "x")                TypeError
  ]
-}
tests :: [Test]
tests =
  [ Test "inc"      (SrcString "input x in x + 1") (Eval [42] (Value 43))
  , Test "undefVar" (SrcString "x")                TypeError
  -- MOJE TESTY <--------------------
  , Test "wrongTypes" (SrcString "2 + true") TypeError
  , Test "testEnum" (SrcString "5 = true") TypeError
  , Test "testEvalEnum" (SrcString "input x in x") (Eval[10] (Value 10))
  , Test "testTwoVars" (SrcString "input x y in x + y") (Eval[1,2] (Value 3))
  , Test "testLet" (SrcString "let x = 5 in true") TypeError
  , Test "testLetInput" (SrcString "input x in let y = 5 in x + y") (Eval[1] (Value 6))
  , Test "WrongTyped" (SrcString "false * 1") TypeError
  , Test "ifWithDivByZero" (SrcString "if x<5 then x + 6 else x div 0") (Eval[8] (RuntimeError))
  , Test "divByZero" (SrcString "8 div 0") (Eval [] (RuntimeError))
  , Test "typeError" (SrcString "true = 15") TypeError
  , Test "typeError2" (SrcString "not 15") TypeError
  , Test "typeError3" (SrcString "- false") TypeError
  , Test "ifandAnd" (SrcString "input a b in if a >= 7 and b < 8 then a div b else a - b") (Eval[3,9] (Value (-6)))
  , Test "ifandOr" (SrcString "input a b in if a < 11 or b > 51 then 7 * a - 5 * b else 2 * a + 3 * b") (Eval[2,1] (Value 9))
  , Test "ifInIf" (SrcString "input x y in if x <> y then if y = 455 then x mod 10 else x - y else 5 * y") (Eval [3,3] (Value 15))
  , Test "multp"      (SrcString "input x y in x * y") (Eval [10,20] (Value 200))
  , Test "add"       (SrcString "2+2") (Eval []  (Value 4))
  , Test "undefVar" (SrcString "x")                TypeError
  , Test "liczba"(SrcString "2")(Eval [] (Value 2))
  , Test "dzielenie_przez_0" (SrcString "1 div 0") (Eval[] RuntimeError)
  , Test "zmienna_dzielenie_przez_0" (SrcString "input x in x div x") (Eval[0] RuntimeError)
  , Test "x_plus_true" (SrcString "input x in x + true") TypeError
  , Test "za_duzo_zmiennych" (SrcString "input x in x + y") TypeError
  , Test "1/0_ktore_nie_bedzie_wykonywane" (SrcString "if false then 1 div 0 else 0") (Eval [] (Value 0))
  , Test "return_true" (SrcString "true") TypeError
  , Test "sum_bool" (SrcString "true + false") TypeError
  , Test "or_int" (SrcString "3 or 4") TypeError
  , Test "minus_bool" (SrcString "- true") TypeError
  , Test "2_zmienne" (SrcString "input x y in x + y") (Eval[1, 2] (Value 3))
  , Test "mod_0" (SrcString "3 mod 0") (Eval [] RuntimeError)
  , Test "undefVar" (SrcString "x")                TypeError
  , Test "minus_wartosc"      (SrcString "-12") (Eval [] (Value (-12)))
  , Test "if_bool_then_wartosc"      (SrcString "if true then 42 else 1 div 0") (Eval [] (Value 42))
  , Test "if_bool_else_wartosc"      (SrcString "if not true then 1 div 0 else 42") (Eval [] (Value 42))
  , Test "if_not_true"      (SrcString "if not true then 22 else 42") (Eval [] (Value 42))
  , Test "if_true_or_false"      (SrcString "if true or false then 42 else 1 div 0") (Eval [] (Value 42))
  , Test "if_true_and_false"      (SrcString "if true and false then 1 div 0 else 42") (Eval [] (Value 42))
  , Test "if_not_false"      (SrcString "if not false then 21 * 2 else 1 div 0") (Eval [] (Value 42))
  , Test "if_true_and_true"      (SrcString "if true and true and false then 100 else 42") (Eval [] (Value 42))
  , Test "if_true_or_false_and_true"      (SrcString "if true or false and true then 42 else 100") (Eval [] (Value 42))
  , Test "priorytet_op1"      (SrcString "2 + 2 * 2") (Eval [] (Value 6))
  , Test "priorytet_op2"      (SrcString "2 + 2 div 2 + 2 * 2") (Eval [] (Value 7))
  , Test "if_true"      (SrcString "if true then 42 else 0") (Eval [] (Value 42))
  , Test "if_false"      (SrcString "if false then 0 else 42") (Eval [] (Value 42))
  , Test "if_mniejsze_rowne"      (SrcString "if 1 <= 1 then 42 else 1234") (Eval [] (Value 42))
  , Test "if_wieksze_rowne"      (SrcString "if 1 >= 1 then 42 else 1234") (Eval [] (Value 42))
  , Test "if_mniejsze_rowne2"      (SrcString "if 1 <= 0 then 1234 else 42") (Eval [] (Value 42))
  , Test "if_wieksze_rowne2"      (SrcString "if 1 >= 0 then 42 else 1234") (Eval [] (Value 42))
  , Test "op1"      (SrcString "4 mod 2") (Eval [] (Value 0))
  , Test "op2"      (SrcString "21 * 2") (Eval [] (Value 42))
  , Test "zmienne_cztery"      (SrcString "input x y z k in x div 2 + y + z - k ") (Eval [6, 7, 53, 21] (Value 42))
  , Test "if_zmienna1"      (SrcString "input x in if 10 > 9 + x then 100 else x") (Eval [2] (Value 2))
  , Test "if_zmienna2"      (SrcString "input x in if 10 > 9 + x then 100 else x") (Eval [0] (Value 100))
  , Test "if_zmienne1"      (SrcString "input x y in if 10 - y > 9 + x then 100 else y - x") (Eval [1, 0] (Value (-1)))
  , Test "if_zmienne2"      (SrcString "input x y in if 10 - y > 9 + x then 100 else y - x") (Eval [4, 6] (Value 2))
  , Test "if_zmienne3"      (SrcString "input x y in if x > y - x then 100 else y div x") (Eval [8, 8] (Value 100))
  , Test "if_zmienne4"      (SrcString "input x y in if x > y - x then 100 else (y - 10) div x") (Eval [10, 110] (Value 10))
  , Test "if_zmienne5"      (SrcString "input x y in if 10  > 9 + x then 100 else y - x") (Eval [4, 6] (Value 2))
  , Test "if_let_if_then"      (SrcString "input x in let y = if 10 < 9 + x then 100 else 5 in if y < 6 then 42 else 33") (Eval [1] (Value 42))
  , Test "if_let_if_else"      (SrcString "input x in let y = if 10 < 9 + x then 100 else 5 in if y < 6 then 33 else 42") (Eval [2] (Value 42))
  , Test "duzo_let"      (SrcString "input x in let y = x in let z = y in let k = z in k") (Eval [42] (Value 42))
  , Test "duzo_let_zmienna"      (SrcString "input x in let y = x in let z = y in let k = z in x") (Eval [42] (Value 42))
  , Test "duzo_let_if"      (SrcString "input x in let y = x in let z = y in let k = z in if x = z and y = z then 42 else 123  ") (Eval [123] (Value 42))
  , Test "duzo_let_zmienne_if"      (SrcString "input x y in let y = x in let z = y in let k = z in if x = z and y = z then 42 else 123  ") (Eval [123, 23] (Value 42))
  , Test "let_let"      (SrcString "input x in let x = (let y = 1 in y) in x") (Eval [123] (Value 1))
  , Test "niedozowlone_mod1"      (SrcString "input x y z in z + z div 5 + y + x * 2") (Eval [3, 2, 10] (Value 20))
  , Test "niedozowlone_mod2"      (SrcString "input x y in if 10 mod 0 > 9 + x then 100 else y - x") (Eval [4, 6] RuntimeError)
  , Test "niedozwolone_dzielenie"      (SrcString "1 div 0") (Eval [] RuntimeError)
  , Test "niedozowlone_mod4"      (SrcString "1 mod 0") (Eval [] RuntimeError)
  , Test "niedozowlone_mod5"      (SrcString "if false then 1 else 1 mod 0") (Eval [] RuntimeError)
  , Test "niedozowlone_mod6"      (SrcString "if true then 1 mod 0 else 1") (Eval [] RuntimeError)
  , Test "unarny_na_true"      (SrcString "- true")                TypeError
  , Test "unarny_na_false"      (SrcString "- false")                TypeError
  , Test "dodawanie_liczby_i_false"      (SrcString "1 + false")                TypeError
  , Test "dodawanie_booli"      (SrcString "true + false")                TypeError
  , Test "dzielenie_booli"      (SrcString "false div false")                TypeError
  , Test "mnozenie_booli"      (SrcString "false * false")                TypeError
  , Test "wynik_bool1"      (SrcString "42 = 42")                TypeError
  , Test "wynik_bool2"      (SrcString "43 = 42")                TypeError
  , Test "wynik_bool3"      (SrcString "43 >= 42")                TypeError
  , Test "wynik_bool4"      (SrcString "input x in true")                TypeError
  , Test "wynik_bool5"      (SrcString "input x in y = 42")                TypeError
  , Test "wynik_bool6"      (SrcString "input x in y = x")                TypeError
  , Test "wynik_bool7"      (SrcString "input x y z in y = x")                TypeError
  , Test "wynik_bool8"      (SrcString "if true then true else false")                TypeError
  , Test "wynik_bool9"      (SrcString "if true then true else 2")                TypeError
  , Test "let_or"      (SrcString "let x = 1 or 1 in 1")                TypeError
  , Test "let_x_bool"      (SrcString "let x = true in x")                TypeError
  , Test "zlyTypOperacji1" (SrcString "true*1") TypeError
  , Test "zlyTypOperacji2" (SrcString "false*1") TypeError
  , Test "zlyTypOperacji3" (SrcString "1*true") TypeError
  , Test "zlyTypOperacji4" (SrcString "1*false") TypeError
  , Test "zlyTypOperacji5" (SrcString "true + false") TypeError
  , Test "zlyTypOperacji6" (SrcString "true mod false") TypeError
  , Test "zlyTypOperacji7" (SrcString "true div false") TypeError
  , Test "zlyTypOperacji8" (SrcString "0 div false") TypeError
  , Test "zlyTypOperacji9" (SrcString "0 mod false") TypeError
  , Test "zlyTypOperacji10" (SrcString "false div 5") TypeError
  , Test "zlyTypOperacji11" (SrcString "true mod 5") TypeError

  , Test "zlyTypUnarny1" (SrcString "-true") TypeError
  , Test "zlyTypUnarny2" (SrcString "-false") TypeError
  , Test "zleNegowanie1" (SrcString "not 5") TypeError

  , Test "if1" (SrcString "if true then false else false") TypeError
  , Test "if2" (SrcString "if true then 1 else false") TypeError
  , Test "if3" (SrcString "if 5 then 1 else 1") TypeError
  , Test "if4" (SrcString "if true then false else 3") TypeError
  , Test "if5" (SrcString "input x in if x > 0 then 5 else 3") (Eval [1] (Value 5))
  , Test "div0" (SrcString "input x in 1 div x") (Eval [0] (RuntimeError))

  , Test "let1" (SrcString "let x = true in false") TypeError
  , Test "let1" (SrcString "let x = 5 in false") TypeError
  , Test "let2" (SrcString "input x in let x = 3 in true+x") TypeError
  , Test "let3" (SrcString "input x in let x = 4 in true+false") TypeError
  , Test "let4" (SrcString "input x in let x = true in 4+x") TypeError
  , Test "let5" (SrcString "input x in let x = x+0 in 4+x") (Eval [2] (Value 6))

  , Test "dodawanie"   (SrcString "input x in x + 1") (Eval [42] (Value 43))
  , Test "odejmowanie" (SrcString "input x in x - 1") (Eval [42] (Value 41))
  , Test "mnozenie" (SrcString "input x in x * 2") (Eval [42] (Value 84))
  , Test "dzielenie" (SrcString "input x in x div 5") (Eval [10] (Value 2))
  , Test "modulo" (SrcString "input x in x mod 2") (Eval [3] (Value 1))

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

  , Test "program1" (SrcString "input x in if true then let x = x+0 in x + 3 else let x = x+0 in x + 3") (Eval [5] (Value 8))
  , Test "program2" (SrcString "input x in if false then let x = x+0 in x + 3 else let x = x+0 in x + 1") (Eval [5] (Value 6))
  , Test "undefVar" (SrcString "x")                TypeError
  , Test "smplTest1" (SrcString "2")               (Eval [] (Value 2))
  , Test "smplTest2" (SrcString "-2 + 2")          (Eval [] (Value 0))
  , Test "smplTest3" (SrcString "-2 div 0")          (Eval [] RuntimeError)
  , Test "smplTest4" (SrcString "1 - 2 + 3")       (Eval [] (Value 2))
  , Test "smplTest5" (SrcString "1 div 2")         (Eval [] (Value 0))
  , Test "smplTest6" (SrcString "14 mod 10")       (Eval [] (Value 4))
  , Test "smplTest7" (SrcString "11 mod 10 * 10")  (Eval [] (Value 10))
  , Test "smplTypeTest8" (SrcString "2 + true")        TypeError
  , Test "undefVar" (SrcString "x")                TypeError
  , Test "arithmetics" (SrcString "2 + 2") (Eval [] (Value 4))
  , Test "simpleInput"  (SrcString "input x y in x + y") (Eval [17, 42] (Value 59))
  , Test "badInput1" (SrcString "input x in x + y") TypeError
  , Test "firstIf" (SrcString "input x y in if x < y then 20 else 10") (Eval [2, 8] (Value 20))
  , Test "badIf" (SrcString "input x y z in if x then y else z") TypeError
  , Test "secondIf"  (SrcString "input x y in if true then x else y") (Eval [3, 4] (Value 3))
  , Test "firstLet"  (SrcString "input x in let x = 3 * 7 in x + 5") (Eval [56] (Value 26))
  , Test "secondLet" (SrcString "input x y z in let z = y div 4 in x + 42*z") (Eval [78, 16, 4] (Value 246))
  , Test "Number" (SrcString "2") (Eval [] (Value 2))
  , Test "thirdIf" (SrcString "input x in if x >= y then 20 else 6")    TypeError
  , Test "badExpr1" (SrcString "input a b c in a + b + true div c")   TypeError
  , Test "negNum" (SrcString "-17") (Eval [] (Value (-17)))
  , Test "ifAndLet" (SrcString "input x y z in if x = y then let z = 55 in 147 else 73") (Eval [3, 3, 17] (Value 147))
  , Test "thirdLet" (SrcString "input x in let x = 5 in x*x") (Eval [3] (Value 25))
  , Test "badExpr2" (SrcString "78 * true")    TypeError
  , Test "badIf" (SrcString "input x in if 7 then true else 98")    TypeError
  , Test "badInput2" (SrcString "input x in true or false")             TypeError

  ]
