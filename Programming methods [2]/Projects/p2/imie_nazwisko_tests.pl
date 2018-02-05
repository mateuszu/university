% Definiujemy moduł zawierający testy.
% Należy zmienić nazwę modułu na {imie}_{nazwisko}_tests gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez wielkich liter oraz znaków diakrytycznych
:- module(imie_nazwisko_tests, [tests/3]).

% Zbiór faktów definiujących testy
% Należy zdefiniować swoje testy

% ----------------------------- PROGRAM TESTS BEGIN ----------------------------
tests(srcpos, input("def main(_) = 1"),
  program([def(main, wildcard(file(test, 1, 10, 9, 1)), num(no, 1))])).
tests(empty_program, input(""), program([])).
tests(def_with_single_var,input("def test(Var) = 1"), program([def(test, var(no, 'Var'), num(no, 1))])).
tests(simple_let, input("def test(A) =
    let C = 1 in A & C"), program([def(test, var(no, 'A'), let(no, var(no, 'C'),  num(no, 1), op(no, '&', var(no,'A'),var(no,'C'))))])).
% ----------------------------- PROGRAM TESTS END ------------------------------

% ---------------------------- NO TESTS BEGIN ----------------------------------
tests(invalid, input("def main()"), no).
tests(def_with_invalid_var, input("def main(aaa)"), no).
tests(nothing_after_equal_sign, input("def main(A,B) = "), no).
tests(wrong_def_identifier1,input("def 9test(_) = 1"),no).
tests(wrong_def_identifier2,input("def test#(_) = 1"),no).
tests(def_in_def,input("def smth(_) = def smthg(_)"),no).
% ---------------------------- NO TESTS END ------------------------------------

% --------------------------- YES TESTS BEGIN ----------------------------------
tests(adder, file('adder.hdml'), yes).
tests(simple_comparison, input("def compare(A,B) = if A > 0 then B else []"), yes).
tests(half_adder, input("def half_adder(A,B) = A & B, A ^ B"), yes).
tests(nested_function, input("def nested_function(A,B,C) = let P,O = nested_function(A, B, C)
in O | P & C"), yes).
tests(def_with_multi_variables, input("def test(Var1, Var2, Var3) = 3"), yes).
tests(multiple_def,file('several_defs.hdml'),yes).
% --------------------------- YES TESTS END ------------------------------------
