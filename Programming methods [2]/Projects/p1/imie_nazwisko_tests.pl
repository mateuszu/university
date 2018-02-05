% Definiujemy moduł zawierający testy.
% Należy zmienić nazwę modułu na {imie}_{nazwisko} gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez wielkich liter oraz znaków diakrytycznych

:- module(imie_nazwisko_tests, [resolve_tests/5, prove_tests/4]).


% definiujemy operatory ~/1 oraz v/2
:- op(200, fx, ~).
:- op(500, xfy, v).

% Zbiór faktów definiujących testy dla predykatu resolve
% Należy zdefiniować swoje testy
resolve_tests(simple_test1, q, p v q, ~q v r, p v r).
resolve_tests(simple_test2, q, p v ~p v q, p v ~p v ~q, p v ~p).
resolve_tests(simple_test3, p, p v ~q, ~p v ~q, ~q).
resolve_tests(simple_test4, p, p v q, ~p v ~q, q v ~q).
resolve_tests(whitebook1_by_p, p, p v q v s, ~p v ~q v s, q v ~q v s).
resolve_tests(whitebook1_by_q, q, p v q v s, ~p v ~q v s, s).
resolve_tests(whitebook2_by_r, r, p v r, ~r v ~s, p v ~s).
resolve_tests(one_neg_one_lit_by_p, p, p, ~p, []).
resolve_tests(two_neg_two_lits_by_p, p, p v q, ~p v ~q, q v ~q).
resolve_tests(three_lits, p, p v q v r, ~p v ~q v ~r, q v r v ~q v ~r).
resolve_tests(big_resolve, p, p v q v r v s v t v u v w v z, ~p v q v r v s v t v u v w v z, q v r v s v t v u v w v z).
resolve_tests(tricky_resolve, z, p v q v r v s v t v u v w v z, p v q v r v s v t v u v w v ~z, p v q v r v s v t v u v w).
resolve_tests(doubled_lit, p, p v p, ~p v ~p v q, q).
resolve_tests(more_negs_than_non_negs, p, p v q, ~p v ~p v ~ p v ~p v q, ~p v q).
resolve_tests(more_non_negs_than_negs, p, p v p v p v q , ~p v q, p v q).

% Zbiór faktów definiujących testy dla predykatu prove
% Należy zdefiniować swoje testy

prove_tests(example, validity, [p v q v ~r, ~p v q, r v q, ~q, p], unsat).
prove_tests(excluded_middle, validity, [p v ~p], sat).
prove_tests(containg_empty_clause, validity, [[]], unsat).
prove_tests(trivial_conjunctive, validity, [~p, p], unsat).
prove_tests(one_literal, validity, [p], sat).
prove_tests(one_negation, validity, [~p], sat).
prove_tests(duplicated_clause, validity, [p v q, p v q], sat).
prove_tests(xor, validity, [p v q, ~p v ~q], sat).
prove_tests(absorption_1, validity, [p v p, q], sat).
prove_tests(absorption_2, validity, [p, p v q], sat).
prove_tests(de_morgan_1, validity, [ ~p, q], sat).
prove_tests(de_morgan_2, validity, [~p v ~q], sat).
prove_tests(tautology, validity, [p v ~p, q v ~q], sat).
prove_tests(negation_of_implication, validity, [p, ~q], sat).
prove_tests(three_literals_disjunction, validity, [p v q v r], sat).
prove_tests(three_literals_conjunctive, validity, [p,q,r], sat).
prove_tests(ten_literals_conjunctive, validity, [p,q,r,s,t,u,w,z,n,m], sat).
prove_tests(ten_negated_literals_conjunctive, validity, [~p,~q,~r,~s,~t,~u,~w,~z,~n,~m], sat).
prove_tests(whitebook_longer_one, validity, [ ~p v q, ~p v ~r v s, ~q v r, p, ~s ], unsat).
prove_tests(whitebook_longer_one_1, validity, [ p v r, ~r v ~s, q v s, q v r, ~p v ~q, s v p ], unsat).
prove_tests(whitebook_longer_one_2, validity, [ p v r, ~r v ~s, q v s, q v r, ~p v ~q, s v ~p ],sat).
prove_tests(whitebook_longer_one_3, validity, [ p v q v r, ~r v ~q v ~p, ~q v r, ~r v p ], sat).
prove_tests(tricky_conjunctive_1, validity, [p, ~p, q, ~q, r, ~r], unsat).
prove_tests(tricky_conjunctive_2, validity, [p v q, r v s, ~p v ~q, ~p v ~r, ~p v ~s, ~q v ~r, ~q v ~s, ~r v ~s], unsat).
prove_tests(big_one, validity,  [p v q v r v s, p v q v r v ~s, p v q v ~r v ~s, p v ~q v r v s, p v ~q v r v ~s, p v ~q v ~r v ~s,
~p v q v r v s, ~p v q v r v ~s, ~p v q v ~r v s, ~p v q v ~r v ~s, ~p v ~q v r v s, ~p v ~q v r v ~s, ~p v ~q v ~r v s, ~p v ~q v ~r v ~s, r, ~r], unsat).
prove_tests(tricky_conjunctive_2_performance, performance, [p v q, r v s, ~p v ~q, ~p v ~r, ~p v ~s, ~q v ~r, ~q v ~s, ~r v ~s], unsat).
prove_tests(ten_clauses_conjunctive_perform, performance, [p,q,r,s,t,u,w,z,n,m], sat).
prove_tests(big_test_performance, performance, [p v q, r v s, ~p v ~q, ~p v ~r, ~p v ~s, ~q v ~r, ~q v ~s, ~r v ~s, p v q, r v s, ~p v ~q, ~p v ~r, ~p v ~s,
 ~q v ~r, ~q v ~s, ~r v ~s, p, ~p, q, ~q, r, ~r, p, ~p, q, ~q, r, ~r, p, ~p, q, ~q, r, ~r], unsat).
prove_tests(big_test1_performance, performance, [~r v b, ~r v k v p v g, ~r v q v ~l v h v m, ~q v ~m v ~o v i v b v ~n v b v n, ~q v ~h v ~p v m v b v j v j v o, ~p v ~e v ~i v f v n v ~c v d v r, ~p v f v ~m v l v i, ~o v ~l, ~o v c v ~m v ~e v q v ~b v k v h, ~n v ~n v ~g v q v o, ~n v ~d v ~c v ~m v c v ~p v f v ~b v ~e v i, ~n v ~b v ~e v ~j, ~n v h v d, ~m v ~e v ~k, ~m v p v ~h v b v ~l v ~n v d v f v m, ~l v ~o, ~l v ~n v q v ~f, ~l v b, ~k v ~c v k v ~m v ~p, ~k v d v e, ~k v d v m, ~j v ~l v f v e v b, ~j v i, ~j v k v d v j v ~h v p v l, ~j v p v ~n v ~k, ~j v q v n, ~i v ~r v ~h v ~r v ~j, ~i v f v ~b v ~r v f v j v ~b v k v q, ~i v i v b v d v ~m v ~r v ~m v q v ~g, ~i v q v ~c v i v d v ~p v ~e v ~g, ~h v ~p v r v r v c v f, ~g v ~g v ~j v ~c v b v f v ~f, ~g v c v ~q, ~f v ~k v p v ~c v d v m v e v f, ~f v ~c v ~e v f v ~f v f v d v d v ~f, ~f v m v ~n, ~e v ~o v f v ~h v ~o v ~f v l, ~e v b v ~c v ~j v ~o v o, ~d v ~f v g v ~l v ~d v ~i, ~d v i v ~i v r v i v e v e v f v ~e, ~c v ~i v ~c v r v ~b v m, ~c v ~f v f v ~f v ~g v ~n v l v b v l v ~q, ~c v d, ~c v f v ~m v q, ~c v i v o, ~c v j v ~p, ~c v l v ~l v g v ~e v ~o v h, ~b v n v c v g v f, b v ~i v b, c v ~g v ~d v o v ~g v ~j v ~h, c v p v q, d v ~m v ~g v ~o v ~o v ~e v g v ~p v ~q v j, d v ~j v j v ~r v c v f v ~c v ~j, d v ~b v ~e v ~g v ~r v f, e v ~p v ~e v e v o v g v m v ~j v h, e v ~l v ~i v ~l, e v ~i v c v h, e v ~f v ~h v m v g v ~e v j v b v ~k, e v e v ~e v h v ~d, e v n v ~j v j v ~i v j, f v ~p, f v ~m v ~o v g v ~k v g v ~g v ~g v r v ~h, f v ~f v ~e v m v ~b v ~r v ~m v g v ~l v ~l, f v ~e v o v f v f v f v ~q v d v e, g v ~j v d v b v ~d v ~f, h v ~l v ~h v k v ~i v l v p, h v d, h v q, i v ~o v ~h v b v ~l, i v ~i v b v g v g v ~n v h, i v ~h v c, i v p v ~q, j v c v ~q v ~l v n v j, j v n v i v ~o, k v e v ~b v e v ~r v ~r v k v ~q v ~e, l v ~k v f v ~f v ~f v c, l v ~b v g, l v m v ~p v k, m v b v i, m v e, m v q v p v ~c v c v p v ~q v f v ~l, n v ~d, n v ~d v c v ~c, n v ~b v ~d v ~k v ~p v ~q v n v ~r v ~p, n v d v g v c v ~n v k v n, o v ~o v ~r v i v g v d v ~c v ~j v d v ~n, o v ~j v ~c v l v j v ~k v ~l, o v ~c v m v g, o v d v ~d v ~j v c v ~f v ~k v ~q, o v h v j v r v ~i v f v f v n v ~c v b, p v ~f v ~i v ~i v d v m v ~r, p v g v g v ~m v q v ~f, p v i v ~q v i v ~q v i v ~p v p, p v q, q v ~f v ~c v i, q v n v c v n v j v ~n v q v e, q v q v g v l v ~i v k v i v ~l v m v m, r v ~d v m v ~b v ~o v g v ~c v ~n v ~o, r v j, r v r], sat).
prove_tests(big_test2_performance, performance, [~r v ~r v ~q v ~l v o v l v ~c v ~i v r, ~r v ~b v ~i v q, ~r v m v ~l v c, ~q v ~r v m, ~q v ~n v ~r v p v ~m v e v ~p v ~g, ~q v r v ~m v f v f v i v q v ~d v ~l, ~p v ~f v ~k v e v j v c v j v e, ~p v ~b, ~p v e v f v g, ~o v ~l, ~o v d v ~g v r, ~n v ~n v ~o, ~n v d v e v ~p v g v ~f, ~n v g v ~l v ~g v ~q v ~h v ~n v k v ~q, ~m v ~j, ~l v ~n, ~l v ~d v ~k, ~l v l v ~l, ~j v ~k v b v p, ~j v ~f v ~n v ~q v ~j, ~j v m v h v ~h v ~h v ~p v ~j, ~h v ~g v ~l v ~r, ~h v ~g v ~i, ~g v ~p v ~g v ~q v c v ~j v l v n, ~f v ~q v ~b v m, ~f v ~o v d v ~j v i v k v ~m v i v e v ~q, ~f v k v o v n v h v k v ~n v h v ~l, ~f v m v d v r v ~k v o v o v e, ~f v m v q v m v ~b v f v ~q v r, ~f v n v n v q v n, ~e v ~q v k v ~q v ~o v j v ~q v i, ~e v ~c v b v g v q v i v ~g v ~r, ~e v c v r v ~c, ~e v e v ~o v q v ~h v ~b v i v o v b v n, ~e v f v ~n, ~e v r v m v ~d, ~d v ~b v g v o v ~o v c v b v k, ~d v ~b v o v b v r v ~o v b, ~c v ~i, ~c v p v ~p v d v ~c v ~m, ~b v ~l v ~b v l v i, ~b v ~h, ~b v b v ~j v ~g v ~n v r v ~h, ~b v c v m v ~g v ~i, ~b v q v ~q, b v ~n v ~j v ~d v ~o v ~c v o, b v e v ~f v ~m v ~i, b v m v ~g v h v h v ~i v n v q, c v ~p v ~n v ~r v ~r v ~o v ~b v ~h v k v f, c v ~d v ~k v ~l v q v o, c v l v d v ~k, c v r, d v ~r v ~f v ~j v ~p, d v ~l v ~c v ~k, d v q v o v ~j v ~i, e v ~p v ~c v ~d v e v ~h v k, e v ~p v h v ~g v ~q v ~m v r v c, e v ~g v ~h v k v ~o v ~e, e v b v ~b v ~r v f v m v ~p v n v ~f, e v f v g v ~i v ~f, f v ~k v l v ~e v ~i v l v q v j v f v r, f v ~h v g v ~c v h v ~h v g v ~i v ~i, f v e, g v ~r v ~e v l v ~n v f, g v b v q v ~n v k v c v j v ~d v p, g v d, h v ~d v ~h, h v e v c v ~e v g, i v ~o v ~m, i v ~o v g v ~g v ~h v j v ~n v ~p v p v ~o, i v ~m v b v ~n v ~i v ~h v n v ~i v h v r, j v ~g v ~m v f v d v ~d v h, k v ~p v ~f v ~q v j v ~b v j v h v q, k v ~g v ~k v ~i v q, k v b v c v d v ~i v g v ~n v ~e v ~d, k v o v d v c v l v ~m v k v ~o v ~m v ~o, l v h v h v h v ~g v q v i v n, l v i v c v k v f v l v n v k v ~n v m, m v c v ~f, n v ~k v ~d v i v e v o v o, n v ~f v m v ~e v ~o, n v c v ~o v ~r v m v ~b, n v d, n v k v h v ~h v g v d v p v k v ~d, o v ~i v ~p v ~i v l v j v ~i v ~g v c v p, o v g v ~g v e, o v h v g v ~g v ~h v j v ~r, o v k, p v ~q v n v ~i v ~f v ~q v p, p v k v ~k v r v o v m v ~m v ~g v ~p v r, q v ~e v g v n v h v ~f v b v b v j, q v l v e v h v f, q v n v n v ~k v f v h v j v c v ~g, q v o v g v n v o, r v ~o v ~o, r v ~k v c v f v f v n v ~n v c v ~f v ~e, r v ~g v n v n v r, r v ~f v ~j v ~e v g, r v b v ~l v l v ~q v ~c v ~j, r v o v r v ~o v n], sat).
