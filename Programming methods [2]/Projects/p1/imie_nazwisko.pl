%**************************************** CZĘŚĆ PODSTAWOWA ****************************************%
/*
% Definiujemy moduł zawierający rozwiązanie.
% Należy zmienić nazwę modułu na {imie}_{nazwisko} gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez wielkich liter oraz znaków diakrytycznych
:- module(mateusz_ujda, [resolve/4, prove/2]).

% definiujemy operatory ~/1 oraz v/2
:- op(200, fx, ~).
:- op(500, xfy, v).

% Szukanie rezolwenty.
% UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić definicję
% tego predykatu
resolve(q, p v q, ~q v r, r v p).
resolve(p, p v q v r, ~p v ~q v ~r, []).

% Główny predykat rozwiązujący zadanie.
% UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jego
% definicję.
prove(Clauses, Proof) :-
  Clauses = [p v q v ~r, ~p v q, r v q, ~q, p],
  Proof   = [(p v q v ~r, axiom), (~p v q, axiom), (q v ~r, (p, 1, 2)),
    (r v q, axiom), (q, (r, 4, 3)), (~q, axiom), ([], (q, 5, 6))].


*/
%**************************************** CZĘŚĆ PODSTAWOWA ****************************************%



:- module(imie_nazwisko.pl, [resolve/4, prove/2]).

% negation (~) and disjunction (v) operators definition
:- op(200, fx, ~).
:- op(500, xfy, v).

% empty clause is denoted by 'empty' atom term
% used in case when nonempty clause simplifies to empty (for example: ~p v p)
is_empty_clause(empty).

% predicate is_nonempty_clause(+X) succeeds only when
% X can be unified with term in form of '_ v _',
% which denote non empty clause
is_nonempty_clause(_ v _).

% predicate is_literal(+X) succeeds only when X is not nonempty clause
% we assume that empty clause can be treated as literal in some context
% usually we will use small letters to denote literals, like: x, ~q, r
is_literal(X) :-
	\+is_nonempty_clause(X).

% predicate are_literals(+List) succeeds only when all elements on List
% satisfy predicate is_literal/1 (are literals)
are_literals([X]) :-
	is_literal(X), !.

are_literals([X|XS]) :-
	is_literal(X), are_literals(XS).

% predicate negate(+X, ?Y) succeeds only when Y is negation of X
% usage example:
% 	?- negate(~x, X).
% 	X = x
negate(~X, X) :- !.
negate(X, ~X).

% predicate is_n(+X) succeeds only when X has no negation operator applied
% it's assistant predicate used only in is_notnegated_literal(X)
is_n(~_).

% predicate is_notnegated_literal(+X) succeeds only when X is a literal (succeeds is_literal/1) and
% has no negation operator applied
is_notnegated_literal(X) :-
	\+is_n(X),
	is_literal(X).

% predicate membr(-X, +Clause) succeeds only when Clause contains literal X
% support backtracking and when X not specified it'll unify all literals
% from Clause (with repetitions)
% usage example:
% 	?- membr(X, a v b v c v c).
% 	X = a ;
% 	X = b ;
% 	X = c ;
% 	X = c.
membr(X, X) :-
	is_literal(X), !.

membr(X, X v _) :-
	is_literal(X).

membr(X, _ v CS) :-
	membr(X, CS).

% predicate rm2(+Literal, +Clause, -ResultClause) succeeds only when
% ResultClause is a clause produced by removing all occurences of Literal in Clause
% we assume that it can produce clauses containing 'empty' clause/Literal
% example usage:
% 	?- rm2(a, b v a v a, X).
% 	X = b v empty.
rm2(X, X, empty) :- is_literal(X).
rm2(X, Y, Y) :- are_literals([X, Y]).
rm2(X, X v CS, CSR) :- is_literal(X), rm2(X, CS, CSR), !.
rm2(X, CS v X, CSR) :- is_literal(X), rm2(X, CS, CSR), !.
rm2(X, Y v CS, Y v CSR) :- are_literals([X, Y]), rm2(X, CS, CSR).

% predicate rm2(+Literal, +Clause, -ResultClause) is same as rm2/3, but
% it also removes all occurences of 'empty' (if any)
% it won't remove empty if Clause is empty
% example usage:
% 	?- rm(a, b v a v a v ~a, X).
% 	X = b v ~a.
rm(X, CS, CSR2) :- rm2(X, CS, CSR), rm2(empty, CSR, CSR2), !.

% predicate simplify(+Clause1, -Clause2) succeeds when Clause2 is a simplified version of a Clause1 clause.
% simplified version of a clause is when:
% 	(1) there are no literal repetitions and
% 	(2) there is no occurence of literal and it negation
% in the clause
% we assume that it can produce clauses containing 'empty' clause/Literal
% usage example:
% 	?- simplify(~r v q v x v r v r v q, X).
% 	X = q v x v empty.
simplify(X, X) :- is_literal(X), !.
simplify(empty, empty) :- !.
simplify(C, X v W) :- membr(X, C), is_literal(X), negate(X, NX), \+membr(NX, C), rm(X, C, R), simplify(R, W),  !.
simplify(C, empty) :- membr(X, C), is_literal(X), negate(X, NX), membr(NX, C), rm(X, C, C2), rm(NX, C2, C3), is_empty_clause(C3), !.
simplify(C, R) :- membr(X, C), is_literal(X), negate(X, NX), membr(NX, C), rm(X, C, C2), rm(NX, C2, C3), simplify(C3, R), !.

% predicate glue_clauses(+Clause1, +Clause2, -ClauseResult) succeeds when
% ClauseResult is a disjunction of Clause1 and Clause2



% predicate glue_clauses(+Clause1, +Clause2, -ClauseResult) succeeds when
% ClauseResult is a disjunction of Clause1 and Clause2 (as is, without simplification)
% previous version:
/*glue_clauses(X, X, X) :- is_literal(X), !.
glue_clauses(X, Y, X v Y) :- are_literals([X, Y]), dif(X, Y), !.
glue_clauses(X, N v M, X v N v M) :- is_literal(X), \+membr(X, N v M), !.
glue_clauses(X, N v M, N v M) :- is_literal(X), membr(X, N v M), !.
glue_clauses(X v Y, Z, X v Y v Z) :- is_literal(Z), \+membr(Z, X v Y), !.
glue_clauses(X v Y, Z, X v Y) :- is_literal(Z), membr(Z, X v Y), !.
glue_clauses(X v Y, N v M, X v Y v N v M).    */%                                <=============== TUTAJ SIĘ KOŃCZYŁ ORYGINALNY KONIEC KOMENTARZA WIELOWIERSZOWEGO
% problem with previous version: prolog adds brackets N v M ==> (N)v M
% usage example:
% 	?- glue_clauses(q v ~r, k v q v ~r v x, X).
% 	X = q v ~r v k v q v ~r v x.
glue_clauses(C1, C2, C12) :-
	term_string(C1, P1),
	term_string(C2, P2),
	string_concat(P1, ' v ', P1_),
	string_concat(P1_, P2, P3),
	string_to_atom(P3, P3_),
	atom_to_term(P3_, C12, _).

% predicate sum(+Clause1, +Clause2, -ClauseResult) succeeds when
% ClauseResult is simplified disjunction of given (Clause1, Clause2) clauses
% we assume that ClauseResult dont't contain 'empty' clause/literal, but can be empty (sum(empty, empty, X) will unify X with empty)
% usage example:
% 	?- sum(q v ~r, k v ~q v ~r v x, X).
% 	X = ~r v k v x
sum(C1, C2, C1C2) :- glue_clauses(C1, C2, C12), simplify(C12, C12S), rm(empty, C12S, C1C2), !.

% predicate cast_empty_clause(+Clause, -Conversion) suceeds only when
% Conversion is a Clause when Clause is a nonempty clause, otherwise Conversion unify with empty list
% predicate used only in resolve/4 predicate
cast_empty_clause(empty, []) :- !.
cast_empty_clause(X, X) :- \+is_empty_clause(X).

% predicate resolve(+Literal, +Clause1, +Clause2, -Resolvent) for given Literal, Clause1 and Clause2 unify Resolvent with a term
% being a 'resolvent of clauses Clause1 and Clause2 relative to literal Literal'.
% usage example:
% 	?- resolve(q, p v q, ~q v r, X).
% 	X = p v r.
resolve(X, empty, C, C) :- membr(X, C), !.
resolve(X, empty, empty, []) :- is_literal(X), !.
resolve(X, C, empty, C) :- membr(X, C), !.
resolve(X, X, XN, []) :- is_literal(X), negate(X, XN), !.
resolve(X, XN, X, []) :- is_literal(X), negate(X, XN), !.
resolve(X, C1, C2, R) :-
	is_literal(X),
	negate(X, XN),
	membr(X, C1),
	membr(XN, C2),
	rm(X, C1, C1X),
	rm(XN, C2, C2XN),
	sum(C1X, C2XN, RN), cast_empty_clause(RN, R), !.
resolve(X, C1, C2, R) :-
	is_literal(X),
	negate(X, XN),
	membr(XN, C1),
	membr(X, C2),
	rm(XN, C1, C1XN),
	rm(X, C2, C2X),
	sum(C1XN, C2X, RN), cast_empty_clause(RN, R), !.

% predicate index_of(+List, +Element, -Index) succeeds when given List and Element,
% Index can be unified with a number that denote position of element Element in list List.
% usage examples:
% 	?- index_of([1, 2, 3, 4], 3, X).
% 	X = 3.
%
% 	?- index_of([1, 2, 3, 4], 6, X).
% 	false.
index_of([Element|_], Element, 1):- !.
index_of([_|Tail], Element, Index):-
  index_of(Tail, Element, Index1),
  !,
  Index is Index1+1.

% predicate inlcude(+Clause1, +Clause2) succeeds when all literal within clause Clause1 occure in clause Clause2
% usage examples:
% 	?- are_included(p v q, q v r v ~x v p).
% 	true.
%
% 	?- are_included(p v q, q v r v ~x v ~p).
% 	false.
are_included(X, C) :- is_literal(X), membr(X, C), !.
are_included(X v CX, CY) :- membr(X,CY), are_included(CX, CY), !.

% predicate union(+Clause1, +Clause2) succeeds when clauses Clause1 and Clause2 are logically the same
% other words when all literals from Clause1 occures in Clause2 and all listerals from Clause2 occure in Clause1
% usages examples:
% 	?- union(p v q v r, r v p v q).
% 	true.
%
% 	?- union(p v q v r, r v p v ~q).
% 	false.
%
% 	?- union(p v q v r, r v p v q v z).
% 	false.
union(C1, C2) :- are_included(C1, C2), are_included(C2, C1).

% predicate is_proof_member(+Clause1, +ProofClausesList) succeeds when clause Clause1 is a union with at least one clause that is on the given ProofClausesList
% ProofClausesList must unify with list of pairs, where first element of a pair is a clause
% usage examples:
% 	?- is_proof_member(q v ~r, [(p v q v ~r, axiom),  (~p v q, axiom),  (q v ~r, o(p, 1, 2))]).
% 	true.
%
% 	?- is_proof_member(q, [(p v q v ~r, axiom),  (~p v q, axiom),  (q v ~r, o(p, 1, 2))]).
% 	false.
is_proof_member(CX, [(CXP, _)|_]) :- union(CX, CXP), !.
is_proof_member(CX, [(CXP, _)|CXPT]) :- \+union(CX, CXP), is_proof_member(CX,CXPT).


% (1) predicate prove(+Clauses, +InitialProofList, -Proof) succeeds when
% 	term Proof can be unified with refutation proof showing given list of clauses (axioms) Clauses is inconsistent (contradictory)
% (2) Proof term and its meaning was described in given task (translated):
% Proof is a list of pairs of form (Clause, Origin), where Clause is a clause and Origin is a term describing clause Clause origin.
% Possible Origin values:
% 	* axiom: an atom denoting that Clause is a clause from given list of clauses Clauses
% 	* (Literal, N, M): triplet where Clause is a resolvent of (some) clauses occuring in proof (InitialProofList) at positions N and M (couting from 1)
% 		in relation to literal Literal.
% 		Clause in Proof should be at position greater than N and M. A clause at position N has positive Literal occurence and clause at position M its Literal negative version.
% it's a helper predicate used in prove/2
% usage examples:
% 	?- prove([q, x v q, x v ~q, ~x], [], P).
% 	Proof exists!
% 	P = [(q, axiom),  (x v q, axiom),  (x v ~q, axiom),  (x, o(q, 1, 3)),  (~x, axiom),  (~q, o(x, 3, 5)),  ([], o(q, 1, 6))] .
%
% 	?- prove([q, x v ~q, ~x], [(x v q, axiom)], P).
% 	Proof exists!
% 	P = [(x v q, axiom),  (q, axiom),  (x v ~q, axiom),  (x, o(q, 1, 3)),  (~x, axiom),  (~q, o(x, 3, 5)),  ([], o(q, 2, 6))] .
prove(_, Proof, Proof) :- member(([], _), Proof), !. /*, write("Proof exists!"), !.    */%                                          <<================== TUTAJ SIĘ KOŃCZYŁ WIELOWIERSZOWEGO
prove(CS, P, Proof) :-
	member((C1, _), P), index_of(P, (C1, _), N), membr(X, C1), is_notnegated_literal(X), member((C2, _), P), dif(C1, C2), negate(X, XN), membr(XN, C2), index_of(P, (C2, _), M),
	resolve(X, C1, C2, R),
	\+is_proof_member(R, P),
	append(P, [(R, o(X, N, M))], PA),
	prove(CS, PA, Proof).
prove(CS, P, Proof) :- dif(CS, []), member(X, CS), delete(CS, X, CSNOX), append(P, [(X, axiom)], PA), prove(CSNOX, PA, Proof).

% predicate while_true/0 succeeds when while_true succeeds
% it's a self reference and it simulate 'while(true)' loop behaviour from nondeclarative languages
% made only to satisfy task requirements and used only in prove/2 predicate
while_true() :- while_true().

% predicate prove(+CLauses, -Proof) has same meaning as prove(+Clauses, +InitialProofList, -Proof), where InitialProofList is given and it's empty list ([])
% this is the predicate we were given as a task to define (entry point)
% usage example:
% 	?- prove([p v q v ~r, ~p v q, r v q, ~q, p], Proof).
% 	Proof exists!
% 	Proof = [(p v q v ~r, axiom),  (~p v q, axiom),  (q v ~r, o(p, 1, 2)),  (r v q, axiom),  (q v p, o(r, 4, 1)),  (q, o(r, 4, 3)),  (~q, axiom),  (p v ~r, o(q, 1, 7)),  (~p, o(q, 2, 7)),  (~r, o(q, 3, 7)),  (r, o(q, 4, 7)),  (p, o(q, 5, 7)),  ([], o(q, 6, 7))] ;
% 	Proof exists!
% 	Proof = [(p v q v ~r, axiom),  (~p v q, axiom),  (q v ~r, o(p, 1, 2)),  (r v q, axiom),  (q v p, o(r, 4, 1)),  (q, o(r, 4, 3)),  (~q, axiom),  (p v ~r, o(q, 1, 7)),  (~p, o(q, 2, 7)),  (~r, o(q, 3, 7)),  (r, o(q, 4, 7)),  (p, o(q, 5, 7)),  ([], o(r, 11, 10))] ;
% 	Proof exists!
% 	Proof = [(p v q v ~r, axiom),  (~p v q, axiom),  (q v ~r, o(p, 1, 2)),  (r v q, axiom),  (q v p, o(r, 4, 1)),  (q, o(r, 4, 3)),  (~q, axiom),  (p v ~r, o(q, 1, 7)),  (~p, o(q, 2, 7)),  (~r, o(q, 3, 7)),  (r, o(q, 4, 7)),  (p, o(q, 5, 7)),  ([], o(p, 12, 9))] .
% 	(...)
prove(Clauses, Proof) :- prove(Clauses, [], Proof),!.
prove(Clauses, Proof) :- \+prove(Clauses, [], Proof), while_true().
