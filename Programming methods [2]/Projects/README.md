# Project 1

The task is to program two predictions in the Prolog:  
```resolve/4```  
and  
```prove/*```  
which adequately enumerate the resolvent of the two clauses and seek for resolution refutation.

## Simple test

```?- test_all.``` runs tests included in *imie_nazwisko_tests.pl* file.  
```?- run_test(+Name).``` where name is name of single test in *imie_nazwisko_tests.pl* file.

Two additional predicats you can test with:

```?- show_proof(+Input, +Proof)```  
```?- test_and_show(+Name)```

## Main predicates

```resolve(+Var,  +Clause1,  +Clause2,  -Res).``` finds resolvent Res, in relation to literal Var, which appears in the list Clause1 and is negated in Clause2 list.  

Example use:
```
resolve(q, p v q, ~q v r, r v p).
```  


```prove(+Clauses,  -Proof)``` creates resolution refutation of Clauses and returns Proof as a result.

Example use:
```
?- prove([p v q v ~r, ~p v q, r v q, ~q, p], Proof).
Proof = [(p v q v ~r, axiom), (~p v q, axiom), (q v ~r, (p, 1, 2)),
(r v q, axiom), (q, (r, 4, 3)), (~q, axiom), ([], (q, 5, 6))]. 
```    


