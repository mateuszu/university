% Definiujemy moduł zawierający rozwiązanie.
% Należy zmienić nazwę modułu na {imie}_{nazwisko} gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez wielkich liter oraz znaków diakrytycznych
:- module(imie_nazwisko, [parse/3]).

% Główny predykat rozwiązujący zadanie.
% UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jego
% definicję.

% == lexical analysis ==

lexer(Tokens) --> wspaces_and_comments, (
    % operators and stop signs
    "(",        !, { Token = tokLBracket };
    ")",        !, { Token = tokRBracket };
    "[",        !, { Token = tokLBoxBracket };
    "]",        !, { Token = tokRBoxBracket };
    "..",       !, { Token = tokDots };
    ",",        !, { Token = tokOpComma };
    "=",        !, { Token = tokOpEq };
    "<>",       !, { Token = tokOpNeq };
    "<=",       !, { Token = tokOpLeq };
    "<",        !, { Token = tokOpLt };
    ">=",       !, { Token = tokOpGeq };
    ">",        !, { Token = tokOpGt };
    "^",        !, { Token = tokOpCaret };
    "|",        !, { Token = tokOpBar };
    "+",        !, { Token = tokOpPlus };
    "-",        !, { Token = tokOpMinus };
    "&",        !, { Token = tokOpAnd };
    "*",        !, { Token = tokOpMulti };
    "/",        !, { Token = tokOpDiv };
    "%",        !, { Token = tokOpMod };
    "@",        !, { Token = tokOpAt };
    "#",        !, { Token = tokOpHash };
    "~",        !, { Token = tokOpTld };
    % integer literals
    digit(D),   !, number(D, N), { Token = tokNumber(N) };
    % keywords and identyfiers
    initial_letter(L), !,
      identifier(L, Id), { member((Id, Token), [ (def, tokKeyDef), (else, tokKeyElse), (if, tokKeyIf), (in, tokKeyIn), (let, tokKeyLet), (then, tokKeyThen), ('_', tokUnder) ]), !; Token = tokVar(Id) };
    [_], { Token = tokUnknown }
  ),
  !, { Tokens = [Token | TokList] }, lexer(TokList).
lexer([]) --> wspaces_and_comments, !.
lexer(Tokens) --> [], { Tokens = [] }.

white_space -->
   [Char], { member(Char, [9, 10, 11, 12, 13, 32]) }, !, white_space;
   [].

anything -->
  [];
  [Char], !, anything_ctx(Char).

anything_ctx(_) --> [].
anything_ctx(PrevChar) -->
  [Char], {Char >= 0, Char < 127, PrevChar = 42, Char \= 41}, !, anything_ctx(Char);
  [Char], {Char >= 0, Char < 127, PrevChar \= 42}, !, anything_ctx(Char).

comment -->
  "(*", anything, comment, anything, "*)";
  "(*", anything, "*)".

wspaces_and_comments -->
  white_space, comment, wspaces_and_comments;
  white_space.

digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

alphanum([A|T]) -->
   [A], ( { code_type(A, alnum), A < 127 }, !;
          { string_codes("_", [A]) }, !;
          { string_codes("'", [A]) }
        ), !, alphanum(T).
alphanum([]) -->
   [].

initial_letter(L) -->
   [L], { code_type(L, alpha), L < 127 };
   [L], { string_codes("_", [L])}.

identifier(L, Id) -->
  alphanum(As),
  { atom_codes(Id, [L|As]) }.


% == syntax analysis, abstract tree ==

program(Result) -->
  definition(Def), !, program(DefList), {Result = [Def | DefList]}.
program([]) -->
  [].

definition(Def) -->
  [tokKeyDef], !, [tokVar(Ident), tokLBracket], pttrn(P), [tokRBracket, tokOpEq], expr(E), { Def = def(Ident, P, E) }.

% pattern syntax
pttrn(P) -->
  [tokLBracket], !, pttrn(P), [tokRBracket];
  ([tokUnder], { P1 = wildcard(no) }; [tokVar(X)], {P1 = var(no, X) }), !, pttrn(P1, P);
  [tokUnder], !, { P = wildcard(no) };
  [tokVar(X)], !, { P = var(no, X) }.

pttrn(Acc, Pttrn) -->
  [tokOpComma], !, pttrn(P), { Acc1 = pair(no, Acc, P) }, !, pttrn(Acc1, Pttrn).
pttrn(Acc, Acc) -->
  [].

% expression syntax
expr(E) -->
  [tokKeyIf], !, expr(E1), [tokKeyThen], expr(E2), [tokKeyElse], expr(E3), { E = if(no, E1, E2, E3) };
  [tokKeyLet], !, pttrn(P), [tokOpEq], expr(E1), [tokKeyIn], expr(E2), { E = let(no, P, E1, E2) };
  expr_op(E).

expr_op(E) -->
  pair(PairLeft), pair_expr(PairLeft, E).

pair_expr(PairLeft, E) -->
  binary_op_comma, !, expr_op(PairRight), { PairExpr = pair(no, PairLeft, PairRight) }, pair_expr(PairExpr, E).
pair_expr(P, P) -->
  [].

pair(E) -->
  comp(CompLeft), comp_expr(CompLeft, E).

comp_expr(CompLeft, E) -->
  binary_op_comp(O), !, comp(CompRight), { CompExpr = op(no, O, CompLeft, CompRight) }, !, comp_expr(CompExpr, E).
comp_expr(C, C) -->
  [].

comp(E) -->
  at(AtLeft), at_expr(AtLeft, E).

at_expr(AtLeft, E) -->
  binary_op_at(O), !, expr_op(AtRight), { AtExpr = op(no, O, AtLeft, AtRight) }, !, at_expr(AtExpr, E).
at_expr(A, A) -->
  [].

at(E) -->
  add(AddLeft), add_expr(AddLeft, E).

add_expr(AddLeft, E) -->
  binary_op_add(O), !, add(AddRight), { AddExpr = op(no, O, AddLeft, AddRight) }, !, add_expr(AddExpr, E).
add_expr(A, A) -->
  [].

add(E) -->
  mult(MultLeft), mult_expr(MultLeft, E).

mult_expr(MultLeft, E) -->
  binary_op_mult(O), !, mult(MultRight), { MultExpr = op(no, O, MultLeft, MultRight) }, !, mult_expr(MultExpr, E).
mult_expr(M, M) -->
  [].

mult(E) -->
  unary_expr(E).

unary_expr(E) -->
  unary_op(O), !, expr_op(ExprOp), { E = op(no, O, ExprOp) };
  expr_simple(E).

expr_simple(E) -->
  bit(Simple), bit_expr(Simple, E).

bit_expr(Simple, E) -->
  [tokLBoxBracket], expr(Expr1), [tokDots], !, expr(Expr2), [tokRBoxBracket], !, { SimpleExpr = bitsel(no, Simple, Expr1, Expr2) }, !, bit_expr(SimpleExpr, E);
  [tokLBoxBracket], !, expr(Expr), [tokRBoxBracket], !, { SimpleExpr = bitsel(no, Simple, Expr) }, !, bit_expr(SimpleExpr, E).
bit_expr(B, B) -->
  [].

bit(E) -->
  [tokLBracket], !, expr(E), [tokRBracket];
  expr_atom(E).

expr_atom(E) -->
  fcall(E), !;
  variable(E), !;
  numliteral(E), !;
  empty_vector, !, { E = empty(no) };
  single_bit(E).

fcall(call(no, Name, E)) -->
  identifier(Name), [tokLBracket], expr(E), [tokRBracket].

variable(var(no, Name)) -->
  identifier(Name).

numliteral(num(no, N)) -->
  [tokNumber(N)].

identifier(Name) -->
  [tokVar(Name)].

empty_vector -->
  [tokLBoxBracket, tokRBoxBracket].

single_bit(bit(no, EX)) -->
  [tokLBoxBracket], expr(EX), [tokRBoxBracket].

empty -->
  [].

binary_op(O) -->
  binary_op_comp(O), !;
  binary_op_at(O), !;
  binary_op_summ(O), !;
  binary_op_multi(O).

binary_op_mult(O) -->
  [tokOpAnd],   !, { atom_codes(O, "&") };
  [tokOpMulti], !, { atom_codes(O, "*") };
  [tokOpDiv],   !, { atom_codes(O, "/") };
  [tokOpMod],   !, { atom_codes(O, "%") }.

binary_op_add(O) -->
  [tokOpBar],   !, { atom_codes(O, "|") };
  [tokOpCaret], !, { atom_codes(O, "^") };
  [tokOpPlus],  !, { atom_codes(O, "+") };
  [tokOpMinus], !, { atom_codes(O, "-") }.

binary_op_at(O) -->
  [tokOpAt],    !, { atom_codes(O, "@") }.

binary_op_comp(O) -->
  [tokOpEq],    !, { atom_codes(O, "=") };
  [tokOpNeq],   !, { atom_codes(O, "<>") };
  [tokOpLt],    !, { atom_codes(O, "<") };
  [tokOpGt],    !, { atom_codes(O, ">") };
  [tokOpLeq],   !, { atom_codes(O, "<=") };
  [tokOpGeq],   !, { atom_codes(O, ">=") }.

binary_op_comma -->
  [tokOpComma].

unary_op(O) -->
  [tokOpMinus], !, { O = '-' };
  [tokOpHash],  !, { O = '#' };
  [tokOpTld],   !, { O = '~'}.

% go
parse(_, Codes, Program) :-
  phrase(lexer(Tokens), Codes),
  phrase(program(Program), Tokens), !.
parse(_, _, _) :-
  throw(syntax_error("Parsowanie nie powiodlo sie", no)).
