#!/home/boris/bin/swipl -q -t main -f

:- use_module(library('dcg/basics')).

main :-
    init(S),
    loop(S),
    halt(1).

init([]) :-
    prompt(_, '').

cleanup(S) :-
    print_s(S).

quit(end_of_file).

% Evaluation loop of the calculator with the stack as an argument
loop(S) :-
    /* DEBUG */ prolog_current_frame(F),
    write(F), nl,
    read_line_to_codes(user_input, Codes),
    (   quit(Codes) -> cleanup(S)
    ;   parse_line(Codes, Line), 
        reduce_stack([Line|S], NewS),
        /* DEBUG */ format('~w~n', [NewS]),
        !, loop(NewS)
    ).

/* TODO */
% make it possible to push operators and values
% to the stack directly from the program


%% Reduce the current stack %%

reduce_stack([e(Type,Content)|Rest], NewS) :- % unpack the arguments
    reduce_stack(Type, Content, Rest, NewS).
reduce_stack([error|Rest], Rest). % ignore for now
reduce_stack([empty|Rest], Rest). % igonre this one too
reduce_stack(S, S). % the stack could not be reduced

% Delegate depending on stack element type
reduce_stack(a, ArithOp, S, [e(n,R)|NewS]) :-
    do_arithop(ArithOp, S, R, NewS), print_ns(R).
reduce_stack(s, StackOp, S, NewS) :-
    do_stackop(StackOp, S, NewS).
reduce_stack(l, ListOp, S, [e(n,R)|NewS]) :-
    do_listop(ListOp, S, R, NewS), print_ns(R).
reduce_stack(c, _Command, S, S). % for now...
% will fail if numbers on top of stack

% Do stack operations
do_stackop(top, [Top|S], [Top|S]) :- print_se(Top).
do_stackop(show, S, S) :- print_s(S).
do_stackop(swap, [E0,E1|S], NewS) :- reduce_stack([E1,E0|S], NewS).
do_stackop(duplicate, [Top|S], [Top,Top|S]).
do_stackop(pop, [Top|S], S) :- print_se(Top).
do_stackop(revstack, S, NewS) :- reverse(S, RS), reduce_stack(RS, NewS).
do_stackop(clear, [], [e(s,clear)]) :- !. % watch out for this possibility
do_stackop(clear, _S, []).

% Do list operations
do_listop(len, [e(n,N)|S], [Len], [e(n,N)|S]) :- length(N,Len).
do_listop(sum, [e(n,N)|S], [Sum], [e(n,N)|S]) :- sum_list(N,Sum).
do_listop(prod, [e(n,N)|S], [Prod], [e(n,N)|S]) :- foldl(mul,N,1,Prod).
do_listop(mean, [e(n,N)|S], [Mean], [e(n,N)|S]) :-
    length(N,Len),
    sum_list(N,Sum),
    Mean is Sum/Len.
do_listop(median, [e(n,N)|S], [Median], [e(n,N)|S]) :-
    sort(N,SN),
    length(SN,Len),
    Mod is Len mod 2, Middle is Len div 2,
    (   Mod =:= 1 -> nth0(Middle, N, Median)
    ;   nth0(Middle, N, Above),
        nth1(Middle, N, Below),
        Median is (Above+Below)/2
    ).
do_listop(sort, [e(n,N)|S], SN, S) :- sort(N,SN).
do_listop(rev, [e(n,N)|S], RN, S) :- reverse(N,RN).
do_listop(shuffle, [e(n,N)|S], SN, S) :- random_permutation(N,SN).

% Do arithmetic operations
do_arithop(BinO, [e(n,N0),e(n,N1)|S], R, S) :- % binary operator
    phrase( arithop(BinO, 2), _ ), !, % check arity
    mapbinop(BinO, N0, N1, R).
do_arithop(UnO, [e(n,N)|S], R, S) :- % unary operator
    phrase( arithop(UnO, 1), _ ), !, % check arity
    maplist(UnO, N, R).

% Map binary op to lists with different length
mapbinop(BinOp, N0, N1, Result) :-
    length(N0,L0), length(N1,L1),
    compare(C,L0,L1),
    (   C = (=) -> maplist(BinOp, N0, N1, Result)
    ;   C = (<) -> 0 =:= L1 rem L0, maplistfs(N0, N1, Result, N0, BinOp)
    ;   C = (>) -> 0 =:= L0 rem L1, maplistss(N0, N1, Result, N1, BinOp)
    ).

% Map elements, repeating the first, shorter, list
maplistfs([], [], [], _, _) :- !.
maplistfs([], N1, R, N0, BinOp) :- !, maplistfs(N0, N1, R, N0, BinOp).
maplistfs([E0|N0], [E1|N1], [R|Result], N0B, BinOp) :-
    call(BinOp, E0, E1, R),
    maplistfs(N0, N1, Result, N0B, BinOp).

% Map elements, repeating the second, shorter, list
maplistss([], [], [], _, _) :- !.
maplistss(N0, [], R, N1, BinOp) :- !, maplistss(N0, N1, R, N1, BinOp).
maplistss([E0|N0], [E1|N1], [R|Result], N1B, BinOp) :-
    call(BinOp, E0, E1, R),
    maplistss(N0, N1, Result, N1B, BinOp).

% Arithmetic functions as predicates
add(N0,N1,R) :- R is N1+N0.
sub(N0,N1,R) :- R is N1-N0.
mul(N0,N1,R) :- R is N1*N0.
dvd(N0,N1,R) :- R is N1/N0.
pow(N0,N1,R) :- R is N1**N0.
sqr(N0,R) :- R is sqrt(N0).
abs(N0,R) :- R is abs(N0).

%% Output %%

% Print the whole stack
print_s([E|Es]) :- print_se(E), !, print_s(Es).
print_s([]).

% Print a stack element
print_se(e(Type, Content)) :- print_se(Type, Content).

print_se(n, N) :- print_ns(N). % print a list of numbers
print_se(a, Op) :- % print an arithmetic operator
    % convert back to original representation
    phrase( arithop(Op, _Arity), String),
    format('~s~n', [String]).
% catch-all clause, for now
print_se(_Type, Name) :- format('~w~n', [Name]).

% Print a list of numbers on a line
% a list of numbers is guaranteed to have at least one element!
print_ns([N|Ns]) :- print_ns(Ns, N).

print_ns([N|Ns], Prev) :- format('~w ', [Prev]), print_ns(Ns, N).
print_ns([], Last) :- format('~w~n', [Last]).

%% Input %%

% From a line of input, make a stack element
parse_line(Codes, Line) :-
    tokenize(Codes, Tokens), 
    maplist(parse, Tokens, Parsed),
    normalize(Parsed, Line).

% Split a line to white space separated tokens
tokenize(Codes, Tokens) :-
    phrase( line(Tokens), Codes ).

line(Tokens) --> tokens(Tokens).

tokens([]) --> blanks_to_nl, !.
tokens([T|Ts]) --> token(T), tokens(Ts).

token(T) --> whites, nonblanks(T).

% parse a token to a tagged value
parse(Token, Parsed) :-
    phrase( parsed(Parsed), Token ), !.
parse(Token, u(Unknown)) :- % token not recognized
    atom_codes(Unknown, Token).

parsed(p(n,N)) --> number(N), !. % using `number` from dcg/basics
parsed(p(a,AO)) --> arithop(AO, _Arity), !.
parsed(p(s,SO)) --> stackop(SO), !.
parsed(p(l,LO)) --> listop(LO), !.
parsed(p(c,C)) --> command(C), !.
% will fail if the the token is not recognized

arithop(add, 2) --> "+".
arithop(sub, 2) --> "-".
arithop(mul, 2) --> "*".
arithop(dvd, 2) --> "/".
arithop(pow, 2) --> "pow".
arithop(sqr, 1) --> "sqrt".
arithop(abs, 1) --> "abs".

stackop(top) --> "top".
stackop(show) --> "show".
stackop(swap) --> "swap".
stackop(duplicate) --> "duplicate".
stackop(pop) --> "pop".
stackop(revstack) --> "revstack".
stackop(clear) --> "clear".

listop(len) --> "len".
listop(sum) --> "sum".
listop(prod) --> "prod".
listop(mean) --> "mean".
listop(median) --> "median".
listop(sort) --> "sort".
listop(rev) --> "rev".
listop(shuffle) --> "shuffle".

command(quit) --> "quit".

% From a list of parsed, tagged elements, make a valid stack element
normalize(Parsed, Line) :-
    (   Parsed = [] -> Line = empty
    ;   Parsed = [p(n,N)|Ns] % numbers
    ->  nvalues(Ns, N, NList), Line = e(n, NList)
    ;   Parsed = [p(Type, Content)] % an operator or command
    ->  Line = e(Type, Content)
    ;   Line = error % validation failed
    ).

% convert a list of parsed, tagged numbers to a list of numbers
nvalues([], N, [N]).
nvalues([p(n,N1)|Rest], N, [N|Ns]) :- nvalues(Rest, N1, Ns).

