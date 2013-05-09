#!/home/boris/bin/swipl -q -g main -t halt(1) -s
main :-
    init(S),
    loop(S).

init([]) :-
    prompt(_, '').

cleanup(S) :-
    print_s(S).

quit(end_of_file).

loop(S) :-
    prolog_current_frame(F),
    write(F), nl,
    read_line_to_codes(user_input, Codes),
    (   quit(Codes) -> cleanup(S) 
    ;   parse_line(Codes, Line),
        reduce_stack([Line|S], NewS), !,
        /* DEBUG */ format('~w~n', [NewS]),
        loop(NewS)
    ).

/* TODO */
% make it possible to push operators and values
% to the stack directly from the program

reduce_stack([e(Type,Content)|Rest], NewS) :-
    reduce_type(Type, Content, Rest, NewS).
reduce_stack([error|Rest], Rest).
% if the stack can't be reduced, for example numbers on top
reduce_stack(S, S).

reduce_type(a, ArithOp, S, NewS) :- do_arithop(ArithOp, S, NewS).
reduce_type(s, StackOp, S, NewS) :- do_stackop(StackOp, S, NewS).
reduce_type(l, ListOp, S, NewS) :- do_listop(ListOp, S, NewS).
reduce_type(c, _Command, S, S). % for now...
% not necessary? reduce_type(n, _N, S, S).

do_stackop(top, [Top|S], [Top|S]) :- print_se(Top).
do_stackop(show, S, S) :- print_s(S).
do_stackop(swap, [E0,E1|S], NewS) :- reduce_stack([E1,E0|S], NewS).
do_stackop(duplicate, [Top|S], [Top,Top|S]).
do_stackop(pop, [Top|S], S) :- print_se(Top).
do_stackop(revstack, S, NewS) :- reverse(S, RS), reduce_stack(RS, NewS).
do_stackop(clear, [], [e(s,clear)]) :- !. % watch out for this possibility
do_stackop(clear, _S, []).

do_listop(len, [e(n,N)|S], [e(n,[Len]),e(n,N)|S]) :- length(N,Len).
do_listop(sum, [e(n,N)|S], [e(n,[Sum]),e(n,N)|S]) :- sum_list(N,Sum).
do_listop(prod, [e(n,N)|S], [e(n,[Prod]),e(n,N)|S]) :- foldl(*,N,1,Prod).
do_listop(mean, [e(n,N)|S], [e(n,[Mean]),e(n,N)|S]) :-
    length(N,Len),
    sum_list(N,Sum),
    Mean is Sum/Len.
do_listop(sort, [e(n,N)|S], [e(n,SN)|S]) :- sort(N,SN).
do_listop(rev, [e(n,N)|S], [e(n,RN)|S]) :- reverse(N,RN).
do_listop(shuffle, [e(n,N)|S], [e(n,SN)|S]) :- random_permutation(N,SN).


+(N0,N1,R) :- R is N1+N0.
-(N0,N1,R) :- R is N1-N0.
*(N0,N1,R) :- R is N1*N0.
/(N0,N1,R) :- R is N1/N0.

% print the whole stack
print_s([E|Es]) :- print_se(E), print_s(Es).
print_s([]).

% print stack element
print_se(e(Type, Content)) :- print_se(Type, Content), !.
print_se(n, [N|Ns]) :- print_ns(Ns, N).
print_se(_Type, Name) :- format('~w~n', [Name]).

% print a list of numbers on a line
print_ns([], Last) :- format('~w~n', [Last]).
print_ns([N|Ns], Prev) :- format('~w ', [Prev]), print_ns(Ns, N).

% Codes are the codes of one line from input,
% Line is a stack element
parse_line(Codes, Line) :-
    tokenize(Codes, Tokens),
    maplist(parse, Tokens, Parsed),
    normalize(Parsed, Line).

% Parsed is a list of parsed tokens,
% Line is a stack element
normalize(Parsed, Line) :-
    (   Parsed = [] -> Line = empty
    ;   Parsed = [p(Type,Content)|Rest],
        normalize(Type, Content, Rest, Line) -> true
    ;   Line = error
    ).

% validate and normalize a list of parsed tokens
normalize(a, AO, [], e(a,AO)).
normalize(s, SO, [], e(s,SO)).
normalize(l, LO, [], e(l,LO)).
normalize(c, C,  [], e(c,C)).
normalize(n, N, More, e(n,Ns)) :-
    nvalues(More, N, Ns).

% convert a list of parsed numbers to a list of numbers
nvalues([], N, [N]).
nvalues([p(n,N1)|Rest], N, [N|Ns]) :- nvalues(Rest, N1, Ns).

% split a line to white space separated tokens
tokenize(Codes, Tokens) :-
    phrase( line(Tokens), Codes ).

% parse a token to a tagged value
parse(Token, Parsed) :-
    phrase( parsed(Parsed), Token ), !.
parse(Token, u(Unknown)) :-
    atom_codes(Unknown, Token).

:- use_module(library('dcg/basics')).

line(Tokens) --> tokens(Tokens).
tokens([]) --> blanks_to_nl, !.
tokens([T|Ts]) --> token(T), tokens(Ts).

token(T) --> whites, nonblanks(T).

parsed(p(n,N)) --> number(N), !.
parsed(p(a,AO)) --> arithop(AO), !.
parsed(p(s,SO)) --> stackop(SO), !.
parsed(p(l,LO)) --> listop(LO), !.
parsed(p(c,C)) --> command(C), !.

arithop(+) --> "+".
arithop(-) --> "-".
arithop(*) --> "*".
arithop(/) --> "/".

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
%listop(median) --> "median".
listop(sort) --> "sort".
listop(rev) --> "rev".
listop(shuffle) --> "shuffle".

command(quit) --> "quit".
