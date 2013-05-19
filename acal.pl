#!/home/boris/bin/swipl -q -t main -f

:- use_module(library('dcg/basics')).

main :-
    init(S),
    loop(S),
    halt(1).

init([]) :-
    nb_setval(float_precision, 3),
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

reduce_stack([e(c,Command)|Rest], NewS) :- % unpack the arguments
    do_command(Command, Rest, NewS).
reduce_stack([error|Rest], Rest). % ignore for now
reduce_stack([empty|Rest], Rest). % ignore this one too
reduce_stack(S, S). % the stack could not be reduced

% Do stack operations
do_command(top, [Top|S], [Top|S]) :- print_se(Top).
do_command(show, S, S) :- print_s(S).
do_command(swap, [E0,E1|S], NewS) :- reduce_stack([E1,E0|S], NewS).
do_command(duplicate, [Top|S], [Top,Top|S]).
do_command(pop, [Top|S], S) :- print_se(Top). % push to register
% do_command(push, ...)
do_command(revstack, S, NewS) :- reverse(S, RS), reduce_stack(RS, NewS).
do_command(clear, _S, []).

% Do list operations
do_command(len, [e(n,N)|S], [e(n,[Len]),e(n,N)|S]) :- length(N,Len).
do_command(sum, [e(n,N)|S], [e(n,[Sum]),e(n,N)|S]) :- sum_list(N,Sum).
do_command(prod, [e(n,N)|S], [e(n,[Prod]),e(n,N)|S]) :- foldl(mul,N,1,Prod).
do_command(mean, [e(n,N)|S], [e(n,[Mean]),e(n,N)|S]) :-
    length(N,Len),
    sum_list(N,Sum),
    Mean is Sum/Len.
do_command(median, [e(n,N)|S], [e(n,[Median]),e(n,N)|S]) :-
    sort(N,SN),
    length(SN,Len),
    Mod is Len mod 2, Middle is Len div 2,
    (   Mod =:= 1 -> nth0(Middle, N, Median)
    ;   nth0(Middle, N, Above),
        nth1(Middle, N, Below),
        Median is (Above+Below)/2
    ).
do_command(sort, [e(n,N)|S], [e(n,SN)|S]) :- msort(N,SN).
do_command(set, [e(n,N)|S], [e(n,SN)|S]) :- sort(N,SN).
do_command(rev, [e(n,N)|S], [e(n,RN)|S]) :- reverse(N,RN).
do_command(shuffle, [e(n,N)|S], [e(n,SN)|S]) :- random_permutation(N,SN).

% subsequence extracts a subsequence from an existing list
% [1] is the list, [0] holds the arguments to the function
% X :- X >= 0
%   first X elements of [1] (or the whole, if X > Len)
% X :- X < 0
%   last X elements of [1]
% From,To :- [From-To) elements of [0], indices are 0-based
%   negative means counting from the back
% From,Step,Len :- Every Step'th element starting at From
%   altogether at most Len elements
%

% Do arithmetic operations
do_command(BinOp, [e(n,N0),e(n,N1)|S], [e(n,R)|S]) :-
    memberchk(BinOp, [add,sub,mul,dvd,pow]), % binary operator
    mapbinop(BinOp, N0, N1, R).
do_command(UnOp, [e(n,N)|S], [e(n,R)|S]) :-
    memberchk(UnOp, [sqrt,abs]), % unary operator
    maplist(UnOp, N, R).

% Map binary op to lists with different length
mapbinop(BinOp, N0, N1, Result) :-
    length(N0,L0), length(N1,L1),
    compare(C,L0,L1),
    eq_len(C,L0,N0,L1,N1,NewN0,NewN1), % could fail!
    maplist(BinOp, NewN0, NewN1, Result).
    
eq_len(=,_,N0,_,N1,N0,N1).
eq_len(<,L0,N0,L1,N1,NewN0,N1) :-
    0 =:= L1 rem L0,
    Times is L1 div L0,
    rep(N0,Times,NewN0).
eq_len(>,L0,N0,L1,N1,N0,NewN1) :-
    0 =:= L0 rem L1,
    Times is L0 div L1,
    rep(N1,Times,NewN1).
rep(List, Times, RepList) :-
    findall(List, between(1,Times,_), Ls),
    append(Ls, RepList).

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
print_s([e(Type,Content)|Es]) :- print_se(Type,Content), !, print_s(Es).
print_s([]).

% Print a stack element
print_se(n, N) :- print_ns(N). % print a list of numbers
print_se(c, Command) :- % print a command
    % convert back to original representation
    phrase( command(Command), String),
    format('~s~n', [String]).
% catch-all clause, for now
print_se(_Type, Name) :- format('~w~n', [Name]).

% Print a list of numbers on a line
% a list of numbers is guaranteed to have at least one element!
print_ns([N|Ns]) :- print_ns(Ns, N).

print_ns([N|Ns], Prev) :- print_n(Prev), format(' '), print_ns(Ns, N).
print_ns([], Last) :- print_n(Last), format('~n').

print_n(Int) :- integer(Int), format('~d', [Int]).
print_n(Float) :- float(Float),
    format('~g', [Float]).

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

parsed(parsed(n,N)) --> number(N), !. % using `number` from dcg/basics
parsed(parsed(c,C)) --> command(C), !.
% will fail if the the token is not recognized

command(add) --> "+".
command(sub) --> "-".
command(mul) --> "*".
command(dvd) --> "/".
command(pow) --> "pow".
command(sqr) --> "sqrt".
command(abs) --> "abs".

command(top) --> "top".
command(show) --> "show".
command(swap) --> "swap".
command(duplicate) --> "duplicate".
command(pop) --> "pop".
command(revstack) --> "revstack".
command(clear) --> "clear".

command(len) --> "len".
command(sum) --> "sum".
command(prod) --> "prod".
command(mean) --> "mean".
command(median) --> "median".
command(sort) --> "sort".
command(set) --> "set".
command(rev) --> "rev".
command(shuffle) --> "shuffle".

command(quit) --> "quit".

% From a list of parsed, tagged elements, make a valid stack element
normalize(Parsed, Line) :-
    (   Parsed = [] -> Line = empty
    ;   maplist(nvalue, Parsed, NList)
    ->  Line = e(n, NList)
    ;   Parsed = [parsed(c, Command)] % a command
    ->  Line = e(c, Command)
    ;   Line = error % validation failed
    ).
nvalue(parsed(n,N), N).

