#!/home/boris/bin/swipl -q -t main -f

:- use_module(library('dcg/basics')).

main :-
    init(S),
    loop(S).

init([]) :-
    prompt(_, '').

quit(S) :-
    print_s(S),
    halt(1).

% Evaluation loop of the calculator with the stack as an argument
loop(S) :-
    %/* DEBUG */ prolog_current_frame(F),
    %            write(F), nl,
    read_line_to_codes(user_input, Codes),
    parse_line(Codes, Line), 
    /* DEBUG */ format('line: ~w~n', Line),
    reduce_stack([Line|S], NewS),
    /* DEBUG */ format('reduced stack: ~w~n', [NewS]),
    !, loop(NewS).

/* TODO */
% make it possible to push operators and values
% to the stack directly from the program


%% Reduce the current stack %%
reduce_stack([quit|S], S) :- quit(S).
reduce_stack([e(c,Command)|Rest], NewS) :- % unpack the arguments
    do_command(Command, Rest, NewS).
reduce_stack([error|Rest], Rest). % ignore for now
reduce_stack([empty|Rest], Rest). % ignore this one too
reduce_stack(S, S). % the stack could not be reduced

%% Commands %%

% Show the top element of the stack
% - stack must have at least one element
do_command(top, [e(T,C)|S], [e(T,C)|S]) :- print_se(T, C).

% Show the whole stack, top to bottom
do_command(show, S, S) :- print_s(S).

% Swap the top two element of the stack
% - stack must have at least two elements
do_command(swap, [E0,E1|S], NewS) :- reduce_stack([E1,E0|S], NewS).

% Reverse the whole stack
do_command(revstack, S, NewS) :- reverse(S, RS), reduce_stack(RS, NewS).

% Reverse the order of the N elements of the stack below the top
do_command(nrevstack, [e(n,[N])|S], NewS) :-
    integer(N), N > 0,
    length(First, N),
    append(First, Rest, S),
    reverse(First, Rev),
    append(Rev, Rest, CurS),
    reduce_stack(CurS, NewS).

% Add a copy of the top element on top of the stack
% - stack must have at least one element
do_command(duplicate, [Top|S], [Top,Top|S]).

/* TODO */  % pop to a register
do_command(pop, [e(T,C)|S], S) :- print_se(T, C).

% do_command(push, ...)

% Clear the stack (rendering it empty)
do_command(clear, _S, []).

% The following commands: len, sum, prod, mean, median
% push their result to the top of the stack without
% removing the list of numbers they are applied to
%
% Length of the list of numbers
do_command(len, [e(n,N)|S], [e(n,[Len]),e(n,N)|S]) :- length(N, Len).

% Sum of the list of numbers
do_command(sum, [e(n,N)|S], [e(n,[Sum]),e(n,N)|S]) :- sum_list(N, Sum).

% Product of the list of numbers
do_command(prod, [e(n,N)|S], [e(n,[Prod]),e(n,N)|S]) :- foldl(mul, N, 1, Prod).

% Arithmetic mean of the list of numbers
do_command(mean, [e(n,N)|S], [e(n,[Mean]),e(n,N)|S]) :-
    length(N, Len),
    sum_list(N, Sum),
    Mean is Sum/Len.

% Median of the list of numbers
% - if even number of elements, take arithmetic mean of middle two
do_command(median, [e(n,N)|S], [e(n,[Median]),e(n,N)|S]) :-
    msort(N, SN),
    length(SN, Len),
    Mod is Len mod 2, Middle is Len div 2,
    (   Mod =:= 1 -> nth0(Middle, SN, Median)
    ;   nth0(Middle, SN, Above),
        nth1(Middle, SN, Below),
        Median is (Above+Below)/2
    ).

% Sort the numbers in increasing order
% - do not remove duplicates
do_command(sort, [e(n,N)|S], [e(n,SN)|S]) :- msort(N, SN).

% Numbers sorted in increasing order, without duplicates (set)
do_command(set, [e(n,N)|S], [e(n,SN)|S]) :- sort(N, SN).

% Reverse the order of numbers
do_command(rev, [e(n,N)|S], [e(n,RN)|S]) :- reverse(N, RN).

% Shuffle the numbers randomly
do_command(shuffle, [e(n,N)|S], [e(n,SN)|S]) :- random_permutation(N, SN).

% Make one list of numbers from the top two lists of numbers
% - top element is at the back of the new list
do_command(bind, [e(n,N0),e(n,N1)|S], [e(n,B)|S]) :- append(N1, N0, B).

% Make one list of number from the top N lists of numbers on the stack
% - N is the single integer value on the top of the stack
% - "older" (lower in the stack) elements come before "newer" elements
do_command(nbind, [e(n,[N])|S], [e(n,BoundNs)|Rest]) :-
    integer(N), N > 1,
    length(Ns, N), append(Ns, Rest, S),
    maplist(stacked_nvals, Ns, ExtrNs),
    reverse(ExtrNs, RevExtrNs),
    append(RevExtrNs, BoundNs).

% Split off the first number from a list of numbers
% - The element is now the new top
% - the list of numbers must have more than one elements
do_command(split, [e(n,[N0,N1|NRest])|S], [e(n,[N0]),e(n,[N1|NRest])|S]).

% Split off the first N numbers from a list of numbers
% - The split off elements are now the top
% - the list of numbers must have more than N elements
do_command(nsplit, [e(n,[N]),e(n,Ns)|S], [e(n,Front),e(n,[B|Back])|S]) :-
    integer(N), N > 0,
    length(Front, N),
    append(Front, [B|Back], Ns).

% Make an list of integers [From, To)
do_command(range, [e(n,[From,To])|S], [e(n,Range)|S]) :-
    integer(From), integer(To),
    (   From < To ->  srange(<, From, 1, To, From, Range)
    ;   From > To ->  srange(>, From, -1, To, From, Range)
    ).
<<<<<<< HEAD
do_command(srange, [e(n,[From,Step,To])|S], [e(n,Range)|S]) :-
=======

% Make a list of integers [From, From+Step, ..., To)
do_command(srange, [e(n,[From,Step,To])|S], [e(n,Range)|S]) :-
    integer(From), integer(Step), integer(To),
>>>>>>> back_to_float
    (   From < To, Step > 0
    ->  srange(<, From, Step, To, From, Range)
    ;   From > To, Step < 0
    ->  srange(>, From, Step, To, From, Range)
    ).
<<<<<<< HEAD
do_command(lrange, [e(n,[From,Step,Len])|S], [e(n,Range)|S]) :-
    integer(Len), Len > 0,
    Step =\= 0,
=======

% Make a list of integers [From, From+Step, ...) of length Len
do_command(lrange, [e(n,[From,Step,Len])|S], [e(n,Range)|S]) :-
    integer(From), integer(Step), integer(Len), Len > 0,
>>>>>>> back_to_float
    length(Range, Len),
    lrange(Range, From, Step).

% Do arithmetic operations
do_command(BinOp, [e(n,N0),e(n,N1)|S], [e(n,R)|S]) :-
    memberchk(BinOp, [add,sub,mul,dvd,pow]), % binary operator
    eq_len(N0, N1, NewN0, NewN1), % could fail!
    maplist(BinOp, NewN0, NewN1, R),
    print_ns(R).
do_command(UnOp, [e(n,N)|S], [e(n,R)|S]) :-
    memberchk(UnOp, [sqr,abs]), % unary operator
    maplist(UnOp, N, R),
    print_ns(R).

% Make the two lists the same length
eq_len(N0, N1, NewN0, NewN1) :-
    length(N0, L0), length(N1, L1),
    compare(C, L0, L1),
    eq_len(C, N0, L0, N1, L1, NewN0, NewN1).
eq_len((=), N0, _, N1, _, N0, N1).
eq_len((<), N0, L0, N1, L1, NewN0, N1) :-
<<<<<<< HEAD
    0 =:= L1 mod L0,
    Times is L1 div L0,
    rep(N0, Times, NewN0).
eq_len((>), N0, L0, N1, L1, N0, NewN1) :-
    0 =:= L0 mod L1,
    Times is L0 div L1,
=======
    0 =:= L1 mod L0, Times is L1 div L0,
    rep(N0, Times, NewN0).
eq_len((>), N0, L0, N1, L1, N0, NewN1) :-
    0 =:= L0 mod L1, Times is L0 div L1,
>>>>>>> back_to_float
    rep(N1, Times, NewN1).
% Repeat a list
rep(List, Times, RepList) :-
    findall(List, between(1,Times,_), Ls),
    append(Ls, RepList).

<<<<<<< HEAD
%% Math helper predicates %%
add(N0,N1,R) :- R is N1 + N0.
sub(N0,N1,R) :- R is N1 - N0.
mul(N0,N1,R) :- R is N1 * N0.
dvd(N0,N1,R) :- R is N1 rdiv N0.
pow(N0,N1,R) :- R is N1 ** N0.
sqr(N0,R) :- R is sqrt(N0).
abs(N0,R) :- R is abs(N0).

=======
% Arithmetic functions as predicates
add(N0, N1, R) :- R is N1+N0.
sub(N0, N1, R) :- R is N1-N0.
mul(N0, N1, R) :- R is N1*N0.
dvd(N0, N1, R) :- R is N1/N0.
pow(N0, N1, R) :- R is N1**N0.
sqr(N0, R) :- R is sqrt(N0).
abs(N0, R) :- R is abs(N0).

% Range functions
>>>>>>> back_to_float
srange(Rel, From, Step, To, Current, [Current|Range]) :-
    call(Rel, Current, To),
    Next is Current + Step,
    !, srange(Rel, From, Step, To, Next, Range).
<<<<<<< HEAD
srange(_,_,_,_,_,[]).
=======
srange(_, _, _, _, _, []).

lrange([], _, _).
lrange([Current|Range], Current, Step) :-
    Next is Current + Step,
    lrange(Range, Next, Step).
>>>>>>> back_to_float

lrange([], _, _).
lrange([Current|Range], Current, Step) :-
    Next is Current + Step,
    lrange(Range, Next, Step).
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
print_n(Rational) :- rational(Rational),
    format('~g', [Rational]).

%% Input %%

% From a line of input, make a stack element
parse_line(end_of_file, quit).
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

parsed(parsed(n,R)) --> number(N), !, { R is rationalize(N) }.
parsed(parsed(c,C)) --> command(C), !.
% will fail if the the token is not recognized

% Arithmetic operators
command(add) --> "+".
command(sub) --> "-".
command(mul) --> "*".
command(dvd) --> "/".
command(pow) --> "pow".
command(sqr) --> "sqrt".
command(abs) --> "abs".
% you can put these in a list probably...
% Stack operators
command(top) --> "top".
command(show) --> "show".
command(swap) --> "swap".
command(revstack) --> "revstack".
command(nrevstack) --> "nrevstack".
command(duplicate) --> "duplicate".
command(pop) --> "pop".
command(clear) --> "clear".
% List operators
command(len) --> "len".
command(sum) --> "sum".
command(prod) --> "prod".
command(mean) --> "mean".
command(median) --> "median".
command(sort) --> "sort".
command(set) --> "set".
command(rev) --> "rev".
command(shuffle) --> "shuffle".
command(bind) --> "bind".
command(nbind) --> "nbind".
command(split) --> "split".
command(nsplit) --> "nsplit".
command(range) --> "range".
command(srange) --> "srange".
command(lrange) --> "lrange".
<<<<<<< HEAD
=======

>>>>>>> back_to_float
% Commands
command(quit) --> "quit".

% From a list of parsed, tagged elements, make a valid stack element
normalize(Parsed, Line) :-
    (   Parsed = [] -> Line = empty
    ;   maplist(parsed_nvals, Parsed, NList)
    ->  Line = e(n, NList)
    ;   Parsed = [parsed(c, Command)] % a command
    ->  Line = e(c, Command)
    ;   Line = error % validation failed
    ).

parsed_nvals(parsed(n,N), N).
stacked_nvals(e(n,N), N).

