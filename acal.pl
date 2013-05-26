#!/home/boris/bin/swipl -q -t main -f

:- use_module(library('dcg/basics')).

main :-
    init(S, G),
    loop(S, G).

init([], G) :-
    prompt(_, ''),
    list_to_assoc([popreg-[]], G).

quit(S, G) :-
    print_s(S, G),
    halt(1).

% Evaluation loop of the calculator with the stack as an argument
loop(S, G) :-
    %/* DEBUG */ prolog_current_frame(F),
    %            write(F), nl,
    read_line_to_codes(user_input, Codes),
    parse_line(Codes, Input), 
    /* DEBUG */ format('~w~n', Input),
    reduce_stack(Input, S, G, NewS, NewG),
    /* DEBUG */ format('~w~n~w~n', [NewS, NewG]),
    !, loop(NewS, NewG).

/* TODO */
% make it possible to push operators and values
% to the stack directly from the program

%% Reduce the current stack %%
reduce_stack(quit, S, G, S, G) :- quit(S, G).
reduce_stack(error, S, G, S, G). % ignore for now
reduce_stack(empty, S, G, S, G). % ignore this one too
reduce_stack(el(c,Command), S, G, NewS, NewG) :-
    do_command(Command, S, G, NewS, NewG).
reduce_stack(New, S, G, [New|S], G). % the stack could not be reduced

%% Commands %%

% Show the top element of the stack
% - stack must have at least one element
do_command(top,
        [el(T,C)|S], G,
        [el(T,C)|S], G
    ) :-
    print_se(T, C, G).

% Show the whole stack, top to bottom
do_command(show,
        S, G,
        S, G
    ) :-
    print_s(S, G).

% Swap the top two element of the stack
% - stack must have at least two elements
do_command(swap,
        [E0,E1|S], G,
        NewS, G
    ) :-
    reduce_stack(E1, [E0|S], G, NewS, G).

% Reverse the whole stack
do_command(revstack,
        S, G,
        NewS, G
    ) :-
    reverse(S, [Top|RS]),
    reduce_stack(Top, RS, G, NewS, G).

% Reverse the order of the N elements of the stack below the top
do_command(nrevstack,
        [el(n,[N])|S], G,
        NewS, G
    ) :-
    integer(N), N > 0,
    length(First, N),
    append(First, Rest, S),
    reverse(First, Rev),
    append(Rev, Rest, [Top|CurS]),
    reduce_stack(Top, CurS, G, NewS, G).

% Add a copy of the top element on top of the stack
% - stack must have at least one element
do_command(duplicate,
        [Top|S], G,
        [Top,Top|S], G
    ).

% Pop the value on top to the top of a register
do_command(pop,
        [el(T,C)|S], G,
        S, NewG
    ) :-
    print_se(T, C, G),
    get_assoc(popreg, G, PR, NewG, [el(T,C)|PR]).

% Push the top of the register back to the top of the stack
do_command(push,
        S, G,
        [Top|S], NewG
    ) :-
    get_assoc(popreg, G, [Top|PR], NewG, PR).

% Clear the stack (rendering it empty)
do_command(clear,
        _S, G,
        [], G
    ).

% The following commands: len, sum, prod, mean, median
% push their result to the top of the stack without
% removing the list of numbers they are applied to
%
% Length of the list of numbers
do_command(len,
        [el(n,N)|S], G,
        [el(n,[Len]),el(n,N)|S], G
    ) :-
    length(N, Len).

% Sum of the list of numbers
do_command(sum,
        [el(n,N)|S], G,
        [el(n,[Sum]),el(n,N)|S], G
    ) :-
    sum_list(N, Sum).

% Product of the list of numbers
do_command(prod,
        [el(n,N)|S], G,
        [el(n,[Prod]),el(n,N)|S], G
    ) :-
    foldl(mul, N, 1, Prod).

% Arithmetic mean of the list of numbers
do_command(mean,
        [el(n,N)|S], G,
        [el(n,[Mean]),el(n,N)|S], G
    ) :-
    length(N, Len),
    sum_list(N, Sum),
    Mean is Sum/Len.

% Median of the list of numbers
% - if even number of elements, take arithmetic mean of middle two
do_command(median,
        [el(n,N)|S], G,
        [el(n,[Median]),el(n,N)|S], G
    ) :-
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
do_command(sort,
        [el(n,N)|S], G,
        [el(n,SN)|S], G
    ) :-
    msort(N, SN).

% Numbers sorted in increasing order, without duplicates (set)
do_command(set,
        [el(n,N)|S], G,
        [el(n,SN)|S], G
    ) :-
    sort(N, SN).

% Reverse the order of numbers
do_command(rev,
        [el(n,N)|S], G,
        [el(n,RN)|S], G
    ) :-
    reverse(N, RN).

% Shuffle the numbers randomly
do_command(shuffle,
        [el(n,N)|S], G,
        [el(n,SN)|S], G
    ) :-
    random_permutation(N, SN).

% Make one list of numbers from the top two lists of numbers
% - top element is at the back of the new list
do_command(bind,
        [el(n,N0),el(n,N1)|S], G,
        [el(n,B)|S], G
    ) :-
    append(N1, N0, B).

% Make one list of number from the top N lists of numbers on the stack
% - N is the single integer value on the top of the stack
% - "older" (lower in the stack) elements come before "newer" elements
do_command(nbind,
        [el(n,[N])|S], G,
        [el(n,BoundNs)|Rest], G
    ) :-
    integer(N), N > 1,
    length(Ns, N), append(Ns, Rest, S),
    maplist(stacked_nvals, Ns, ExtrNs),
    reverse(ExtrNs, RevExtrNs),
    append(RevExtrNs, BoundNs).

% Split off the first number from a list of numbers
% - The element is now the new top
% - the list of numbers must have more than one elements
do_command(split,
        [el(n,[N0,N1|NRest])|S], G,
        [el(n,[N0]),el(n,[N1|NRest])|S], G
    ).

% Split off the first N numbers from a list of numbers
% - The split off elements are now the top
% - the list of numbers must have more than N elements
do_command(nsplit,
        [el(n,[N]),el(n,Ns)|S], G,
        [el(n,Front),el(n,[B|Back])|S], G
    ) :-
    integer(N), N > 0,
    length(Front, N),
    append(Front, [B|Back], Ns).

% Make an list of integers [From, To)
do_command(range,
        [el(n,[From,To])|S], G,
        [el(n,Range)|S], G
    ) :-
    integer(From), integer(To),
    (   From < To ->  srange(<, From, 1, To, From, Range)
    ;   From > To ->  srange(>, From, -1, To, From, Range)
    ).

% Make a list of integers [From, From+Step, ..., To)
do_command(srange,
        [el(n,[From,Step,To])|S], G,
        [el(n,Range)|S], G
    ) :-
    integer(From), integer(Step), integer(To),
    (   From < To, Step > 0
    ->  srange(<, From, Step, To, From, Range)
    ;   From > To, Step < 0
    ->  srange(>, From, Step, To, From, Range)
    ).

% Make a list of integers [From, From+Step, ...) of length Len
do_command(lrange,
        [el(n,[From,Step,Len])|S], G,
        [el(n,Range)|S], G
    ) :-
    integer(From), integer(Step), integer(Len), Len > 0,
    length(Range, Len),
    lrange(Range, From, Step).

% Do arithmetic operations
do_command(BinOp,
        [el(n,N0),el(n,N1)|S], G,
        [el(n,R)|S], G
    ) :-
    memberchk(BinOp, [add,sub,mul,dvd,pow]), % binary operator
    eq_len(N0, N1, NewN0, NewN1), % could fail!
    maplist(BinOp, NewN0, NewN1, R),
    print_ns(R).

do_command(UnOp,
        [el(n,N)|S], G,
        [el(n,R)|S], G
    ) :-
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
    0 =:= L1 mod L0, Times is L1 div L0,
    rep(N0, Times, NewN0).
eq_len((>), N0, L0, N1, L1, N0, NewN1) :-
    0 =:= L0 mod L1, Times is L0 div L1,
    rep(N1, Times, NewN1).
% Repeat a list
rep(List, Times, RepList) :-
    findall(List, between(1,Times,_), Ls),
    append(Ls, RepList).

% Arithmetic functions as predicates
add(N0, N1, R) :- R is N1+N0.
sub(N0, N1, R) :- R is N1-N0.
mul(N0, N1, R) :- R is N1*N0.
dvd(N0, N1, R) :- R is N1/N0.
pow(N0, N1, R) :- R is N1**N0.
sqr(N0, R) :- R is sqrt(N0).
abs(N0, R) :- R is abs(N0).

% Range functions
srange(Rel, From, Step, To, Current, [Current|Range]) :-
    call(Rel, Current, To),
    Next is Current + Step,
    !, srange(Rel, From, Step, To, Next, Range).
srange(_, _, _, _, _, []).

lrange([], _, _).
lrange([Current|Range], Current, Step) :-
    Next is Current + Step,
    lrange(Range, Next, Step).

lrange([], _, _).
lrange([Current|Range], Current, Step) :-
    Next is Current + Step,
    lrange(Range, Next, Step).
%% Output %%

% Print the whole stack
print_s([el(Type,Content)|Es], G) :- print_se(Type,Content, G), !, print_s(Es, G).
print_s([], _).

% Print a stack element
print_se(n, N, _G) :- print_ns(N). % print a list of numbers
print_se(c, Command, _G) :- % print a command
    % convert back to original representation
    phrase( command(Command), String),
    format('~s~n', [String]).
% catch-all clause, for now
print_se(_Type, Name, _G) :- format('~w~n', [Name]).

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

parsed(parsed(n,N)) --> number(N), !.
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
command(push) --> "push".
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

% Commands
command(quit) --> "quit".

% From a list of parsed, tagged elements, make a valid stack element
normalize(Parsed, Line) :-
    (   Parsed = [] -> Line = empty
    ;   maplist(parsed_nvals, Parsed, NList)
    ->  Line = el(n, NList)
    ;   Parsed = [parsed(c, Command)] % a command
    ->  Line = el(c, Command)
    ;   Line = error % validation failed
    ).

parsed_nvals(parsed(n,N), N).
stacked_nvals(el(n,N), N).


