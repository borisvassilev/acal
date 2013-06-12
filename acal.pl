#!/home/boris/bin/swipl -q -g main -f

:- use_module(library('dcg/basics')).

main :-
    init(S, G),
    loop(user_input, S, G, EndS, EndG),
    quit(EndS, EndG).

init(S, G) :-
    get_options(OptPairs),
    list_to_assoc([popreg-[]|OptPairs], InitG),
    (   open_initfile(InitG, InitStream)
    ->  loop(InitStream, [], InitG, S, G)
    ;   S = [], G = InitG
    ),

    prompt(_, '').

open_initfile(G, InitStream) :-
    get_assoc(initfile, G, InitFile),
    atom(InitFile),
    access_file(InitFile, read),
    open(InitFile, read, InitStream).

quit(S, G) :-
    print_s(S),
    print_g(G),
    halt(1).

get_options(OptionPairs) :-
    OptSpecs = [
        [opt(verbose), type(integer), default(2),
         shortflags([v]), longflags([verbosity]),
         help([ 'verbosity level,'
              , '0: only explicit print commands'
              , '1: print out results of arithmetic operations'
              , '2: print out results of all operations on numbers'
              ])
        ]
    ,   [opt(initfile), meta('INITFILE'), type(atom),
         shortflags([i]), longflags([initfile]),
         help(['evaluate INITFILE before interactive prompt'])
        ]
    ],
    catch(
        opt_arguments(OptSpecs, Opts, _),
        error(_E, _M),
        (
            opt_parse(OptSpecs, [], Opts, _) % use defaults
        )
    ),
    maplist(opt_pairs, Opts, OptionPairs),
    write(OptionPairs),nl.
opt_pairs(Opt, Key-Val) :- Opt =.. [Key,Val].

% Evaluation loop of the calculator with the stack as an argument
loop(IS, S, G, EndS, EndG) :-
    read_line_to_codes(IS, Codes),
    parse_line(Codes, Input),
    Input \= quit,
    reduce_stack(Input, S, G, NewS, NewG),
    !, loop(IS, NewS, NewG, EndS, EndG).
loop(_, S, G, S, G). % close the loop

%% Reduce the current stack %%
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
    print_se(T, C).

% Show the whole stack, top to bottom
do_command(show,
        S, G,
        S, G
    ) :-
    print_s(S).

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
    get_assoc(popreg, G, PR, NewG, [el(T,C)|PR]).

% Push the top of the register back to the top of the stack
do_command(push,
        S, G,
        [Top|S], NewG
    ) :-
    get_assoc(popreg, G, [Top|PR], NewG, PR).

% Clear the pop register
do_command(clearpopreg,
        S, G,
        S, NewG
    ) :-
    put_assoc(popreg, G, [], NewG).

% Delete the value on top
do_command(del,
        [_Top|S], G,
        S, G
    ).

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
    length(N, Len),
    vprint(el(n,[Len]), G, 1).

% Sum of the list of numbers
do_command(sum,
        [el(n,N)|S], G,
        [el(n,[Sum]),el(n,N)|S], G
    ) :-
    sum_list(N, Sum),
    vprint(el(n,[Sum]), G, 1).

% Product of the list of numbers
do_command(prod,
        [el(n,N)|S], G,
        [el(n,[Prod]),el(n,N)|S], G
    ) :-
    foldl(mul, N, 1, Prod),
    vprint(el(n,[Prod]), G, 1).

% Arithmetic mean of the list of numbers
do_command(mean,
        [el(n,N)|S], G,
        [el(n,[Mean]),el(n,N)|S], G
    ) :-
    length(N, Len),
    sum_list(N, Sum),
    Mean is Sum/Len,
    vprint(el(n,[Mean]), G, 1).

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
    ),
    vprint(el(n,[Median]), G, 1).

% Sort the numbers in increasing order
% - do not remove duplicates
do_command(sort,
        [el(n,N)|S], G,
        [el(n,SN)|S], G
    ) :-
    msort(N, SN),
    vprint(el(n,SN), G, 2).

% Numbers sorted in increasing order, without duplicates (set)
do_command(set,
        [el(n,N)|S], G,
        [el(n,SN)|S], G
    ) :-
    sort(N, SN),
    vprint(el(n,SN), G, 2).

% Reverse the order of numbers
do_command(rev,
        [el(n,N)|S], G,
        [el(n,RN)|S], G
    ) :-
    reverse(N, RN),
    vprint(el(n,RN), G, 2).

% Shuffle the numbers randomly
do_command(shuffle,
        [el(n,N)|S], G,
        [el(n,SN)|S], G
    ) :-
    random_permutation(N, SN),
    vprint(el(n,SN), G, 2).

% Make one list of numbers from the top two lists of numbers
% - top element is at the front of the new list
do_command(bind,
        [el(n,N0),el(n,N1)|S], G,
        [el(n,B)|S], G
    ) :-
    append(N0, N1, B),
    vprint(el(n,B), G, 2).

% Make one list of number from the top N lists of numbers on the stack
% - N is the single integer value on the top of the stack
% - "older" (lower in the stack) elements come after "newer" elements
do_command(nbind,
        [el(n,[N])|S], G,
        [el(n,BoundNs)|Rest], G
    ) :-
    integer(N), N > 1,
    length(Ns, N), append(Ns, Rest, S),
    maplist(stacked_nvals, Ns, ExtrNs),
    append(ExtrNs, BoundNs),
    vprint(el(n,BoundNs), G, 2).

% Split off the first number from a list of numbers
% - The element is now the new top
% - the list of numbers must have more than one elements
do_command(split,
        [el(n,[N0,N1|NRest])|S], G,
        [el(n,[N0]),el(n,[N1|NRest])|S], G
    ) :-
    vprint([el(n,[N0]),el(n,[N1|NRest])], G, 2).

% Split off the first N numbers from a list of numbers
% - The split off elements are now the top
% - the list of numbers must have more than N elements
do_command(nsplit,
        [el(n,[N]),el(n,Ns)|S], G,
        [el(n,Front),el(n,[B|Back])|S], G
    ) :-
    integer(N), N > 0,
    length(Front, N),
    append(Front, [B|Back], Ns),
    vprint([el(n,Front),el(n,[B|Back])], G, 2).

% Make an list of integers [From, To)
do_command(range,
        [el(n,[From,To])|S], G,
        [el(n,Range)|S], G
    ) :-
    integer(From), integer(To),
    (   From < To ->  srange(<, From, 1, To, From, Range)
    ;   From > To ->  srange(>, From, -1, To, From, Range)
    ),
    vprint(el(n,Range), G, 2).

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
    ),
    vprint(el(n,Range), G, 2).

% Make a list of integers [From, From+Step, ...) of length Len
do_command(lrange,
        [el(n,[From,Step,Len])|S], G,
        [el(n,Range)|S], G
    ) :-
    integer(From), integer(Step), integer(Len), Len > 0,
    length(Range, Len),
    lrange(Range, From, Step),
    vprint(el(n,Range), G, 2).

% Create a list of the values in [1] indexed by the integers in [0]
do_command(nth,
        [el(n,Indices),el(n,Ns)|S], G,
        [el(n,Nths),el(n,Ns)|S], G
    ) :-
    nths(Indices, Ns, Nths),
    vprint(el(n,Nths), G, 2).

do_command(rep,
        [el(n,[Rep]),el(n,Ns)|S], G,
        [el(n,RepNs)|S], G
    ) :-
    integer(Rep), Rep > 0,
    rep(Ns, Rep, RepNs),
    vprint(el(n,RepNs), G, 2).

% Do arithmetic operations
do_command(BinOp,
        [el(n,N0),el(n,N1)|S], G,
        [el(n,R)|S], G
    ) :-
    memberchk(BinOp, [add,sub,mul,dvd,pow]), % binary operator
    eq_len(N0, N1, NewN0, NewN1), % could fail!
    maplist(BinOp, NewN0, NewN1, R),
    vprint(el(n,R), G, 1).
do_command(BinOpBinR,
        [el(n,N0),el(n,N1)|S], G,
        [el(n,R0),el(n,R1)|S], G
    ) :-
    memberchk(BinOpBinR, [intdiv]), % binary operator, two results
    eq_len(N0, N1, NewN0, NewN1), % could fail!
    maplist(BinOpBinR, NewN0, NewN1, R0, R1),
    vprint([el(n,R0),el(n,R1)], G, 1).
do_command(UnOp,
        [el(n,N)|S], G,
        [el(n,R)|S], G
    ) :-
    memberchk(UnOp, [sqr,abs]), % unary operator
    maplist(UnOp, N, R),
    vprint(el(n,R), G, 1).

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
add(N0, N1, R) :- R is N1 + N0.
sub(N0, N1, R) :- R is N1 - N0.
mul(N0, N1, R) :- R is N1 * N0.
dvd(N0, N1, R) :- N0 =\= 0, R is N1 / N0.
pow(N0, N1, R) :- R is N1 ** N0.
sqr(N0, R) :- R is sqrt(N0).
abs(N0, R) :- R is abs(N0).

intdiv(N0, N1, R0, R1) :-
    I0 is truncate(N0),
    I1 is truncate(N1),
    R0 is I1 // I0,
    R1 is I1 rem I0.

%% Helper predicates %%

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

% Select elements with indices
% Select elements with indices
nths(Ns, List, Nths) :-
    LFunc =.. [listfunctor|List],
    nths_(Ns, LFunc, Nths).
nths_([], _, []).
nths_([N|Ns], LFunc, [E|Es]) :-
    integer(N), N1 is N + 1,
    arg(N1, LFunc, E),
    nths_(Ns, LFunc, Es).

%% Output %%

% Print if verbosity level is appropriate
vprint(X, G, MinLevel) :-
    get_assoc(verbose, G, Level),
    (   Level >= MinLevel -> vprint(X), !
    ;   true
    ).
% Delegate printing
vprint(el(n,Ns)) :- print_ns(Ns).
vprint(el(c,C)) :- print_se(c, C).
vprint([H|Tail]) :- print_s([H|Tail]).

% Print global state
print_g(G) :-
    forall(
        gen_assoc(K, G, V),
        format('~w-~w~n', [K,V])
    ).

% Print a list of stack elements
print_s([el(Type,Content)|Es]) :-
    print_se(Type,Content),
    !, print_s(Es).
print_s([]).

% Print a stack element
print_se(n, N) :- print_ns(N). % print a list of numbers
print_se(c, Command) :- % print a command
    % convert back to original representation
    phrase( command(Command), String),
    format('~s~n', [String]).

% Print a list of numbers on a line
print_ns([N|Ns]) :- % guaranteed to have at least one element
    print_ns(Ns, N).

print_ns([N|Ns], Prev) :-
    print_n(Prev),
    format(' '),
    print_ns(Ns, N).
print_ns([], Last) :-
    print_n(Last),
    format('~n').

print_n(Int) :-
    integer(Int),
    format('~d', [Int]).
print_n(Float) :-
    float(Float),
    format('~g', [Float]).
print_n(Rational) :-
    rational(Rational),
    format('~g', [Rational]).

%% Input %%

% From a line of input, make a stack element
parse_line(end_of_file, quit) :- !.
parse_line("quit", quit) :- !.
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

parsed(parsed(n,N)) --> number(N).
parsed(parsed(c,C)) --> command(C).
% will fail if the the token is not recognized

% Arithmetic operators
command(add) --> "+".
command(sub) --> "-".
command(mul) --> "*".
command(dvd) --> "/".
command(pow) --> "pow".
command(sqr) --> "sqrt".
command(abs) --> "abs".
command(intdiv) --> "//".
% you can put these in a list probably...
% Stack operators
command(top) --> "top".
command(show) --> "show".
command(swap) --> "swap".
command(revstack) --> "revstack".
command(nrevstack) --> "nrevstack".
command(duplicate) --> "duplicate".
command(del) --> "del".
command(pop) --> "pop".
command(push) --> "push".
command(clearpopreg) --> "clearpopreg".
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
command(nth) --> "nth".
command(rep) --> "rep".

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


