#!/home/boris/bin/swipl -q -g main -t halt(1) -s

main :-
    init(Stack),
    loop(Stack),
    cleanup.

init([]) :-
    prompt(_, '').

cleanup :-
    format('bye!~n').

quit(end_of_file).

loop(Stack) :-
    prolog_current_frame(F),
    write(F), nl,
    read_line_to_codes(user_input, Codes),
    (   quit(Codes) -> true
    ;   parse_line(Codes, Line),
        reduce_stack([Line|Stack], NewStack),
        format('~w~n', [NewStack]),
        loop(NewStack)
    ).

reduce_stack([e(Type,Content)|Rest], NewStack) :-
    rs(Type, Content, Rest, NewStack), !.
reduce_stack([error|Rest], Rest).

rs(s, top, [Top|Rest], [Top|Rest]) :- print_se(Top).
rs(s, show, Stack, Stack) :- write(Stack), nl, print_s(Stack).
% a temporary catch-all
rs(Type, Content, Stack, [e(Type,Content)|Stack]).

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

command(quit) --> "quit".
