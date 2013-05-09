#!/home/boris/bin/swipl -q -g main -t halt(1) -s

main :-
    init,
    loop,
    cleanup.

init :-
    prompt(_, '').

cleanup :-
    write(bye), nl.

quit(end_of_file).

loop :-
    read_line_to_codes(user_input, Codes),
    (  quit(Codes)
    -> true
    ;  act(Codes), loop
    ).

act(Codes) :-
    tokenize(Codes, Tokens),
    maplist(parse, Tokens, Parsed),
    format('~w~n', [Parsed]).

tokenize(Codes, Tokens) :-
    phrase( line(Tokens), Codes ).

parse(Token, Parsed) :-
    phrase( parsed(Parsed), Token ).
parse(Token, u(Unknown)) :-
    atom_codes(Unknown, Token).

:- use_module(library('dcg/basics')).

line(Tokens) --> tokens(Tokens).
tokens([]) --> blanks_to_nl, !.
tokens([T|Ts]) --> token(T), tokens(Ts).

token(T) --> whites, nonblanks(T).

parsed(v(V)) --> number(V), !.
parsed(a(AO)) --> arithop(AO), !.
parsed(s(SO)) --> stackop(SO), !.
parsed(l(LO)) --> listop(LO), !.
parsed(c(C)) --> command(C), !.

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
