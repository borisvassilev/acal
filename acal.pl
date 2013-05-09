#!/home/boris/bin/swipl -q -g main -t halt(1) -s

main :-
    init,
    loop,
    cleanup.

init :-
    prompt(_, '').

cleanup :-
    write(bye), nl.

quit?(end_of_file).

loop :-
    read_line_to_codes(user_input, Codes),
    (  quit?(Codes)
    -> true
    ;  act(Codes), loop
    ).

act(Codes) :-
    format('~s~n', [Codes]).

