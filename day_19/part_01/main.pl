:- set_prolog_flag(double_quotes, chars).

main :-
    open('input_test.pl', read, Str),
    read_file(Str, Lines),
    close(Str),

    [AvailableRaw,_|Needed] = Lines,
    split_string(AvailableRaw, ",", " ", Available),

    write(Available), nl,
    write(Needed), nl.

read_file(Stream, []) :-
    at_end_of_stream(Stream).

read_file(Stream, [Elem|Rest]) :-
    \+ at_end_of_stream(Stream),
    read(Stream, Elem),
    read_file(Stream, Rest).