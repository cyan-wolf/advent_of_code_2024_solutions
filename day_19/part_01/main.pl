:- set_prolog_flag(double_quotes, chars).

% available (?TowelPart)
% Allows adding new facts at runtime.
:- dynamic(available/1).

main :-
    % Read input filename from command arguments.
    current_prolog_flag(argv, [InputFilePath]),

    % Read the input file lines from the input file.
    open(InputFilePath, read, Str),
    read_file(Str, Lines),
    close(Str),

    % Parse the lines from the input file.
    [AvailableRaw,_|Needed] = Lines,
    split_string(AvailableRaw, ",", " ", Available),

    % Dynamically assert the available towels as... available.
    forall(member(TowelPart, Available), assertz(available(TowelPart))),

    % TODO: actually use backtracking to build the needed towels using 
    %       the available towel parts.
    % ...

    % Debug.
    write(Available), nl,
    write(Needed), nl.

% read_file(+Stream, ?Lines)
% Reads file lines as a list from the given input stream.
read_file(Stream, []) :-
    at_end_of_stream(Stream).

read_file(Stream, [Elem|Rest]) :-
    \+ at_end_of_stream(Stream),
    read(Stream, Elem),
    read_file(Stream, Rest), !.