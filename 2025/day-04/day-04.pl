:- dynamic hasRollOfPaper/2.

% Load input

loadFile(File) :-
    open(File, read, Stream),
    readLines(Stream, 0),
    close(Stream).

readLines(Stream, Y) :-
    read_line_to_codes(Stream, Codes),
    ( Codes \= end_of_file ->
        processLine(Codes, 0, Y),
        Y1 is Y + 1,
        readLines(Stream, Y1)
    ; true ).

processLine([], _, _).
processLine([Char|Rest], X, Y) :-
    ( Char = 0'@ -> assertz(hasRollOfPaper(X, Y)) ; true ),
    X1 is X + 1,
    processLine(Rest, X1, Y).

% Part 1

canBeAccessed(X,Y) :-
    hasRollOfPaper(X,Y),
    findall(1, (adjacent(X,Y,Xa,Ya), hasRollOfPaper(Xa,Ya)), Rolls),
    length(Rolls, Count),
    Count < 4.

adjacent(X,Y,Xa,Ya) :-
    member(DX, [-1,0,1]),
    member(DY, [-1,0,1]),
    \+ (DX = 0, DY = 0),
    Xa is X + DX,
    Ya is Y + DY.

countAccessible(Count) :-
    findall((X,Y), canBeAccessed(X,Y), Positions),
    length(Positions, Count).

% Part 2

removeAccessible(Count) :-
    findall((X,Y), canBeAccessed(X,Y), Accessible),
    length(Accessible, N),
    ( N > 0 ->
        removePositions(Accessible),
        removeAccessible(Rest),
        Count is N + Rest
    ; Count = 0 ).

% Remove a list of positions from the database
removePositions([]).
removePositions([(X,Y)|Rest]) :-
    retract(hasRollOfPaper(X,Y)),
    removePositions(Rest).
