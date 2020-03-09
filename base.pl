h(1, 4).
h(4, 8).
h(12, 11).
h(19, 14).
o(3, 3).
o(3, 4).
o(3, 5).
o(3, 10).
o(3, 11).
o(3, 14).
o(3, 15).
o(4, 4).
o(4, 9).
o(4, 12).
o(4, 13).
o(4, 16).
o(5, 4).
o(5, 9).
o(5, 16).
o(6, 4).
o(6, 9).
o(6, 16).
o(7, 4).
o(7, 10).
o(7, 15).
o(8, 4).
o(8, 11).
o(8, 14).
o(9, 3).
o(9, 4).
o(9, 5).
o(9, 12).
o(9, 13).
o(11, 4).
o(11, 9).
o(11, 13).
o(11, 14).
o(11, 15).
o(12, 4).
o(12, 9).
o(12, 14).
o(13, 4).
o(13, 9).
o(13, 14).
o(14, 4).
o(14, 9).
o(14, 14).
o(15, 4).
o(15, 9).
o(15, 14).
o(16, 4).
o(16, 9).
o(16, 14).
o(17, 5).
o(17, 6).
o(17, 7).
o(17, 8).
o(17, 13).
o(17, 14).
o(17, 15).
t(6, 12).
t(6, 13).
t(19, 19).


on_map(I, J):-
    between(0, 19, I),
    between(0, 19, J).

    
is_human([I, J]) :-
    not(I = 0), not(J = 0),
    h(I, J), not(t(I, J)).


last(X, [X]).
last(X, [_|Z]) :-
    last(X, Z).

print_path([]).

print_path([[I, J] | T]) :-
    format("~p ~n", [[I, J]]),
    print_path(T).

print_path([["P", [I, J]] | T]) :-
    write("P "),
    format("~p ~n", [[I, J]]),
    print_path(T).


sort_paths_by_length(Paths, Sorted) :-
    map_list_to_pairs(length, Paths, LengthPath),
    keysort(LengthPath, SortedPairs),
    pairs_values(SortedPairs, Sorted).


valid_move_direction([0, 1]).
valid_move_direction([1, 0]).
valid_move_direction([0, -1]).
valid_move_direction([-1, 0]).


valis_pass_direction([-1, -1]).
valis_pass_direction([-1, 0]).
valis_pass_direction([-1, 1]).
valis_pass_direction([0, -1]).
valis_pass_direction([0, 1]).
valis_pass_direction([1, -1]).
valis_pass_direction([1, 0]).
valis_pass_direction([1, 1]).



% [I, J] - current position of the runner;
% [V, H] - the vertical (V) and horizontal (H) components of the move the runner is gonna make;
% [I1, J1] - position of runner given that he took a move [V, H].
can_move([I, J], [V, H], [I1, J1]) :-
    valid_move_direction([V, H]),
    I1 is I + V,
    J1 is J + H,
    on_map(I1, J1),
    \+(o(I1, J1)).

% [I, J] - current position of the ball;
% [V, H] - the vertical (V) and horizontal (H) components of the direction the ball took while flying;
% [I1, J1] - position of the ball given that it was thrown in direction [V, H].
can_move_pass([I, J], [V, H], [I1, J1]) :-
    valis_pass_direction([V, H]),
    I1 is I + V,
    J1 is J + H,
    on_map(I1, J1),
    \+ (o(I1, J1)).

can_pass([I, J], [V, H], [I1, J1]) :-
    can_move_pass([I, J], [V, H], [I1, J1]),
    h(I1, J1).


% [I, J] - current position of the ball;
% [V, H] - the vertical (V) and horizontal (H) the ball took as it flew to an ally human;
% [I2, J2] - position of the ally human.
can_pass([I, J], [V, H], [I2, J2]) :-
    not(h(I1, J1)),
    can_move_pass([I, J], [V, H], [I1, J1]),
    can_pass([I1, J1], [V, H], [I2, J2]).


%%%%%% BACKTRACKING SEARCH %%%%%%

% 1. base case: player is already at the touchdown point
find_backtrack_path([I, J], Path, RevPath, _)  :-
    t(I, J),
    reverse(Path, RevPath).


% 2. attempt to move
% [I, J] - current position of the player;
% CurrentPath - cells, that the player has visited so far;
% Path - the final path;
% Pass - true/0 if pass can still be made, false/0 if not.
find_backtrack_path([I, J], CurrentPath, Path, Pass) :-
	can_move([I, J], _, [I1, J1]),
    \+( member([I1, J1], CurrentPath) ),
    find_backtrack_path([I1, J1], [[I1, J1] | CurrentPath], Path, Pass).

% 3. attempt to make a pass
find_backtrack_path([I, J], CurrentPath, Path, Pass) :-
    call(Pass),
	can_pass([I, J], _, [I1, J1]),
    \+( member([I1, J1], CurrentPath) ),
    find_backtrack_path([I1, J1], [ ["P", [I1, J1]] | CurrentPath], Path, false).


solve_backtrack(P) :-
    not(o(0, 0)),
    find_backtrack_path([0, 0], [[0, 0]], P, true).

%%%%%% RANDOM SEARCH %%%%%%%

random_move([I, J], Path, [I1, J1]) :-
    findall([I1, J1],
        (
            can_move([I, J], _, [I1, J1]),
            not(member([I1, J1], Path))
        ),
        Ds),
    random_member([I1, J1], Ds).


% 1. when touchdown is reached, return RevPath
find_random_path([I, J], Path, RevPath, _) :-
    t(I, J),
    reverse(Path, RevPath).


% 2. when random search fails, return RevPath
find_random_path([I, J], Path, RevPath, Pass) :-
    not(random_move([I, J], Path, _)),
    not(call(Pass)),
    reverse(Path, RevPath).


% 3. attempts to make a pass
find_random_path([I, J], CurrentPath, Path, Pass) :-
    call(Pass),
    can_pass([I, J], _, [I1, J1]),
    find_random_path([I1, J1], [ ["P", [I1, J1]]| CurrentPath ], Path, false).

% 4. attempts to make a move
find_random_path([I, J], CurrentPath, Path, Pass) :-
    random_move([I, J], CurrentPath, [I1, J1]),
    find_random_path([I1, J1], [ [I1, J1] | CurrentPath ], Path, Pass).
    

% 100 iterations of random search

% stops when C reaches 100
solve_random_(L, L, 100).

% executes find_random_path 100 - C times
solve_random_(CurrentL, L, C) :-
    C1 is C + 1,
    C1 =< 100,
    find_random_path([0, 0], [[0, 0]], P, true),

    % only successful attempts are returned
    ( 
        (last([I, J], P), t(I, J)) ->
        solve_random_([P | CurrentL], L, C1) ;
        solve_random_(CurrentL, L, C1)
    ).

solve_random(F) :-
    not(o(0, 0)),
    solve_random_([], L, 0),
    sort_paths_by_length(L, Sorted),
    Sorted = [F | _].
    % print_path(F).




%%%%%%% BREADTH-FIRST SEARCH %%%%%%%

% find all the possible moves 
move_successors([[I, J], History, Pass], Succ):-
    findall(
       [[I1, J1], [[I1, J1] | History], NewPass],
        (
            can_move([I, J], _, [I1, J1]),
            not(member([I1, J1], History)),
            NewPass = Pass
        ),
       Succ
    ).


% find the positions of all the possible players to whom a ball may be passed
pass_successors([[I, J], History, Pass], Succ):-
    findall(
       [[I1, J1], [[I1, J1] | History], NewPass],
        (
            Pass = true,
            can_pass([I, J], _, [I1, J1]),
            not(member([I1, J1], History)),
            NewPass = false
        ),
       Succ
    ).


% 1. base case: the touchdown point is found.
solve_BFS_([[[I, J], History, _] | _], Path) :-
    t(I, J),
    reverse(History, Path).

    
% 2. recursion:
%    [[I, J], History, Pass] - an element of the Queue that is maintained by the algorithm;
%           [I, J] stores the destination;
%           History stores the moves taken to reach the destination;
%           Pass is true/0 if pass can still be made, false/0 otherwise
%    Path is the final answer
solve_BFS_([[[I, J], History, Pass] | T], Path) :-
    move_successors([[I, J], History, Pass], MoveSucc),
    pass_successors([[I, J], History, Pass], PassSucc),
    append(T, MoveSucc, Q),     % appending all the move and pass successors
    append(Q, PassSucc, Queue), % to the Queue to be processed later
    Queue = [[[_, _], _, _] | _],
    solve_BFS_(Queue, Path).


% runs solve_BFS_/2 with the appropriate intitial conditions
solve_BFS(P) :-
    not(o(0, 0)),
    solve_BFS_([[[0, 0], [[0, 0]], true]], P).
    % print_path(P).

% runs 
timed_random(P, Time) :-
    get_time(Start),
    solve_random(P),
    get_time(End),
    Time is End - Start.
    


timed_backtrack(P, Time) :-
    get_time(Start),
    solve_backtrack(P),
    get_time(End),
    Time is End - Start.


timed_BFS(P, Time) :-
    get_time(Start),
    solve_BFS(P),
    get_time(End),
    Time is End - Start.
    
    