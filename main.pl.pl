%Reference:
%https://github.com/MontyWest/prolog/

:- retractall(size(_,_)).
:- retractall(covid(_,_)).
:- retractall(doctor(_,_)).
:- retractall(mask(_,_)).
:- retractall(home(_,_)).
:- dynamic doctor/2.
:- dynamic home/2.
:- dynamic mask/2.
:- dynamic size/2.
:- dynamic covid/2.

actor(0,0).


build_map:-
  trace,
  map_size, create_home, create_covid1, create_covid2, create_doctor,create_mask, print_map.
run:-
   map_size, create_home, create_covid1, create_covid2, create_mask, create_doctor, home(X,Y), solve([0,0], [X,Y], Path).

map_size :-
    write('enter rows: '),
    read(X),
    write('enter columns'),
    read(Y),
    assertz(size(X,Y)).

% creating Covid1 and Covid2 
create_covid1 :-
    size(A,B), random(0,B,X), random(0,A,Y),
    ( \+ actor(X,Y), (X=\=1, Y=\=0), (X=\=1, Y=\=1), (X=\=0, Y=\=1) )-> (assertz(covid(X,Y)), infection1(X,Y));
    create_covid1.

infection1(X,Y) :-
    A1 is X - 1, B1 is Y - 1, assertz(covid(A1, B1)),
    A2 is X - 1, B2 is Y,     assertz(covid(A2, B2)),
    A3 is X - 1, B3 is Y+1,   assertz(covid(A3, B3)),
    A4 is X,     B4 is Y+1,   assertz(covid(A4, B4)),
    A5 is X+1,   B5 is Y+1,   assertz(covid(A5, B5)),
    A6 is X+1,   B6 is Y,     assertz(covid(A6, B6)),
    A7 is X+1,   B7 is Y-1,   assertz(covid(A7, B7)),
    A8 is X,     B8 is Y-1,   assertz(covid(A8, B8)).

create_covid2 :-
    size(A,B), random(0,B,X), random(0,A,Y),
    ( \+ actor(X,Y), \+ covid(X,Y), (X=\=1, Y=\=0), (X=\=1, Y=\=1),(X=\=0, Y=\=1)) -> (assertz(covid(X,Y)), infection2(X,Y));
    create_covid2.

infection2(X,Y) :-
    A1 is X - 1, B1 is Y - 1, assertz(covid(A1, B1)),
    A2 is X - 1, B2 is Y,     assertz(covid(A2, B2)),
    A3 is X - 1, B3 is Y+1,   assertz(covid(A3, B3)),
    A4 is X,     B4 is Y+1,   assertz(covid(A4, B4)),
    A5 is X+1,   B5 is Y+1,   assertz(covid(A5, B5)),
    A6 is X+1,   B6 is Y,     assertz(covid(A6, B6)),
    A7 is X+1,   B7 is Y-1,   assertz(covid(A7, B7)),
    A8 is X,     B8 is Y-1,   assertz(covid(A8, B8)).

% creating home
create_home :-
    size(A,B), random(0,B,X),random(0,A,Y),
    (\+ actor(X,Y), \+ covid(X,Y)) -> (assertz(home(X,Y)));
    create_home.

% creating doctor
create_doctor :-
    size(A,B), random(0,B,X), random(0,A,Y),
    (\+ actor(X,Y), \+ covid(X,Y), \+ home(X,Y)) -> (assertz(doctor(X,Y)));
    create_doctor.

% creating mask
create_mask :-
    size(A,B), random(0,B,X), random(0,A,Y),
    (\+ actor(X,Y), \+ covid(X,Y), \+ home(X,Y), \+ doctor(X,Y)) -> (assertz(mask(X,Y)));
    create_mask.


% Printing map
print_map :-
  print_map([]).

print_map(Path) :-
  size(Rows, Columns ), nl,
  print_x_axis(0,Columns), nl,
  print_x_margin(0, Columns), nl,
  print_rows(0, Rows, Columns, Path), nl,
  print_x_margin(0,Columns).

print_5_spaces :-
  write('     '). 

print_3_spaces :-
  write('   '). 

print_x_axis(From, To) :-
  print_5_spaces,
  print_column_numbering(From, To).

print_column_numbering(To, To) :-
  write(To).
print_column_numbering(From, To) :-
  From>=To.
print_column_numbering(From, To) :-
  write(From), write(' '),
  Next is From+1,
  print_column_numbering(Next, To),
  !.

print_x_margin(From, To) :-
  print_3_spaces, write('+'), print_map_horiz_border(From, To), write('+').


print_map_horiz_border(To, To) :-
  write('---').
print_map_horiz_border(From, To) :-
  From>=To.  
print_map_horiz_border(From, To) :-
  write('--'),
  Next is From+1,
  print_map_horiz_border(Next, To),
  !.



print_rows(Row, Row, Columns, Path) :-
  print_map_row(Row, 0, Columns, Path).
print_rows(Row, RowTo, _, _) :-
  Row>=RowTo.
print_rows(Row, RowTo, Columns, Path) :-
  print_map_row(Row, 0, Columns, Path),
  nl,RowNext is Row+1,print_rows(RowNext, RowTo, Columns, Path),
  !.

print_map_row(Row, ColumnFrom, ColumnTo, Path) :-
  write(' '), write(Row), write(' '), write('|'),
  print_map_row_inner(Row, ColumnFrom, ColumnTo, Path).


print_map_row_inner(Row, Column, Column, Path):-
  write(' '), print_symbol(Row, Column, Path), write(' |').

print_map_row_inner(_, Column, ColumnTo, _) :-
  Column>=ColumnTo.

print_map_row_inner(Row, Column, ColumnTo, Path) :-
  write(' '), print_symbol(Row, Column, Path),
  NextColumn is Column+1,
  print_map_row_inner(Row, NextColumn, ColumnTo, Path),
  !.

%chracters in the map

% moves are "O"
print_symbol(Row, Column, Path) :-
  memberchk([Row,Column], Path), write('A').


print_symbol(Row, Column, _) :-
  doctor(Row, Column), write('D').


print_symbol(Row, Column, _) :-
  mask(Row, Column), write('M').


print_symbol(Row, Column, _) :-
  covid(Row, Column), write('X').


print_symbol(Row, Column, _) :-
  home(Row, Column), write('H').

print_symbol(Row, Column, _) :-
  actor(Row, Column), write('S').

print_symbol(_, _, _) :-
  write('.').



%correct moves
correct_move([X0,Y0], [X1,Y1]) :- 
  adj_block([X0,Y0], [X1,Y1]), 
  %% free_block(X0,Y0), 
  free_block(X1,Y1).


in_map(X1,Y1) :- 
  size(A,B), 
  X1>=0,
  X1=<A,
  Y1>=0, 
  Y1=<B.

%% free block can be used as path
free_block([X,Y]) :-
  free_block(X,Y).
free_block(X0,Y0) :- 
  in_map(X0,Y0), \+(covid(X0,Y0)).


adj_block([X0,Y0], [X0,Y1]) :- 
  (Y1 is Y0-1).
adj_block([X0,Y0], [X1,Y0]) :- 
  (X1 is X0-1).
adj_block([X0,Y0], [X0,Y1]) :- 
  (Y1 is Y0+1).
adj_block([X0,Y0], [X1,Y0]) :- 
  (X1 is X0+1).
adj_block([X0,Y0], [X1,Y1]) :- 
  (X1 is X0-1), (Y1 is Y0-1).
adj_block([X0,Y0], [X1,Y1]) :- 
  (X1 is X0-1), (Y1 is Y0+1).
adj_block([X0,Y0], [X1,Y1]) :- 
  (X1 is X0+1), (Y1 is Y0-1).
adj_block([X0,Y0], [X1,Y1]) :- 
  (X1 is X0+1), (Y1 is Y0+1).
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BACKTRACKING: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(ST, ET, Path) :-
  free_block(ST),
  free_block(ET),
  size(N,M),
  get_paths_list(ST, ET, N*M, [], PathList),
  !,
  get_shortest_path(Path, PathList),
  print_map(Path).


get_shortest_path(Path, PathList) :-
  get_shortest_path_length(PathList, Min),
  member(Path, PathList),
  length(Path, Min).


get_shortest_path_length([H|T], Min) :-
  length(H, MinStart),
  get_shortest_path_length(T, MinStart, Min).


get_shortest_path_length([], Min, Min).
get_shortest_path_length([NewPath|T], LatestMin, Min) :-
  length(NewPath, N),
  NewMin is min(N, LatestMin), 
  get_shortest_path_length(T, NewMin, Min).


get_paths_list(ST, ET, MaxLength, Cumu, PathList) :-
  find_path_plus(ST, ET, MaxLength, [ET], Path),
  length(Path, N),
  N =< MaxLength, 
  \+ memberchk(Path, Cumu),
  get_paths_list(ST, ET, N, [Path|Cumu], PathList).


get_paths_list(_, _, _, PathList, PathList).


find_path_plus(ST, ST, _, Path, Path):-
  !.

find_path_plus(ST, CurrentT, MaxLength, Cumu, Path) :-
  correct_move(CurrentT, ST),
  length(Cumu, N),
  N < MaxLength,
  Path = [ST|Cumu],
  !.

find_path_plus(ST, CurrentT, MaxLength, Cumu, Path) :-
  length(Cumu, N),
  N < MaxLength,
  correct_move(CurrentT, D),
  \+ memberchk(D, Cumu),
  find_path_plus(ST, D, MaxLength, [D|Cumu], Path).
