/*  Israel Felhandler */
/*  COP4020 - Programming Languages */
/*  Spring 2017 */
/*  Project 4: ProLog class roster */

menu(X) :- 
   write('\tClass roster management system'), nl,
   write('\t=============================='), nl,
   write('\t   MENU'), nl,
   write('\t=============================='), nl,
   write('\t0. Reset roster'), nl,
   write('\t1. Load roster from file'), nl,
   write('\t2. Store roster to file'), nl, 
   write('\t3. Display roster sorted by ID'), nl, 
   write('\t4. Add student to roster'), nl, 
   write('\t5. Remove student from roster'), nl, 
   write('\t6. Exit'), nl, 
   write('\tEnter your choice (followed by a \'.\'): '),
   read(Sel),
   process(Sel, X).

process(0,_) :-
    nl,
    write('\tRoster is now empty.'),
    nl, nl, menu([]).

process(1, _) :- 
    nl,
    write('\tEnter file name: '),
    read(Filename),           /* Read Data */
    see(Filename),            /* Open file */
    read(Newlist),            /* Read date */
    seen,                     /* Close file */
    nl, nl, menu(Newlist).

process(2, X) :-
    nl,
    write('\tEnter file name to store Roster to: '),
    read(Filename),
    tell(Filename),           /* Open file */
    writeq(X), write(.),
    told,                     /* Close file */
    nl, nl, menu(X).

process(3,X) :- 
    nl,
    naive_sort(X, Slist),
    show_records(Slist),
    nl, nl, menu(X).

process(4,X) :-
    nl,
    write('\tAdd a student to the Roster'), nl,
    readInfo([A,B,C]),
    nl, nl, menu([[A,B,C] | X]).

process(5, X) :- 
    nl,
    write('\tRemove a student from the Roster:'), nl,
    write('\tEnter student name or ID : '),
    read(Val),
    remove(Val, X, Removedlist),
    nl, menu(Removedlist).

process(6, _) :- write('Good-bye'), nl, !.

process(_, X) :- menu(X).

readInfo([A, B, C]) :-
  nl,
  write('\tStudent ID: '),
  read(A), nl,
  write('\tStudent Name: '),
  read(B), nl,
  write('\tStudent Grade: '),
  read(C).

show_records(Roster) :-
   Roster = [ID | Name],
   write('\tID = '),
   ID = [Grade | D],
   write(Grade),
   write('\tName = '),
   D = [E | F],
   format("~a", [E]),
   write('\tGrade = '),
   F = [G | _],
   write(G),
   nl,
  show_records(Name).

display_record([A,B,C], Num):-
  write('\tNo.'),
  write(Num),
  write(': Student ID ='),
  write(A),
  write(', Student Name ='),
  format("~s", [B]),
  write(', Student Grade ='),
  write(C), nl.

sorted([]).
sorted([_]).
sorted([[X | _], [Y | YL] | L]) :-
  X < Y, sorted([[Y | YL] | L]).

naive_sort(Oldroster, Sortedroster):- 
  perm(Oldroster, Sortedroster),        /* Sortedroster is permutation of original Roster */
  sorted(Sortedroster).                 /* Sortedroster is sorted */

display_roster([FirstStudent | Restoflist], N):-
  display_record(FirstStudent, N),              /* Display single student */
  K is N+1,
  display_roster(Restoflist, K), nl, nl.        /* Recursively call for each students */

takeout(Item, [Item | L], L).       /* Remove single item */
takeout(Item, [X | L], [X | L1]) :- takeout(Item, L, L1).

perm([], []).
perm([X | Y], Z) :- perm(Y, W), takeout(X, Z, W).

remove(_, [], []).
remove(Item, [[Item,_,_] | L], L).       /* ID to remove found at the head */
remove(Item, [[_,Item,_] | L], L).       /* Name to remove found */
remove(Item, [X | L] , [X | L2]):-       /* Item not in Record */
  remove(Item, L, L2).