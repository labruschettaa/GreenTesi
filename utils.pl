cleanUp() :-
    retractall(of(_,_)), retractall(maxOF(_)), retractall(minOF(_)),
    retractall(mf(_,_)), retractall(maxMF(_)), retractall(minMF(_)),
    retractall(maxResources(_,_,_,_)), retractall(minResources(_,_,_,_)).

safeCOp(F, E, MaxE, MinE, R) :-
    O is F*(E - MinE), D is MaxE - MinE,
    safeDiv(O, D, R).

safeROp(F, E, MaxE, MinE, R) :-
    O is F*(MaxE - E), D is MaxE - MinE, safeDiv(O, D, R).

safeDiv(_, 0, 0).
safeDiv(O, D, R) :- dif(D,0), R is O / D.

