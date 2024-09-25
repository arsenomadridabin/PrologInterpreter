use "Main.sml";

Prolog(parse "init.");

(* Assert facts *)

Prolog(parse "father(indra,madhur).");
Prolog(parse "father(madhur,abin).");
Prolog(parse "father(ashok,prateek).");
Prolog(parse "mother(muna,abin).");
Prolog(parse "brother(madhur,ashok).");
Prolog(parse "brother(ashok,madhur).");


(* Assert rules *)
Prolog(parse "parent(X,Y) :- father(X,Y).");
Prolog(parse "uncle(X,Y) :- brother(X,Z),father(Z,Y).");
Prolog(parse "couple(X,Y) :-  father(X,Z), mother(Y,Z).");
Prolog(parse "grandfather(X,Z) :- father(X,Y),father(Y,Z).");


(* Queries *)


(* Should return No *)
Prolog(parse "random?");

(* Should return True *)
Prolog(parse "parent(madhur,abin)?");

(* Should return X=abin *)
Prolog(parse "parent(madhur,X)?");

(* Should return X=prateek *)
Prolog(parse "uncle(madhur,X)?");

(* Should return X=indra *)
Prolog(parse "grandfather(X,abin)?");

(* Should return X=muna *)
Prolog(parse "couple(madhur,X)?");

(* Should return No *)
Prolog(parse "parent(abin,madhur)?");



