:-use_module(library(random)).
:-use_module(library(clpfd)).
:-use_module(library(lists)).

caminho(Pecas,N,L,C):-
	generatePuzzle(T, L, C),
	length(Pecas,N),
	findall(X-Y,verificaPeca(T, X,Y, 1),Sols),
	escolhePecas(Pecas, Sols),
	diferentes(Pecas), !.
	
caminho(Pecas, N,L,C):-caminho(Pecas,N,L,C).	

diferentes([]).
diferentes([H|T]):-
	write(H-T),
	\+ member(H, T),
	diferentes(T).
		
choose([], []).
choose(List, Elt) :-
	length(List, Length),
	random(0, Length, Index),
	nth0(Index, List, Elt).

escolhePecas([],_).
escolhePecas([H|T], PecasLivres):-
	choose(PecasLivres, X),
	member(X, [H]),
	escolhePecas(T, PecasLivres).
	
verificaPeca(T,X,Y,Jogador) :- verificaPecaAux(T,X,Y,Jogador,1).
verificaPecaAux([T|_],X,Y,Jogador,Y) :-
	verificaPecaLinha(T,X,Jogador, 1).
verificaPecaAux([_|R],X,Y,Jogador,Linha) :-
	Linha2 is Linha+1,
	verificaPecaAux(R,X,Y,Jogador,Linha2).

verificaPecaLinha([Jogador|_], X, Jogador, X).
verificaPecaLinha([_|R], X, Jogador, Coluna) :-
	N1 is Coluna+1,
	verificaPecaLinha(R, X, Jogador, N1).
	
limite([], 0).
limite([0|T], M):-
	M > 0,
	M1 is M-1,
	limite(T, M1).

generatePuzzle([H|T], N, M):-
	limite(H, M),
	generatePuzzle2(T, N, M, N).

linhaBranca([0|T], M):-
	M1 is M-1,
	linhaBranca2(T, M1).

linhaBranca2([0], 1).
linhaBranca2([1|T], M):-
	M > 1,
	M1 is M-1,
	linhaBranca2(T, M1).
	
generateExit([1], 1).
generateExit([1|T], M):-
	M > 1,
	Div is M mod 2,
	Div = 0,
	M1 is M-1,
	generateExit(T, M1).
	
generateExit([0|T], M):-
	M > 1,
	Div is M mod 2,
	Div = 1,
	M1 is M-1,
	generateExit(T, M1).

generateEntrance([], 0).
generateEntrance([1|T], M):-
	M > 0,
	Div is M mod 2,
	Div = 0,
	M1 is M-1,
	generateEntrance(T, M1).
	
generateEntrance([0|T], M):-
	M > 0,
	Div is M mod 2,
	Div = 1,
	M1 is M-1,
generateEntrance(T, M1).	
	
generatePuzzle2([T], 1, M, _):-limite(T, M).
generatePuzzle2([[1,1|H]|T], N, M, Norig):-
	N =3,
	M2 is M-2,
	generateEntrance(H, M2),
	N1 is N-1,
	generatePuzzle2(T, N1, M, Norig).
	
generatePuzzle2([H|T], N, M, Norig):-
	N =:= Norig-1,
	generateExit(H, M),
	N1 is N-1,
	generatePuzzle2(T, N1, M, Norig).	
	
generatePuzzle2([H|T], N, M, Norig):-	
	N > 1, N \== 3,N =\= Norig-1,
	Div is N mod 2,
	Div = 0,
	linhaBranca(H, M),
	N1 is N-1,
	generatePuzzle2(T, N1, M, Norig).

generatePuzzle2([H|T], N, M, Norig):-	
	N > 1, N \== 3,N =\= Norig-1,
	Div is N mod 2,
	Div = 1,
	generateLine(H, M),
	N1 is N-1,
	generatePuzzle2(T, N1, M, Norig).	

generateLine([], 0).
generateLine([1|T], M):-
	M > 0,
	Div is M mod 2,
	Div = 0,
	M1 is M-1,
	generateLine(T, M1).
	
generateLine([0|T], M):-
	M > 0,
	Div is M mod 2,
	Div = 1,
	M1 is M-1,
	generateLine(T, M1).	

concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :-
	concatenate(L1, L2, L3).	

tabToList(T,Lista):-
	tabToList2(T, Lista, _).
tabToList2([], F, [F|_]).
tabToList2([H|T], Lista, [H3|T3]):-
	concatenate(H3, H, H4),
	tabToList2(T, Lista, [H4|T3]).
	