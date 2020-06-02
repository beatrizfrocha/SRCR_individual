%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

% Sistema de Representação de Conhecimento e Raciocínio com capacidade 
% de recomendação de transporte público para o caso de estudo

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Carregar predicados do ficheiro no qual é guardado o estado

:- include('base_conhecimento_paragens.pl').
:- include('base_conhecimento_carreiras.pl').
:- include('grafo.pl').

:- use_module(library(lists)).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calcular um trajeto entre dois pontos

adjacente(X,Y,grafo(_,Es)) :- member(aresta(X,Y),Es).
adjacente(X,Y,grafo(_,Es)) :- member(aresta(Y,X),Es).

caminho(G,A,B,P) :- caminho1(G,A,[B],P).

caminho1(_,A,[A|P1],[A|P1]).
caminho1(G,A,[Y|P1],P) :- 
   adjacente(X,Y,G), \+ memberchk(X,[Y|P1]), caminho1(G,A,[X,Y|P1],P).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Selecionar apenas algumas das operadoras de transporte para um determinado percurso

differ([], _, []).
differ([(A,X)|Tail], L2, Result):- member(X, L2), !, differ(Tail, L2, Result). 
differ([(A,X)|Tail], L2, Result):- differ(Tail, L2, Result1), Result = [A|Result1].

inclui_op(G,[],[]).
inclui_op(grafo(A,B),L,R) :- differ(A,L,R1), remove_aresta(B,R1,R2), R = grafo(A,R2). 

trajeto_com_op(G,A,B,O,P) :- inclui_op(G,O,R), caminho(R,A,B,P).   

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Excluir um ou mais operadores de transporte para o percurso

equals([], _, []).
equals([(A,X)|Tail], L2, Result):- member(X, L2), !, equals(Tail, L2, Result1), Result = [A|Result1]. 
equals([(A,X)|Tail], L2, Result):- equals(Tail, L2, Result).

remove_aresta([], _, []).
remove_aresta([aresta(A,B)|Tail], L2, Result):- member(A, L2), !, remove_aresta(Tail, L2, Result);
                                                member(B, L2), !, remove_aresta(Tail, L2, Result).
remove_aresta([aresta(A,B)|Tail], L2, [aresta(A,B)|Result]):- remove_aresta(Tail, L2, Result).

exclui_op(G,[],G).
exclui_op(grafo(A,B),L,R) :- equals(A,L,R1), remove_aresta(B,R1,R2), R = grafo(A,R2). 

trajeto_sem_op(G,A,B,O,P) :- exclui_op(G,O,R), caminho(R,A,B,P).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Escolher o percurso que passe apenas por abrigos com publicidade

trajeto_com_publicidade(G,A,B,P) :- exclui_op(G,['No'],R), caminho(R,A,B,P).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Escolher o percurso que passe apenas por paragens abrigadas

trajeto_com_abrigo(G,A,B,P) :- exclui_op(G,['Sem Abrigo'],R), caminho(R,A,B,P).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar quais as paragens com o maior número de carreiras num determinado percurso

max_carreiras(G,A,B,L) :- caminho(G,A,B,P), obtem_comprimentos_listas_carreiras(G,P,R1), lista_maiores(R1,R2), lista_primeiro_elem(R2,L).

lista_primeiro_elem([],[]).
lista_primeiro_elem([(X,Y)|T],R) :- lista_primeiro_elem(T,R1), R = [X|R1].

lista_maiores([],[]).
lista_maiores([(X,Y) | T], R) :- 
   lista_maiores(T, M), segundo_da_cabeca(M, P), Y > P, R = [(X, Y)].
lista_maiores([(X,Y) | T], R) :- 
   lista_maiores(T, M), segundo_da_cabeca(M, P), Y == P, append(M, [(X, Y)], R).
lista_maiores([(X,Y) | T], R) :- 
   lista_maiores(T, M), segundo_da_cabeca(M, P), Y < P, R = M.

segundo_elemento((X,Y),Y).

segundo_da_cabeca(L, R) :- head(L, P), segundo_elemento(P, R).

obtem_comprimentos_listas_carreiras(G, [], []).
obtem_comprimentos_listas_carreiras(G, [H|T], L) :- comprimento_lista_carreiras(G,H,A), obtem_comprimentos_listas_carreiras(G,T,B), append([A],B,L).

comprimento_lista_carreiras(G,P,R) :- devolve_lista_carreiras(G,P,(X,R1)), length(R1,Res), R = (P,Res).

devolve_lista_carreiras(grafo([(X,Y)|T],B),P,R) :- P == X, R = (P,Y);
                                                   devolve_lista_carreiras(grafo(T,B),P,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Escolher o menor percurso (usando critério menor número deparagens)

caminho_com_menor_nr_paragens(G,I,F,R) :- adjacentes(I,G,L1), caminhos(I,F,G,L1,L2), lista_menores(L2,[(A,B)|T]), append([I],A,R);
                                          caminho(G,I,F,R).

lista_menores([],[]).
lista_menores([X],[X]).
lista_menores([(X,Y) | T], R) :- 
   lista_menores(T, M), segundo_da_cabeca(M, P), Y < P, R = [(X, Y)].
lista_menores([(X,Y) | T], R) :- 
   lista_menores(T, M), segundo_da_cabeca(M, P), Y == P, append(M, [(X, Y)], R).
lista_menores([(X,Y) | T], R) :- 
   lista_menores(T, M), segundo_da_cabeca(M, P), Y > P, R = M.

caminhos(I,F,G,[],[]).
caminhos(I,F,G,[H|T],R) :- caminho(G,H,F,L1), caminhos(I,F,G,T,L2), length(L1,R1), append([(L1,R1)],L2,R).

adjacentes(P,G,R) :- findall(X,adjacente(X,P,G),R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Escolher um ou mais pontos intermédios por onde o percurso deverá passar.

caminho_com_pontos_intermedios(G, PI, [], PF, Caminho)
    :- caminho(G, PI, PF, Caminho).

caminho_com_pontos_intermedios(G, PI, [P | Ps], PF, Caminho) :-
    caminho(G, PI, P, Caminho1),
    caminho_com_pontos_intermedios(G, P, Ps, PF, Caminho_Restante),
    tail(Caminho_Restante, T),
    append(Caminho1, T, Caminho).

