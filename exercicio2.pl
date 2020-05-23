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