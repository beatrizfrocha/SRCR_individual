:- include('arestas.pl').
:- include('pontos.pl').

g(grafo(Pontos,Arestas)) :- gids(Pontos),arestas(Arestas).

g_op(grafo(Pontos,Arestas)) :- operadoras(Pontos),arestas(Arestas).

g_publicidade(grafo(Pontos,Arestas)) :- publicidade(Pontos),arestas(Arestas).

g_abrigo(grafo(Pontos,Arestas)) :- abrigos(Pontos),arestas(Arestas).

g_carreiras(grafo(Pontos,Arestas)) :- carreiras(Pontos),arestas(Arestas).

g_distancias(grafo(Pontos,Arestas)) :- coordenadas(Pontos),arestas(Arestas).
