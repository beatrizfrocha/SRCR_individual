:- include('exercicio2.pl').

teste_ponto1(G, P1, P2, R) :- write('Teste ponto 1: '),
    caminho(G, P1, P2, P), P == R,
    write('Sucesso \n'); write('Insucesso \n').

teste_ponto2(G, P1, P2, O, R) :- write('Teste ponto 2: '),
    trajeto_com_op(G, P1, P2, O, P), P == R,
    write('Sucesso \n'); write('Insucesso \n').

teste_ponto3(G, P1, P2, O, R) :- write('Teste ponto 3: '),
    trajeto_sem_op(G, P1, P2, O, P), P == R,
    write('Sucesso \n'); write('Insucesso \n').

teste_ponto4(G, P1, P2, R) :- write('Teste ponto 4: '),
    max_carreiras(G, P1, P2, P), P == R,
    write('Sucesso \n'); write('Insucesso \n').

teste_ponto7(G, P1, P2, R) :- write('Teste ponto 7: '),
    trajeto_com_publicidade(G, P1, P2, P), P == R,
    write('Sucesso \n'); write('Insucesso \n').

teste_ponto8(G, P1, P2, R) :- write('Teste ponto 8: '),
    trajeto_com_abrigo(G, P1, P2, P), P == R,
    write('Sucesso \n'); write('Insucesso \n').

teste_ponto9(G, P1, L, P2, R) :- write('Teste ponto 9: '),
    caminho_com_pontos_intermedios(G, P1, L, P2, P), P == R,
    write('Sucesso \n'); write('Insucesso \n').

run(1) :- 
   g(G),
   teste_ponto1(G,'183','181',['183','791','595','182','499','593','181']).

run(2) :- 
   g_op(G),
   teste_ponto2(G,'262','513',['SCoTTURB'],['262','263','507','509','508','924','513']).

run(3) :- 
   g_op(G),
   teste_ponto3(G,'262','513',['Vimeca'],['262','263','507','509','508','924','513']).

run(4) :- 
   g_carreiras(G),
   teste_ponto4(G,'183','595',['595']).

run(7) :- 
   g_publicidade(G),
   teste_ponto7(G,'237','597',['237','107','250','261','597']).

run(8) :- 
   g_abrigo(G),
   teste_ponto8(G,'595','250',['595','182','181','180','594','185','89','107','250']).

run(9) :- 
   g(G),
   teste_ponto9(G,'791',['181'],'182',['791','595','182','499','593','181','180','594','185','89','107','250','261','597','953','609','799','171','172','162','161','156','734','159','155','741','742','686','687','87','154','709','1014','68','788','170','183','791','595','182']).
