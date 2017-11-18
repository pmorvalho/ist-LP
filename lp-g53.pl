/*                                      Logica para a Programacao
                                                Grupo 53
                                           80980 David Calhas
                                        81151 Pedro Miguel Orvalho


Breve descricao:
Na primeira parte deste projecto, pode encontrar as funcoes de output do programa,
seguido das funcoes utilizadas nas 3 resolucoes possiveis: manual, cega e informada, nomeadamente
o mov_legal/4.
A resolucao manual e a terceira parte do codigo, seguida da resolucao cega, e da informada.
No nosso projecto, decidimos fazer a resolve informada que utiliza a distancia de Hamming.
Por ultimo pode encontrar no final do codigo, o predicado da transformacao_possivel (credito adicional).


Estrutura dos predicados:
-comentario com a sua descricao e breve descricao deste,
-linha em branco,
-predicados relativos,
-duas linhas em branco.
(regras maiores: predicados separados por um linha em branco)


Partes do codigo separadas por 3 linhas em branco*/



/* ******************************************Funcoes de output********************************** */


/*Escreve a transformacao desejada*/

transformacao([A, B, C, D, E, F, G, H, I],[J, K, L, M, N, O, P, Q, R]) :-
                          write('Transformacao desejada:'), nl,
                          escreve(A), escreve(B), escreve(C),
                          write('    '),
                          escreve(J), escreve(K), escreve(L),nl,
                          escreve(D), escreve(E), escreve(F),
                          write(' -> '),
                          escreve(M), escreve(N), escreve(O), nl,
                          escreve(G), escreve(H), escreve(I),
                          write('    '),
                          escreve(P), escreve(Q), escreve(R), nl.


/* escreve/1 -  escreve um elemento, que corresponde a um elemento da lista */

escreve(S) :- S = 0, write('   ').
escreve(S) :- S < 10, write(' '), write(S), write(' ').


/*escreve_solucao/1 - escreve no ecra os movimentos feitos*/

escreve_solucao([[M, P] | []]) :- write('mova a peca '),
                                  write(P),
                                  traduz(M, Mp),
                                  write(Mp),
                                  write('.'),
                                  nl.

escreve_solucao([[M, P] | R]) :- write('mova a peca '),
                                 write(P),
                                 traduz(M, Mp),
                                 write(Mp),
                                 nl,
                                 escreve_solucao(R).


/* traduz/2 e um predicado auxiliar de escreve_solucao/1 */

traduz(c, ' para cima').
traduz(b, ' para baixo').
traduz(e, ' para a esquerda').
traduz(d, ' para a direita').


/* escreve_linha/1 - escreve uma linha, o que equivale a 3 elementos da lista */

escreve_linha([X, Y, Z]):-escreve(X),
                          escreve(Y),
                          escreve(Z), nl.


/* escreve_lista/1 - imprime a lista no ecra  */

escreve_lista([]).
escreve_lista([X, Y, Z|R]) :-escreve_linha([X,Y,Z]),
                             escreve_lista(R).



/* ****************************Funcoes utilizadas em todas as resolucoes***********************  */


/* mov_legal/4 - verifica se o movimento pedido pode-se ou nao efectuar*/

mov_legal(C1,c,P,C2):-posicao(C1,X), X=<6,
                      mov(C1,C2,P,3).
mov_legal(C1,b,P,C2):-posicao(C1,X), X>3,
                      mov(C1,C2,P,-3).
mov_legal(C1,e,P,C2):-posicao(C1,X), X=\=3,
                      X=\=6,X=\=9,
                      mov(C1,C2,P,1).
mov_legal(C1,d,P,C2):-posicao(C1,X), X=\=1,
                      X=\=4,X=\=7,
                      mov(C1,C2,P,-1).


/* mov/4 - efectua a alteracao pedida, de acordo com o movimento pedido  */

mov(L,K,P,Q):-posicao(L,R),
              E is R + Q,
              elemento(L,P,E),
              troca_elementos(0,9,L,L2),
              troca_elementos(P,0,L2,L3),
              troca_elementos(9,P,L3,K).


/* diferente/2 verifica se duas litas sao diferentes. */

diferente(X,Y):- X=Y,!,fail;true.


/* posicao/1 - verifica a posicao do 0 na lista. */

posicao(L,P):-posicao(L,1,P).
posicao([0|_],Ac,Ac).
posicao([Y|R],Ac,P):- Y=\=0,
                      Acc is  Ac + 1 ,
                      posicao(R,Acc,P).


/* elemento/3 - da o valor(E) na posicao P, a primeira posicao.  */

elemento(L,V,P):- elemento(L,V,P,1).
elemento([X|_],X,P,P).
elemento([_|R],V,P,Ac):- Ac_sub is Ac +1, elemento(R,V,P,Ac_sub),!.


/* troca_elementos(X,Y,L,L2)/4 - X e o elemento de L que vai ser trocado por Y, gerando a nova lista L2  */

troca_elementos(_,_,[],[]).
troca_elementos(X,Y,[X|R],[Y|R1]):-troca_elementos(X,Y,R,R1).
troca_elementos(X,Y,[Z|R],[Z|R1]):-X=\=Z,troca_elementos(X,Y,R,R1),!.


/* inverte/2 inverte uma lista */

inverte(L,S) :- inverte(L,[],S).
inverte([],L,L).
inverte([X|R],T,L) :- inverte(R,[X|T],L).



/* *****************************************Solucao Manual*************************************** */


/*resolve_manual/2 - funcao que digita a transformacao desejada, e da inicio ao ciclo de possiveis
jogadas do jogador, no final dando os Parabens a este*/

resolve_manual(L,K):-transformacao(L,K),!,
                     movimento(L,K),!,
                     writeln('Parabens!').


/* movimento/2 - ciclo que recebe instrucoes do utilizador, fazendo as alteracoes pedidas a
 lista depois de verificar se o movimento e legal. */

movimento(L,K):- \+ diferente(L,K),!.
movimento(L,K):-diferente(L,K),!,
                write('Qual o seu movimento?'),nl,
                read(X),movimento(L,K,X).

movimento(L,K,X):-mov_legal(L,X,_,C2),!,nl,
                  escreve_lista(C2),nl,
                  diferente(L,K),
                  movimento(C2,K).

movimento(L,K,X):- \+ mov_legal(L,X,_,_),!,
                   diferente(L,K),
                   write('Movimento ilegal'),nl,
                   movimento(L,K).



/* ***************************************Procura Cega******************************************** */


/* resolve_cego/2 - ciclo que verifica uma possivel resposta para a transformacao */

resolve_cego(L,R):-transformacao(L,R),!,movs_possiveis(L,R,_,[L],[]),!.


/* movs_possiveis/2 - ciclo do resolve cego, que resolve o puzzle,
 digitando no ecra no final as jogadas efectuadas. Vai guardando os movimentos(M), numa lista
 de movimentos(Movs) que imprime no final. Guarda as configuracoes das listas ja geradas numa
 lista (Lgeradas) para nao entrar em ciclo. */

movs_possiveis(L,L,_,_,Movs):- !,inverte(Movs,T), escreve_solucao(T).
movs_possiveis(L,R,M,Lgeradas,Movs):- mov_legal(L,M,P,Trans),
                                      \+ membro(Trans,Lgeradas),
                                      insere(Trans,Lgeradas,G),
                                      insere([M,P],Movs,T),
                                      movs_possiveis(Trans,R,_,G,T).


/* membro/2 - verifica se o elemento X e membro duma lista */

membro(X,[X|_]).
membro(X,[_|Y]):-membro(X,Y).


/* insere/3 -  insere um elemento numa lista */

insere(X,L,[X|L]).



/* *************************************Procura Informada******************************************* */


/*resolve_info_h/2 - comeca o ciclo da resolucao informada, imprimindo a transformacao desejada inicial*/

resolve_info_h(L,K):-transformacao(L,K),!,resolve_informada(L,K),!.


/* resolve_informada/2 - ciclo da resolucao informada, que resolve o puzzle,
 vai guardando as possiveis jogadas em cada node
 mal um node, tenha um H=0, isto e, tenha uma configuracao igual a configuracao desejada, o ciclo termina
 e imprime no final as jogadas efectuadas.*/

resolve_informada(L,K):- calcula_hamming(L,H,K),!,F is H,
                         resolve_informada([node(L,F,0,H,[])],[],K).

resolve_informada([node(_,_,_,0,M)|_],_,_):- !,inverte(M,T),escreve_solucao(T).

resolve_informada(Aberta,Fe,K):- !, pesquisa_menor_node(Aberta,node(L,F,G,H,M)),
                                 expande_node(node(L,F,G,H,M),Aberta,Abert,Fe,K),
                                 remove_node(node(L,F,G,H,M),Abert,Aber),
                                 insere_node(node(L,F,G,H,M),Fe,Fee),
                                 resolve_informada(Aber,Fee,K).


/*expande_node/5 recebe um node, e ve os possiveis filhos desse node, e chama o cria_node para
tratar deles*/

expande_node(node(L,_,G,_,M),Aberta,Abe,Fechada,K):- !,findall([Mo,Pe,C2],mov_legal(L,Mo,Pe,C2),Resultado),
                                                     inverte(Resultado,Resultado2),
                                                     Ga is G + 1,
                                                     cria_node(Resultado2,Ga,M,Aberta,Fechada,Abe,K).


/* cria_node/7 cria um node e insere-o numa lista de nodes*/

cria_node([],_,_,Abtt,_,Abtt,_):-!.
cria_node([[Mo,Pe,C2]|R],Ga,M,A,Fechada,Abtt,K):-  \+ membro_lista_nodes(C2,A),
                                                  \+ membro_lista_nodes(C2,Fechada),!,
                                                  calcula_hamming(C2,Ha,K),
                                                  insere([Mo,Pe],M,Maa),
                                                  F is Ga + Ha,
                                                  insere_node(node(C2,F,Ga,Ha,Maa),A, Abt),
                                                  cria_node(R,Ga,M,Abt,Fechada,Abtt,K).

cria_node([[_,_,_]|R],Ga,M,A,Fechada,Abtt,K):- !,cria_node(R,Ga,M,A,Fechada,Abtt,K).


/* insere_node/3 receve um node e insere-o na lista de node pelo inicio. */

insere_node(node(C,F,G,H,M),Aberta, Ab):-insere_node(node(C,F,G,H,M),Aberta,[], Ab).
insere_node(0,_,Ab,Ab):-!.
insere_node(node(C,F,G,H,M),Aberta,[], Ab):-insere_node(0,Aberta,[node(C,F,G,H,M)|Aberta],Ab).


/*pesquisa_menor_node/2 pesquisa_menor_node na lista de nodes o node com menor valor de F, sendo
o F = G + H*/

pesquisa_menor_node([node(L2,F2,G2,H2,M2)|R],node(L,F,G,H,M)):-
                                  pesquisa_menor_node(node(L2,F2,G2,H2,M2),R,node(L,F,G,H,M)).

pesquisa_menor_node(node(L,F,G,H,M),[],node(L,F,G,H,M)):-!.

pesquisa_menor_node(node(_,F2,_,_,_),[node(L3,F3,G3,H3,M3)|R],node(L,F,G,H,M)):-
                                  F3<F2,!,pesquisa_menor_node(node(L3,F3,G3,H3,M3),R,node(L,F,G,H,M)).

pesquisa_menor_node(node(L2,F2,G2,H2,M2),[node(_,F3,_,_,_)|R],node(L,F,G,H,M)):-
                                  F3>=F2,!,pesquisa_menor_node(node(L2,F2,G2,H2,M2),R,node(L,F,G,H,M)).


/*remove_node/2 - remove um node de uma lista de nodes*/

remove_node(node(L,F,G,H,M),[node(L,F,G,H,M)|R],R):-!.
remove_node(node(L,F,G,H,M),[node(L2,F2,G2,H2,M2)|R],[node(L2,F2,G2,H2,M2)|R1]):-!,remove_node(node(L,F,G,H,M),R,R1).


/* calcula a distancia de hamming entre duas listas */

calcula_hamming(L,H,K):-!,calcula_hamming(L,H,K,0).
calcula_hamming([],H,[],H):-!.
calcula_hamming([L|R],H,[K|T],Ac):- L=\=K,L=\=0,
                                    !, Acc is Ac + 1,
                                    calcula_hamming(R,H,T,Acc).
calcula_hamming([_|R],H,[_|T],Ac):- !,calcula_hamming(R,H,T,Ac).


/*verifica se a lista L, faz parte ou nao de um node da lista de nodes */

membro_lista_nodes(_,[]):-!,fail.
membro_lista_nodes(L,[node(L,_,_,_,_)|_]):- !,true.
membro_lista_nodes(L,[node(_,_,_,_,_)|R]):- !,membro_lista_nodes(L,R).



/* *******************************Transformacao  Possivel*****************************************
                                   Credito Adicional


transformacao_possivel/2 - verifica se a transformacao pedida e possivel de efectuar
ou nao, calculando o numero de calcula_inversoes das duas listas, caso o numero seja par, ou
caso seja impar nas duas listas, a transformacao e possivel. Caso contrario e impossivel.*/


transformacao_possivel(L,L2):-!,calcula_inversoes(L,I),
                              calcula_inversoes(L2,I2),
                              R is I mod 2,
                              V is I2 mod 2,
                              R=:=V.


/* calcula_inversoes/2 - calcula o numero de calcula_inversoes numa lista */

calcula_inversoes(L,I):-calcula_inversoes(L,0,I).
calcula_inversoes([],I,I):-!.
calcula_inversoes([L|R],S,I):-!,calcula_menores(L,R,S2),
                              Ss is S + S2,
                              calcula_inversoes(R,Ss,I).


/* calcula_menores/3 - calcula o numero de numeros inferiores ao numero dado que estao
a sua frente numa lista */

calcula_menores(L,R,S):-calcula_menores(L,R,0,S).
calcula_menores(_,[],S,S):-!.
calcula_menores(X,[Y|R],Ss,S):-X>Y,
                               Y=\=0,!,
                               Sss is Ss + 1,
                               calcula_menores(X,R,Sss,S).
calcula_menores(X,[_|R],Ss,S):-!,calcula_menores(X,R,Ss,S).
