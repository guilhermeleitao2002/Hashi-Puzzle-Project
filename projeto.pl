% Guilherme Leitao, ist199951

% ------------------------------------------------------------------------------------------------------------------
% extrai_ilhas_linha(N_L, Linha, Ilhas), em que N_L eh um inteiro positivo,
% correspondente ao numero de uma linha e Linha eh uma lista correspondente a uma linha
% de um puzzle, significa que Ilhas eh a lista ordenada (ilhas da esquerda para a direita)
% cujos elementos sao as ilhas da linha Linha
% ------------------------------------------------------------------------------------------------------------------
extrai_ilhas_linha(N_L, L1, L2) :-
    extrai_ilhas_linha(N_L, L1, L2, 1).

extrai_ilhas_linha(_, [], [], _).

extrai_ilhas_linha(N_L, [H|T1], [ilha(H, (N_L, Count))|T2], Count) :-
    H \== 0, % se for uma ilha
    !,
    NewCount is Count + 1, % incrementar variavel de contagem do eixo das abscissas
    extrai_ilhas_linha(N_L, T1, T2, NewCount).

extrai_ilhas_linha(N_L, [_|T1], R, Count) :-
    NewCount is Count + 1, % incrementar variavel de contagem do eixo das abscissas
    extrai_ilhas_linha(N_L, T1, R, NewCount).


% ------------------------------------------------------------------------------------------------------------------
% ilhas(Puz, Ilhas), em que Puz eh um puzzle, significa que Ilhas eh a lista ordenada
% (ilhas da esquerda para a direita e de cima para baixo) cujos elementos sao as ilhas de Puz
% ------------------------------------------------------------------------------------------------------------------
ilhas(Puz, Ilhas) :-
    ilhas(Puz, Ilhas_Nested, 1),
    flatten(Ilhas_Nested, Ilhas). % fazer o alisamento das ilhas

ilhas([], [], _).

ilhas([L1|Res1], [L2|Res2], Count) :-
    extrai_ilhas_linha(Count, L1, L2),
    NewCount is Count + 1, % incrementar variavel de contagem do eixo das ordenadas
    ilhas(Res1, Res2, NewCount).


% ------------------------------------------------------------------------------------------------------------------
% vizinhas(Ilhas, Ilha, Vizinhas), em que Ilhas eh a lista de ilhas de um puzzle
% e Ilha eh uma dessas ilhas, significa que Vizinhas eh a lista ordenada (ilhas de cima para
% baixo e da esquerda para a direita ) cujos elementos sao as ilhas vizinhas de Ilha
% ------------------------------------------------------------------------------------------------------------------
vizinhas(Ilhas, ilha(_, (Y, X)), Vizinhas) :-
    % todas as ilhas na mesma linha ou coluna
    findall(ilha(N1, (Y1, X1)), (member(ilha(N1, (Y1, X1)), Ilhas), ((Y1 == Y, X1 \== X); (Y1 \== Y, X1 == X))), Vizinhas_Aux),
    findall(ilha(N1, (Y1, X1)), (member(ilha(N1, (Y1, X1)), Vizinhas_Aux), (X1 == X, Y1 < Y)), Up_Aux), % apenas as ilhas em cima
    ultimo(Up_Aux, Up), % apenas o imediatamente em cima
    findall(ilha(N1, (Y1, X1)), (member(ilha(N1, (Y1, X1)), Vizinhas_Aux), (Y1 == Y, X1 < X)), Left_Aux), % apenas as ilhas a esquerda
    ultimo(Left_Aux, Left), % apenas o imediatamente a esquerda
    findall(ilha(N1, (Y1, X1)), (member(ilha(N1, (Y1, X1)), Vizinhas_Aux), (Y1 == Y, X1 > X)), Right_Aux), % apenas as ilhas a direita
    primeiro(Right_Aux, Right), % apenas o imediatamente a direita
    findall(ilha(N1, (Y1, X1)), (member(ilha(N1, (Y1, X1)), Vizinhas_Aux), (X1 == X, Y1 > Y)), Down_Aux), % apenas as ilhas em baixo
    primeiro(Down_Aux, Down), % apenas o imediatamente em baixo
    append(Up, Left, Vizinhas2),
    append(Vizinhas2, Right, Vizinhas1),
    append(Vizinhas1, Down, Vizinhas).

% retira o primeiro elemento de uma lista
primeiro(P, P) :- P == [].
primeiro([P|R], [P|R]) :- R == [].
primeiro([P|R], [P]) :- R \== [].

% retira o ultimo elemento de uma lista
ultimo(P, [U]) :- P \== [], !, last(P, U).
ultimo(P, P).


% ------------------------------------------------------------------------------------------------------------------
% estado(Ilhas, Estado), em que Ilhas eh a lista de ilhas de um puzzle, significa que
% Estado eh a lista ordenada cujos elementos sao as entradas referentes a cada uma das
% ilhas de Ilhas
% ------------------------------------------------------------------------------------------------------------------
estado(Ilhas, Estado) :-
    estado_aux(Ilhas, Estado, Ilhas).

estado_aux([], [], _) :- !.

estado_aux([Ilha|R1], [[Ilha, Vizinhas, []]|R2], Ilhas) :-
    vizinhas(Ilhas, Ilha, Vizinhas), % buscar as vizinhas de cada ilha
    estado_aux(R1, R2, Ilhas).


% ------------------------------------------------------------------------------------------------------------------
% posicoes_entre(Pos1, Pos2, Posicoes), em que Pos1 e Pos2 sao posicoes, significa que Posicoes eh a lista
% ordenada de posicoes entre Pos1 e Pos2 (excluindo Pos1 e Pos2). Se Pos1 e Pos2
% nao pertencerem a mesma linha ou a mesma coluna, o resultado eh false.
% ------------------------------------------------------------------------------------------------------------------
posicoes_entre((Y1, X1), (Y2, X2), Posicoes) :-
    (Y1 == Y2; X1 == X2), % tem que pertencer a mesma coluna ou linha
    (((Y1 == Y2) -> ((X1 < X2) -> (NewX1 is X1 + 1, posicoes_y(Y1, Posicoes, NewX1, X2)); (NewX2 is X2 + 1, posicoes_y(Y1, Posicoes, NewX2, X1)))); % se pertence a mesma linha
    ((Y1 < Y2) -> (NewY1 is Y1 + 1, posicoes_x(X1, Posicoes, NewY1, Y2)); (NewY2 is Y2 + 1, posicoes_x(X1, Posicoes, NewY2, Y1)))). % se pertence a mesma coluna

posicoes_y(_, [], X, X) :- !.

posicoes_y(Y, [(Y, X1)|R], X1, X2) :-
    X1 < X2,
    NextX1 is X1 + 1, % incrementar a variavel de contagem
    posicoes_y(Y, R, NextX1, X2).

posicoes_x(_, [], Y, Y) :- !.

posicoes_x(X, [(Y1, X)|R], Y1, Y2) :-
    Y1 < Y2,
    NextY1 is Y1 + 1, % incrementar a variavel de contagem
    posicoes_x(X, R, NextY1, Y2).


% ------------------------------------------------------------------------------------------------------------------
% cria_ponte(Pos1, Pos2, Ponte), em que Pos1 e Pos2 sao 2 posicoes, significa
% que Ponte eh uma ponte entre essas 2 posicoes
% ------------------------------------------------------------------------------------------------------------------
cria_ponte((Y, X1), (Y, X2), Ponte) :-
    cria_ponte_y(Y, X1, X2, Ponte), !. % se pertence a mesma linha
cria_ponte((Y1, X), (Y2, X), Ponte) :-
    cria_ponte_x(X, Y1, Y2, Ponte). % se pertence a mesma coluna

cria_ponte_y(Y, X1, X2, ponte((Y, X1), (Y, X2))) :- X1 < X2.
cria_ponte_y(Y, X1, X2, ponte((Y, X2), (Y, X1))) :- X1 > X2.
cria_ponte_x(X, Y1, Y2, ponte((Y1, X), (Y2, X))) :- Y1 < Y2.
cria_ponte_x(X, Y1, Y2, ponte((Y2, X), (Y1, X))) :- Y1 > Y2.


% ------------------------------------------------------------------------------------------------------------------
% caminho_livre(Pos1, Pos2, Posicoes, I, Vz), em que Pos1 e Pos2 sao posicoes, Posicoes eh a
% lista ordenada de posicoes entre Pos1 e Pos2, I eh uma ilha, e Vz eh uma das suas
% vizinhas, significa que a adicao da ponte ponte(Pos1, Pos2) nao faz com que I e Vz deixem de ser vizinhas
% ------------------------------------------------------------------------------------------------------------------
caminho_livre((Y1, X1), (Y2, X2), Posicoes, ilha(_, (Y3, X3)), ilha(_, (Y4, X4))) :-
    % situacoes em que I e Vz se encontram na mesma linha ou coluna que Pos1 e Pos1
    % mas entre eles:     Pos1 ---- I --- Vz ---- Pos2      , por exemplo
    ((Y1 == Y2, Y2 == Y3, Y3 == Y4, ((X1 < X3, X1 < X4, X2 > X3, X2 > X4); (X2 < X3, X2 < X4, X1 > X3, X1 > X4)));
    (X1 == X2, X2 == X3, X3 == X4, ((Y1 < Y3, Y1 < Y4, Y2 > Y3, Y2 > Y4); (Y2 < Y3, Y2 < Y4, Y1 > Y3, Y1 > Y4))));
    % no caso de se tratarem das mesmas posicoes
    ((Y1 == Y3, X1 == X3, Y2 == Y4, X2 == X4); (Y1 == Y4, X1 == X4, Y2 == Y3, X2 == X3));
    (posicoes_entre((Y3, X3), (Y4, X4), PosicoesIVz), % possivel ponte entre a ilha e sua vizinha
    disjuntas(Posicoes, PosicoesIVz)). % nao podem ter elementos em comum

disjuntas([], _) :- !.

disjuntas([E|R], L) :-
    \+ member(E, L),
    disjuntas(R, L).


% ------------------------------------------------------------------------------------------------------------------
% actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada), em
% que Pos1 e Pos2 sao as posicoes entre as quais ira ser adicionada uma ponte,
% Posicoes eh a lista ordenada de posicoes entre Pos1 e Pos2,
% e Entrada eh uma entrada, significa que Nova_Entrada eh igual a
% Entrada, excepto no que diz respeito a lista de ilhas vizinhas; esta deve ser actualizada,
% removendo as ilhas que deixaram de ser vizinhas, apos a adicao da ponte
% ------------------------------------------------------------------------------------------------------------------
actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, Vizinhas, P], [Ilha, Novas_Vizinhas, P]) :-
    % introduz apenas as ilhas que continuarem a ser vizinhas de Ilha, mesmo apos a insercao da ponte entre Pos1 e Pos2
    findall(I, (member(I, Vizinhas), caminho_livre(Pos1, Pos2, Posicoes, Ilha, I)), Novas_Vizinhas).


% ------------------------------------------------------------------------------------------------------------------
% actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) ,
% em que Estado eh um estado (ver Seccao 2.4), Pos1 e Pos2 sao as posicoes entre as
% quais foi adicionada uma ponte, significa que Novo_estado eh o estado que se obtem de
% Estado apos a actualizacao das ilhas vizinhas de cada uma das suas entradas
% ------------------------------------------------------------------------------------------------------------------
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    % aplica actualiza_vizinhas_entrada a todas as entradas do Estado fornecido
    maplist(actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes), Estado, Novo_estado).


% ------------------------------------------------------------------------------------------------------------------
% ilhas_terminadas(Estado, Ilhas_term), em que Estado eh um estado (ver Seccao 2.4), significa que
% Ilhas_term eh a lista de ilhas que ja tem todas as pontes associadas,
% designadas por ilhas terminadas. Se a entrada referente a uma ilha for [ilha(N_pontes,
% Pos), Vizinhas, Pontes], esta ilha esta terminada se N_pontes for diferente de
% 'X' (a razao para esta condicao ficara aparente mais a frente) e o comprimento da lista
% Pontes for N_pontes
% ------------------------------------------------------------------------------------------------------------------
ilhas_terminadas(Estado, Ilhas_term) :-
    % apenas as ilhas que tiverem assinaladas N pontes e tiverem uma lista de pontes de comprimento N
    findall(ilha(N, Pos), (member([ilha(N, Pos), _, Pontes], Estado), N \== 'X', length(Pontes, N_Pontes), N == N_Pontes), Ilhas_term).


% ------------------------------------------------------------------------------------------------------------------
% tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada),
% em que Ilhas_term eh uma lista de ilhas terminadas e Entrada eh uma entrada (ver
% Seccao 2.4), significa que Nova_entrada eh a entrada resultante de remover as ilhas de
% Ilhas_term, da lista de ilhas vizinhas de entrada
% ------------------------------------------------------------------------------------------------------------------
tira_ilhas_terminadas_entrada(Ilhas_term, [I, V, P], [I, N_V, P]) :-
    % filtrar a lista de vizinhas apenas com aquelas que nao pertencem tambem a lista de ilhas terminadas
    exclude([E] >> member(E, Ilhas_term), V, N_V).


% ------------------------------------------------------------------------------------------------------------------
% tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado), em que
% Estado eh um estado (ver Seccao 2.4) e Ilhas_term eh uma lista de
% ilhas terminadas, significa que Novo_estado eh o estado resultante de aplicar o predicado
% tira_ilhas_terminadas_entrada a cada uma das entradas de Estado
% ------------------------------------------------------------------------------------------------------------------
tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    % aplicar o predicado tira_ilhas_terminadas_entrada para cada entrada do Estado
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).


% ------------------------------------------------------------------------------------------------------------------
% marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada),
% em que Ilhas_term eh uma lista de ilhas terminadas e Entrada
% eh uma entrada (ver Seccao 2.4), significa que Nova_entrada eh a entrada obtida de
% Entrada da seguinte forma: se a ilha de Entrada pertencer a Ilhas_term, o numero
% de pontes desta eh substituido por 'X'; em caso contrario Nova_entrada eh igual a Entrada
% ------------------------------------------------------------------------------------------------------------------
% se a ilha estiver na lista das ilhas terminadas, marca-a, caso contrario, permance igual
marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(N1, I), V, P], [ilha('X', I), V, P]) :-
    member(ilha(N1, I), Ilhas_term), !.
marca_ilhas_terminadas_entrada(_, [ilha(N, I), V, P], [ilha(N, I), V, P]).


% ------------------------------------------------------------------------------------------------------------------
% marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado), em que
% Estado eh um estado (ver Seccao 2.4) e Ilhas_term eh uma lista de ilhas
% terminadas, significa que Novo_estado eh o estado resultante de aplicar o predicado
% marca_ilhas_terminadas_entrada a cada uma das entradas de Estado
% ------------------------------------------------------------------------------------------------------------------
marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    % aplicar o predicado marca_ilhas_terminadas_entrada para cada entrada do Estado
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).


% ------------------------------------------------------------------------------------------------------------------
% trata_ilhas_terminadas(Estado, Novo_estado), em que Estado eh um estado
% (ver Seccao 2.4), significa que Novo_estado eh o estado resultante de aplicar
% os predicados tira_ilhas_terminadas e marca_ilhas_terminadas a Estado
% ------------------------------------------------------------------------------------------------------------------
trata_ilhas_terminadas(Estado, Novo_estado) :-
    % obter a lista com a ilhas terminadas
    ilhas_terminadas(Estado, Ilhas_term),
    % marcar as mesmas
    marca_ilhas_terminadas(Estado, Ilhas_term, Estado_aux),
    % finalmente, tira-las
    tira_ilhas_terminadas(Estado_aux, Ilhas_term, Novo_estado).


% ------------------------------------------------------------------------------------------------------------------
% junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado), em
% que Estado eh um estado e Ilha1 e Ilha2 sao 2 ilhas, significa que Novo_estado eh
% o estado que se obtem de Estado por adicao de Num_pontes pontes entre Ilha1 e Ilha2
% ------------------------------------------------------------------------------------------------------------------
junta_pontes(Estado, N, ilha(N1, Pos1), ilha(N2, Pos2), Novo_estado) :-
    % cria ponte entre Ilha1 e Ilha2
    cria_ponte(Pos1, Pos2, Ponte),
    length(Ponte_N, N),
    % cria lista com N pontes iguais
    maplist(=(Ponte), Ponte_N),
    % adiciona a lista criada acima na lista de pontes da entrada respetiva a Ilha1
    maplist(encontra_ilha(Ponte_N, ilha(N1, Pos1)), Estado, Novo_estado_aux1),
    % adiciona a lista criada acima na lista de pontes da entrada respetiva a Ilha2
    maplist(encontra_ilha(Ponte_N, ilha(N2, Pos2)), Novo_estado_aux1, Novo_estado_aux2),
    actualiza_vizinhas_apos_pontes(Novo_estado_aux2, Pos1, Pos2, Novo_estado_aux3),
    trata_ilhas_terminadas(Novo_estado_aux3, Novo_estado).

encontra_ilha(Pontes, Ilha, [Ilha, Vizinhas, P], [Ilha, Vizinhas, Pontes_N]) :-
    append(P, Pontes, Pontes_N), !.

encontra_ilha(_, _, [Ilha1, Vizinhas, Pontes], [Ilha1, Vizinhas, Pontes]).