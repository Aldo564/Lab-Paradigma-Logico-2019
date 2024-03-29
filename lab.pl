%Hechos

%##########################################################
%Escenas validos de 5X10 con 2, 4 y 6 enemigos
scene([5,10,2,0,0,
     [["0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0"]
     ,["1","0","0","0","0","0","0","0","2","2"]
     ,["#","#","#","#","#","#","#","#","#","#"]
     ,["#","#","#","#","#","#","#","#","#","#"]]]).

scene([5,10,4,0,0,
     [["0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0"]
     ,["1","2","3","0","0","0","9","9","9","9"]
     ,["#","#","#","#","#","#","#","#","#","#"]
     ,["#","#","#","#","#","#","#","#","#","#"]]]).



scene([5,10,5,0,0,
     [["0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0"]
     ,["1","2","3","4","0","9","9","9","9","9"]
     ,["#","#","#","#","#","#","#","#","#","#"]
     ,["#","#","#","#","#","#","#","#","#","#"]]]).


%##########################################################
%Escenas validos de 10X12 con 4 y 6 enemigos

scene([10,12,4,0,0,
     [["0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["1","2","3","0","0","0","0","0","9","9","9","9"]
     ,["#","#","#","#","#","#","#","#","#","#","#","#"]
     ,["#","#","#","#","#","#","#","#","#","#","#","#"]]]).

scene([10,12,6,0,0,
     [["0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["1","2","3","4","5","0","9","9","9","9","9","9"]
     ,["#","#","#","#","#","#","#","#","#","#","#","#"]
     ,["#","#","#","#","#","#","#","#","#","#","#","#"]]]).


%##########################################################
%Escenas validos de 20X20 con 8 enemigos

scene([20,20,8,0,0,
     [["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0"]
     ,["1","2","3","4","5","6","7","0","0","0","0","0","9","9","9","9","9","9","9","9"]
     ,["#","#","#","#","#","#","#","#","#","#","#","#","#","#","#","#","#","#","#","#"]
     ,["#","#","#","#","#","#","#","#","#","#","#","#","#","#","#","#","#","#","#","#"]]]).


%##########################################################
%Reglas


%#############################################
%Funciones Getters

getAlto(SCENE,ELEM):- % N
    nth0(1,SCENE,ELEM).

getAncho(SCENE, ELEM):- % M
    nth0(0,SCENE,ELEM).

getEnemigos(SCENE,ELEM):- % E
    nth0(2,SCENE,ELEM).

getDif(SCENE,ELEM):- % D
    nth0(3,SCENE,ELEM).

getSeed(SCENE,ELEM):- % SEED
    nth0(4,SCENE,ELEM).

getScene(SCENE,ELEM):- % SCENE
    nth0(5,SCENE,ELEM).

getFila(MATRIZ, F, ELEM):-
    nth0(F, MATRIZ, ELEM).

existe(_, [], _, _):-fail.

existe(_, [], POSx, POSx).

existe(ELEM, [X|_], POSx, _):-
    ELEM = X,
    existe(_, [], POSx, POSx).

existe(ELEM,[X|Xs], POSx, OUT):-
    ELEM \= X,
    POSx1 is POSx + 1,
    existe(ELEM, Xs, POSx1, OUT).


pos(X,[X|_],0).

pos(_,[],_):-
    !,fail.

pos(X,[_|R],Pos):-
    pos(X,R,Pos1),
    Pos is Pos1+1.

%Ejemplos:
% Los ejemplos aqui puestos no estan sujetos a las restricciones del
% enunciado

%getAlto([2,3,2,0,0,[["0","0","0"],["0","0","0"]]],ELEM).
%getAlto([2,5,2,0,0,[["0","0","0","0","0"],["0","0","0","0","0"]]],ELEM).
%getAlto([2,2,2,0,0,[["0","0"],["0","0"]]],ELEM).

%getAncho([2,3,2,0,0,["0","0","0"],["0","0","0"]],ELEM).
%getAncho([3,3,3,0,0,["0","0","0"],["0","0","0"],["0","0","0"],ELEM).
%getAncho([4,3,2,0,0,["0","0","0"],["0","0","0"],["0","0","0"],["0","0","0"]],ELEM).

%getEnemigos[2,3,2,0,0,[["0","2","2"],["#","#","#"]]],ELEM).
%getEnemigos[2,3,4,0,0,[[""",""",""","2"],["#","#","#","#"]]],ELEM).
%getEnemigos[4,3,2,0,0,[["0","0","0"],["0","0","0"],["0","2","2"],["#","#","#"]]],ELEM).

%getDif([2,3,2,0,0,[["0","0","0"],["0","0","0"]]],ELEM).
%getDif([2,5,2,0,0,[["0","0","0","0","0"],["0","0","0","0","0"]]],ELEM).
%getDif([2,2,2,0,0,[["0","0"],["0","0"]]],ELEM).

%getSeed([2,3,2,0,0,[["0","0","0"],["0","0","0"]]],ELEM).
%getSeed([2,5,2,0,0,[["0","0","0","0","0"],["0","0","0","0","0"]]],ELEM).
%getSeed([2,2,2,0,0,[["0","0"],["0","0"]]],ELEM).

%getScene([2,3,2,0,0,[["1","0","2"],["#","#","#"]]],ELEM).
%getScene([2,3,4,0,0,[["0","2","0"],["#","#","#"]]],ELEM).
%getScene([2,3,2,0,0,[["0","1","1"],["#","#","#"]]],ELEM).

%getFila([["1","0","2"],["#","#","#"]], 0, ELEM).
%getFila([["0","2","0"],["#","#","#"]], 1, ELEM).
%getFila([["0","0","0"],["0","1","1"],["#","#","#"]], 2, ELEM).


%#############################################


%#############################################
%Funcion que calcula las columnas de una matriz.
%sizeColumnas debe recibir una lista.
sizeColumnas([],0).

sizeColumnas([_|Xs],N):-
    sizeColumnas(Xs, N1),
    N is N1 +1.

% ya que "sizeColumnas" es una funcion puente, sus ejemplos estan en
% "size"

%#############################################


%#############################################
%Funcion que calcula las filas de una matriz.
%sizeFilas debe recibir una matriz.
sizeFilas([], 0).

sizeFilas([_|Xs],N):-
    sizeFilas(Xs, N1),
    N is N1 + 1.

% ya que "sizeFilas" es una funcion puente, sus ejemplos estan en
% "size"

%#############################################

%#############################################
% Funcion que junta sizeColumnas y sizeFilas y devuelve ambas medidas en
% N y M respectivamente.
size([X|Xs], N, M):-
    MATRIZ = [X|Xs],
    sizeColumnas(X, M),
    sizeFilas(MATRIZ, N).

%Ejemplos:
%size([[1,2],[3,4]], 2, 2).
%size([[1,2,3]], 3, 1).
%size([[1,2],[3,4],[5,7]], 2, 2).

%#############################################



%#############################################
% Funcion que verifica que los valores dados cumplan con las
% especificaciones pedidas.
isScene(N, M, E, ENEMIGOS, MATRIZ):-
    N >= 5,
    M >= 10,
    E = ENEMIGOS,
    size(MATRIZ, NAUX, MAUX),
    NAUX == N,
    MAUX == M.

% Debido a que isSpace es una funcion puente de checkSpace, los ejemplos
% seran puestos en dicha funcion

%#############################################

%#############################################
% Funcion que cuenta los enemigos en un tablero

contarEnemigos([],CONT,CONT).

contarEnemigos([X|Xs], CONT, OUT):-
    X \= "9",
    contarEnemigos(Xs, CONT,OUT).


contarEnemigos([X|Xs], CONT,OUT):-
    X == "9",
    CONT1 is CONT + 1,
    contarEnemigos(Xs, CONT1,OUT).

contarEnemigos([], E, E).

contarEnemigos([M|Ms], E, CONT):-
    contarEnemigos(M, 0,CONT1),
    CONT2 is CONT1 + CONT,
    contarEnemigos(Ms, E, CONT2).



%#############################################
%Funcion que me dice si la escena es valida o no,
%chequea si el ancho y alto son validos segun las medidas puestas,
%cuenta la cantidad de enemigos en el espacio y lo compara con
%la informacion en el TDA.

checkScene([N,M,E,_, _, _]):-
    scene([N, M, ENEMIGOS, 0, 0, MATRIZ]),
    isScene( N, M, E, ENEMIGOS, MATRIZ),
    contarEnemigos(MATRIZ, E, 0).

%Ejemplos:
%checkScene([5, 10, 4, _, _, _]).
%checkScene([10, 12, 4, _, _, _]).
%checkScene([20, 20, 8, _, _, _]).

%#############################################


%#############################################
% Funcion que busca entre los hechos, la escena con las caracteristicas
% pedidas, ademas valida si todos los datos son correctos.

createScene(N, M, E, _, _, _, SCENE):-  % D, SEED
    scene([N, M, E, 0, 0, MATRIZ]),
    checkScene([N, M, E, 0, 0, MATRIZ]),
    SCENE = [N, M, E, 0, 0, MATRIZ].
    %Swrite(SPACE).
    %imprimir(SPACE).

%Ejemplos:
%createScene(5,10,2,_,_,_,OUT).
%createScene(10,12,6,_,_,_,OUT).
%createScene(20,20,8,_,_,_,OUT).

%#############################################


%#############################################
% Funcion que avanza en las columnas y va llamando la funcion
% string_concat con la cabeza de la lista y BOARDSTR, la salida final de
% la funcion es BOARDSTR2.

filaToString([],[], BOARDSTR, FINAL):-
    columnaToString([],[],BOARDSTR, FINAL).

filaToString([], [X|Xs], BOARDSTR, FINAL):-
     string_concat(BOARDSTR, X, BOARDSTR1),
     string_concat(BOARDSTR1, " ", BOARDSTR2),
     filaToString([], Xs, BOARDSTR2, FINAL).

filaToString([M|Ms], [], BOARDSTR, FINAL):-
    columnaToString(Ms, M, BOARDSTR, FINAL).

filaToString([M|Ms], [X|Xs], BOARDSTR, FINAL):-
    string_concat(BOARDSTR, X, BOARDSTR1),
    string_concat(BOARDSTR1, " ", BOARDSTR2),
    filaToString([M|Ms], Xs, BOARDSTR2, FINAL).

% ya que "moverColumnas" es una funcion puente, los ejemplos de esta
% estan en "boardToString"

%#############################################

%#############################################
% Funcion que avanza en las filas de la matriz y va llamando a
% moverColumnas para guardar su contenido.

columnaToString([], [], BOARDSTR, BOARDSTR).

columnaToString(MATRIZ,FILA, BOARDSTR, FINAL):-
    string_concat(BOARDSTR, "\n", BOARDSTR1),
    filaToString(MATRIZ, FILA, BOARDSTR1, FINAL).

% ya que "moverFilas" es una funcion puente, los ejemplos de esta
% estan en "boardToString"

%#############################################

%#############################################
%Funcion que transforma un matriz en string

matrizToString([M|Ms], BOARDSTR, FINAL):-
    columnaToString(Ms, M, BOARDSTR, FINAL).

sceneToString(SCENEIN,SCENESTR):-
    getScene(SCENEIN, SCENE),
    matrizToString(SCENE, "" ,SCENESTR).

%Ejemplos:
%sceneToString([["#","O"],["#","O"]], FINAL).
%sceneToString([[1,2],[3,4]], FINAL).
%sceneToString([[$,#,%],[&,/,*],[-,+,!]], FINAL).

%#############################################


%#############################################
% Funcion MoveMember



verificarEspacio(SCENEIN, X, Y, MOVEDIR):-
    MOVEDIR = 1, %esto significa movimiento a la izquierda
    X > 0,
    X1 is X - 1,
    getScene(SCENEIN, SCENE),
    getFila(SCENE, Y, FILA),
    getFila(FILA, X1, ELEM),
    ELEM = "0".


verificarEspacio(SCENEIN, X, Y, MOVEDIR):-
    MOVEDIR = 0, %esto significa movimiento a la derecha
    getAncho(SCENEIN, ANCHO),
    X < ANCHO,
    X1 is X + 1,
    getScene(SCENEIN, SCENE),
    getFila(SCENE, Y, FILA),
    getFila(FILA, X1, ELEM),
    ELEM = "0".


buscarMember(SCENEIN, MEMBER, X, Y):-
    getAncho(SCENEIN, ANCHO),
    Y is ANCHO - 3,
    getScene(SCENEIN, SCENE),
    getFila(SCENE, Y, FILA),
    pos(MEMBER,FILA, X).

replace([_|Xs], 0, SHIP, [SHIP|Xs]).

replace([X|XS], I, SHIP, [X|YS]):-
    I > -1,
    NI is I-1,
    replace(XS, NI, SHIP, YS), !.

replace(L, _, _, L).


mover(SCENEIN, MEMBER, OLDX, NEWX, Y, SCENEOUT):-
    getScene(SCENEIN, SCENE),
    getFila(SCENE, Y, FILA),
    replace(FILA, OLDX, "0", ELEM),
    replace(ELEM, NEWX, MEMBER, NEWFILA),
    replace(SCENE, Y, NEWFILA, NEWSCENE),
    replace(SCENEIN, 5, NEWSCENE, SCENEOUT).


moveMember(SCENEIN, MEMBER, MOVEDIR, _, SCENEOUT):- %Seed
    MOVEDIR = 0, %DERECHA
    buscarMember(SCENEIN, MEMBER, X, Y),
    verificarEspacio(SCENEIN, X, Y, MOVEDIR),
    X1 is X + 1,
    mover(SCENEIN, MEMBER, X, X1, Y, SCENEOUT),
    sceneToString(SCENEOUT,SCENESTR),
    write(SCENESTR).

moveMember(SCENEIN, MEMBER, MOVEDIR, _, SCENEOUT):- %Seed
    MOVEDIR = 1, %IZQUIERDA
    buscarMember(SCENEIN, MEMBER, X, Y),
    verificarEspacio(SCENEIN, X, Y, MOVEDIR),
    X1 is X - 1,
    mover(SCENEIN, MEMBER, X, X1, Y, SCENEOUT),
    sceneToString(SCENEOUT,SCENESTR),
    write(SCENESTR).



%ejemplo movimiento a la derecha
%moveMember([5,10,2,0,0,[["0","0","0","0","0","0","0","0","0","0"],["0","0","0","0","0","0","0","0","0","0"],["1","0","0","0","0","0","0","0","2","2"],["#","#","#","#","#","#","#","#","#","#"],["#","#","#","#","#","#","#","#","#","#"]]], "1", 0, 0, OUT).

%ejemplo movimiento a la izquierda
%moveMember([5,10,2,0,0,[["0","0","0","0","0","0","0","0","0","0"],["0","0","0","0","0","0","0","0","0","0"],["0","1","0","0","0","0","0","0","2","2"],["#","#","#","#","#","#","#","#","#","#"],["#","#","#","#","#","#","#","#","#","#"]]], "1", 1, 0, OUT).

%movimiento a la derecha de member 3
%moveMember([10,12,4,0,0,[["0","0","0","0","0","0","0","0","0","0","0","0"],["0","0","0","0","0","0","0","0","0","0","0","0"],["0","0","0","0","0","0","0","0","0","0","0","0"],["0","0","0","0","0","0","0","0","0","0","0","0"],["0","0","0","0","0","0","0","0","0","0","0","0"],["0","0","0","0","0","0","0","0","0","0","0","0"],["0","0","0","0","0","0","0","0","0","0","0","0"],["1","2","3","0","0","0","0","0","9","9","9","9"],["#","#","#","#","#","#","#","#","#","#","#","#"],["#","#","#","#","#","#","#","#","#","#","#","#"]]],"3", 0, 0, OUT).


