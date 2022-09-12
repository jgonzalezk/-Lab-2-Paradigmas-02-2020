%HECHOS
%2 Stacks
stack(1,[[["Jorge", "Clave1", 300], ["Kamila", "Clave2", 300], ["Felipe", "Clave3", 300], ["Alejandra", "Clave4", 300]], [[5, "Porque no me guarda todo el string cuando utilizo %c", "30-12-2020", ["C", "Input"], [[1, "Tienes que utilizar %s y reservarle memoria a la variable.", "01-12-2020", [], "Jorge", 0, 0], [2, "Respuesta generica", "04-12-2020", ["Tag test"], "Felipe", 0, 0]], 0, 0, 0, "Alejandra", 0, 0], [4, "Scheme como puedo aplicar una funcion a toda una lista", "28-11-2020", ["Scheme"], [[1, "Puedes utilizar la funcion map", "01-12-2020", ["Scheme", "List"], "Felipe", 0, 0], [2, "Podrias implementar tu propia funcion de map mediante a una recursion", "01-12-2020", ["Recursion", "Scheme"], "Jorge", 0, 0]], 0, 0, 0, "Jorge", 0, 0], [3, "Como puedo hacer que en Prolog para concatenar strings", "27-11-2020", ["Prolog", "String"], [[1, "Puedes concatenar dos string mediante el predicado string_concat(Str1,Str2,StrResultado).", "02-12-2020", [], "Alejandra", 0, 0]], 0, 0, 0, "Kamila", 0, 0], [2, "Por que me sale el sgte error: c error out of memory", "23-11-2020", ["C", "Punteros", "Arreglos"], [[1, "El erro corresponde a que te sales de las posiciones del arreglo.", "02-12-2020", [], "Alejandra", 0, 0], [2, "Quiere decir que te estas saliendo de la memoria de que has declarado", "04-12-2020", [], "Jorge", 0, 0]], 0, 0, 0, "Felipe", 0, 0], [1, "Como puedo hacer una lista en python", "22-11-2020", ["Python", "Lista"], [[1, "Se puede crear una lista de muchas formas pero prueba con definir una lista asi [1,2,3]", "02-12-2020", [], "Alejandra", 0, 0], [2, "Si deseas pasar un string a lista puedes utilizar la funcion list(String), esto te devuelve todo el string separado en una lista", "03-12-2020", [], "Kamila", 0, 0], [3, "Para separar un string mediante los espacios para transformarlo en lista utiliza la funcion .split()", "04-12-2020", ["Python"], "Jorge", 0, 0]], 0, 0, 0, "Felipe", 0, 0]], []]).

stack(2,[[["Jorge", "Clave1", 55], ["Kamila", "Clave2", 130], ["Felipe", "Clave3", 326], ["Alejandra", "Clave4", 475]], [[5, "Pregunta5", "30-12-2020", ["C", "Input", "Tag3"], [[1, "Respuesta 2", "01-12-2020", [], "Felipe", 0, 0], [2, "Respuesta 7", "03-12-2020", [], "Felipe", 0, 0], [3, "Respuesta 10", "04-12-2020", [], "Jorge", 0, 0]], 0, 0, 0, "Kamila", 0, 0], [4, "Pregunta4", "28-11-2020", ["Scheme"], [[1, "Respuesta generica 1", "01-12-2020", ["Etiqueta1"], "Felipe", 0, 0], [2, "Respuesta 6", "02-12-2020", [], "Kamila", 0, 0]], 0, 0, 0, "Jorge", 0, 0], [3, "Pregunta3", "27-11-2020", ["Prolog"], [[1, "Respuesta 5", "02-12-2020", [], "Alejandra", 0, 0]], 0, 0, 0, "Jorge", 0, 0], [2, "Pregunta2", "23-11-2020", [], [[1, "Respuesta 8", "04-12-2020", ["Tag test"], "Felipe", 0, 0]], 0, 0, 0, "Felipe", 0, 0], [1, "Como puedo hacer una lista en python", "22-11-2020", ["Python", "Lista"], [[1, "Respuesta3", "01-12-2020", ["Recursion", "Scheme"], "Jorge", 0, 0], [2, "Respuesta 4", "02-12-2020", [], "Alejandra", 0, 0], [3, "Respuesta 9", "04-12-2020", ["Python"], "Jorge", 0, 0]], 0, 0, 0, "Felipe", 0, 0]], []]).


%----------------------------
%TDA Usuario
/*Dominio
Nombre: Nombre del usuario.
Clave: Contraseña del usuario.
Puntos: Puntos del usuario.
PuntosNuevos: Los puntos actualizados del usuario.
AddPoints: Puntos a agregar.
Usuario: Lista que contiene los datos del usuario, tales como nombre,clave y puntos.
UsuarioOut: Usuario con los puntos modificados.
*/

/*Predicados
crearUsuario(Nombre,Clave,Usuario).
esUsuario(Usuario).
getName(Usuario).
CambiarPuntos(Usuario,AddPoints,UsuarioOut).
*/

/*Metas
Principales: crearUsuario,esUsuario,getName y cambiarPuntos.
*/

/*Clausulas
Reglas y Hechos*/
%Constructor
crearUsuario(Nombre,Clave,[Nombre, Clave, 300]).%Para efectos de probar los predicados los usuarios comienzan con 300 puntos.

%Pertenencia
esUsuario([Nombre,Clave,Puntos]):- string(Nombre),string(Clave),number(Puntos).

%Selector
getName([Nombre|_],Nombre).

%Modificadores
cambiarPuntos([Nombre,Clave,Puntos],AddPoints,[Nombre,Clave,PuntosNuevos]):-
    PuntosNuevos is Puntos+AddPoints.

%---------------------------- PREDICADOS DE UTILIDAD ----------------------------
/*Dominio
E: Elemento a agregar.
A: Cabeza de la lista.
Cola: Cola de la lista original.
NuevaCola: Cola de la nueva lista.
ListaString: Lista con los string que se desean concatenar.
StringResultado: String resultado tras concatenar.
Listas: Listas de cada String.
Lista: Lista de todos los char.
*/

/*Predicados
agregarAlFinal(ListaInicial,Elemento,ListaSalida).
listaVacia(ListaComprobar).
concatenar(ListaString,StringResultado).
*/

/*Metas
Principales: agregarAlFinal,listaVacia y concatenar.
*/

/*Clausulas
Reglas y Hechos*/
agregarAlFinal([],E, [E]).
agregarAlFinal([A|Cola], E, [A|NuevaCola]):-
    agregarAlFinal(Cola, E, NuevaCola).

%Hecho de una lista vacia
listaVacia([]).

%Concatenar multiples strings.
concatenar(ListaString, StringResultado) :-
    maplist(atom_chars, ListaString, Listas),
    append(Listas, Lista),
    atom_chars(StringResultado, Lista).
%--------------------------------------------------------------------------------
%TDA Pregunta
/*Dominio
ID: Identificador de la pregunta.
Pregunta: Texto sobre la pregunta.
Fecha: Fecha de la creacion de la pregunta.
Tags: Etiquetas de la pregunta.
Respuestas: Respuestas de la pregunta.
Estado: Estado de la pregunta, resuelta o no resuelta.(1 o 0 respectivamente)
ResAccept: Identificador de la respuesta aceptada. (0 si aun no es aceptada ninguna respuesta)
Recompensa: Recompensa de la pregunta.
Autor: Autor de la pregunta.
VotoF: Votos positivos de la pregunta.
VotoC: Votos negativos de la pregunta.
IDRes: Identificador de una respuesta.
Cola: Cola de una lista.
VFNuevo: Nueva cantidad de votos positivos.
VCNuevo: Nueva cantidad de votos negativos.
RespuestasNew: Lista de respuestas actualizada.
PreguntaOut: Pregunta ya creada.
PreguntaOut1: Pregunta modificada.
*/

/*Predicados
crearPregunta(ID,Pregunta,Fecha,Tags,Respuestas,Estado,ResAccept,Recompensa,Autor,VotoF,VotoC,PreguntaOut).
getID(PreguntaOut,ID).
getRespuesta(PreguntaOut,Respuesta).
getRecompensa(PreguntaOut,Recompensa).
getAutor(PreguntaOut,Autor).
esPregunta(PreguntaOut).
existeRespuesta(PreguntaOut,IDRes).
setVotoF(PreguntaOut,PreguntaOut1).
setVotoC(PreguntaOut,PreguntaOut1).
setRecompensa(PreguntaOut,CantidadaAgregar,PreguntaOut1).
setResAceptada(PreguntaOut,IDRes,PreguntaOut1).
setNuevaRespuesta(PreguntaOut,Respuesta,PreguntaOut1).
setRespuestas(PreguntaOut,RespuestasNew,PreguntaOut1).
getNombreRespuesta(PreguntaOut,IDRes,Autor).

*/

/*Metas
Principales:crearPregunta, getID, getRespuesta, getRecompensa, getAutor, esPregunta, existeRespuesta,
setVotoF, setVotoC, setRecompensa, setResAceptada, setNuevaRespuesta, setRespuestas y getNombreRespuesta.
*/

/*Clausulas
Reglas y Hechos*/
%Constructor
crearPregunta(ID,Pregunta,Fecha,Tags,Respuestas,Estado,ResAccept,Recompensa,Autor,VotoF,VotoC,
              [ID,Pregunta,Fecha,Tags,Respuestas,Estado,ResAccept,Recompensa,Autor,VotoF,VotoC]).

%Selectores
getID([ID|_],ID).
getRespuesta([_,_,_,_,Resp|_],Resp). %Selector de la lista de respuestas
getRecompensa([_,_,_,_,_,_,_,Recom|_],Recom).
getAutor([_,_,_,_,_,_,_,_,Autor|_],Autor).

%Selector del autor de una respuesta en especifico.
getNombreRespuesta([[IDRes,_,_,_,Autor|_]|_],IDRes,Autor):-!.
getNombreRespuesta([_|Cola],IDRes,Autor):-getNombreRespuesta(Cola,IDRes,Autor).

%Pertenencia              
esPregunta([ID,Pregunta,Fecha,Tags,Respuestas,Estado,ResAccept,Recompensa,Autor,VotoF,VotoC]):-
    number(ID),string(Pregunta),string(Fecha),is_list(Tags),is_list(Respuestas),number(Estado),
    number(ResAccept),number(Recompensa),string(Autor),number(VotoF),number(VotoC).

existeRespuesta([[IDRes|_]|_],IDRes):-!.%Existe o no la respuesta buscada.
existeRespuesta([_|Cola],IDRes):-existeRespuesta(Cola,IDRes).

%Modificadores    
setVotoF([ID,Pregunta,Fecha,Tags,Respuestas,Estado,ResAccept,Recompensa,Autor,VotoF,VotoC],PregOut):-
    VFNuevo is VotoF+1, PregOut=[ID,Pregunta,Fecha,Tags,Respuestas,Estado,ResAccept,Recompensa,Autor,VFNuevo,VotoC].

setVotoC([ID,Pregunta,Fecha,Tags,Respuestas,Estado,ResAccept,Recompensa,Autor,VotoF,VotoC],PregOut):-
    VCNuevo is VotoC+1, PregOut=[ID,Pregunta,Fecha,Tags,Respuestas,Estado,ResAccept,Recompensa,Autor,VotoF,VCNuevo].

setRecompensa([ID,Pregunta,Fecha,Tags,Respuestas,Estado,ResAccept,_,Autor,VotoF,VotoC],Rec,
              [ID,Pregunta,Fecha,Tags,Respuestas,Estado,ResAccept,Rec,Autor,VotoF,VotoC]).

setResAceptada([ID,Pregunta,Fecha,Tags,Respuestas,_,_,Recompensa,Autor,VotoF,VotoC],Raccept,
              [ID,Pregunta,Fecha,Tags,Respuestas,1,Raccept,Recompensa,Autor,VotoF,VotoC]).

%Agregar una nueva respuesta
setNuevaRespuesta([ID,Pregunta,Fecha,Tags,Respuestas,Estado,ResAccept,Recompensa,Autor,VotoF,VotoC],Res,PregOut):-
    agregarAlFinal(Respuestas,Res,ResOut),PregOut=[ID,Pregunta,Fecha,Tags,ResOut,Estado,ResAccept,Recompensa,Autor,VotoF,VotoC].

%Cambiar las respuestas a una lista de respuestas modificadas.
setRespuestas([ID,Pregunta,Fecha,Tags,_,Estado,ResAccept,Recompensa,Autor,VotoF,VotoC],RespuestasNew,
              [ID,Pregunta,Fecha,Tags,RespuestasNew,Estado,ResAccept,Recompensa,Autor,VotoF,VotoC]).

%--------------------------------------------------------------------------------
%TDA Respuesta
/*Dominio
ID: Identificador de la respuesta.
Respuesta: Texto sobre la respuesta.
Fecha: Fecha de la creacion de la respuesta.
Tags: Etiquetas de la respuesta.
Autor: Autor de la respuesta.
VotosF: Votos positivos de la respuesta.
VotosC: Votos negativos de la respuesta.
IDPregunta: Identificador de la pregunta donde se quiere agregar la respuesta.
X: Cantidad de respuestas que tiene la pregunta.
ResOut: Respuesta ya creada.
RespMod: Respuesta modificada.
Respuestas: Listado de respuestas.
Out: Listado de respuestas actualizado.
*/

/*Predicados
crearRespuesta(Preguntas,IDPregunta,Respuesta,Fecha,Tags,Autor,VotosF,VotosC,ResOut).
esRespuesta(ResOut).
getAutorR(ResOut,Autor).
getIDRes(ResOut,ID).
setVotoFRes(ResOut,RespMod).
setVotoCRes(ResOut,RespMod).
actualizarRespuesta(Respuestas,RespMod,Out).
getRespuestaAlt(Respuestas,ID,ResOut).
*/

/*Metas
Principales:crearRespuesta, esRespuesta, getAutorR, getIDRes, setVotoFRes, setVotoCRes, actualizarRespuesta
 y getRespuestaAlt.
*/

/*Clausulas
Reglas y Hechos*/
%Constructor
crearRespuesta(Preguntas,IDPregunta,Respuesta,Fecha,Tags,Autor,VotosF,VotosC,[ID,Respuesta,Fecha,Tags,Autor,VotosF,VotosC]):-
    getPregunta(Preguntas,IDPregunta,Preg),getRespuesta(Preg,Resp),length(Resp,X),ID is X+1.

%Pertenencia
esRespuesta([ID,Respuesta,Fecha,Tags,Autor,VotosF,VotosC]):-
    number(ID),string(Respuesta),string(Fecha),is_list(Tags),string(Autor),number(VotosF),number(VotosC).

%Selectores
getAutorR([_,_,_,_,Autor|_],Autor).
getIDRes([IDRes|_],IDRes).

%Selector de una respuesta mediante su ID
getRespuestaAlt([Respuesta|_],IDres,RespuestaOut):-getIDRes(Respuesta,ID),ID==IDres,
    RespuestaOut=Respuesta,!.
getRespuestaAlt([_|Cola],IDres,RespuestaOut):- getRespuestaAlt(Cola,IDres,RespuestaOut).

%Modificadores
setVotoFRes([ID,Respuesta,Fecha,Tags,Autor,VotosF,VotosC],ResOut):-
    VFNuevo is VotosF+1, ResOut=[ID,Respuesta,Fecha,Tags,Autor,VFNuevo,VotosC].

setVotoCRes([ID,Respuesta,Fecha,Tags,Autor,VotosF,VotosC],ResOut):-
    VCNuevo is VotosC+1, ResOut=[ID,Respuesta,Fecha,Tags,Autor,VotosF,VCNuevo].

%Actualizar el listado de respuestas con una respuesta modificada.
actualizarRespuesta([Respuesta|Cola],ID,RespMod,Out):-getID(Respuesta,ID1),ID1==ID,
    Out=[RespMod|Cola],!.
actualizarRespuesta([Cabeza|Cola],ID,RespMod,[Cabeza|NuevaCola]):-actualizarRespuesta(Cola,ID,RespMod,NuevaCola).

%--------------------------------------------------------------------------------
%TDA Usuarios
/*Dominio
Usuarios: Lista que contiene a diferentes tda usuario.
UsuariosOut: Similar a Usuarios, solamente que este se ecuentra actualizado.
Nombre: Nombre del usuario que se esta buscando.
Password: Contraseña del usuario que se esta buscando.
UsuarioMod: Usuario modificado que se quiere actualiar.
*/

/*Predicados
agregarUsuario(Usuarios,Usuario,UsuariosOut).
existeUsuario(Usuarios,Nombre).
existeUsuarioLogin(Usuarios,Nombre,Password).
getPuntos(Usuarios,Puntos).
getUsuario(Usuarios,Nombre,UsuarioOut).
actualizarUsuario(Usuarios,Nombre,UsuarioMod,UsuariosOut).
*/

/*Metas
Principales:agregarUsuario, existeUsuario, existeUsuarioLogin, getPuntos, getUsuario y actualizarUsuario.
*/

/*Clausulas
Reglas y Hechos*/
%Constructor
%Agregar usuario a la lista de usuarios, caso en que la lista este vacia y no lo este.
agregarUsuario([],Usuario,[Usuario]).
agregarUsuario([U|Cola],Usuario,[U|NuevaCola]):- %La primera lista corresponde a la lista de Usuarios
    agregarUsuario(Cola,Usuario,NuevaCola).
    
%Pertenencia
%Comprobar si un usuario ya existe o no, esto si coinciden los nombres.
existeUsuario([[Nombre|_]|_], Nombre) :- !.
existeUsuario([_|Cola], Nombre) :- existeUsuario(Cola, Nombre).

%Comprobar si el nombre y password de un usuario que se quiere logear coincide con un usuario registrado
existeUsuarioLogin([[Nombre,Password|_]|_], Nombre,Password) :- !.
existeUsuarioLogin([_|Cola], Nombre,Password) :- existeUsuarioLogin(Cola, Nombre,Password).

%Selectores
%Conseguir los puntos de un usuario en especifico.
getPuntos([[Nombre,_,Points]|_], Nombre, Points) :-!.
getPuntos([_|Cola], Nombre, Puntos) :- getPuntos(Cola, Nombre, Puntos).

getUsuario([Usuario|_],Nombre,UsuarioOut):-getName(Usuario,Name),Name==Nombre,
    UsuarioOut=Usuario,!.
getUsuario([_|Cola],Nombre,UsuarioOut):- getUsuario(Cola,Nombre,UsuarioOut).

%Modificador
actualizarUsuario([Usuario|Cola],Nombre,UsuarioMod,Out):-getName(Usuario,Name),Name==Nombre,
    Out=[UsuarioMod|Cola],!.
actualizarUsuario([Cabeza|Cola],Nombre,UsuarioMod,[Cabeza|NuevaCola]):-actualizarUsuario(Cola,Nombre,UsuarioMod,NuevaCola).
                     
%--------------------------------------------------------------------------------
%TDA Preguntas
/*Dominio
Preguntas: Listado que contiene a multiples tda pregunta.
PreguntasOut: Similar a Preguntas, solamente que este se encuentra actualizado.
IDPreg: Identificador de una pregunta.
PregMod: Pregunta modificada.
*/

/*Predicados
agregarPregunta(Preguntas,Pregunta,PreguntasOut).
existePregunta(Preguntas,IDPreg).
getPregunta(Preguntas,IDPreg,PreguntaOut).
getIDanterior(Preguntas,ID).
actualizarPregunta(Preguntas,IDPreg,PregMod,PreguntasOut).
*/

/*Metas
Principales: agregarPregunta, existePregunta, getPregunta, getIDanterior y actualizarPregunta.
*/

/*Clausulas
Reglas y Hechos*/
%Constructor
%Agregar una pregunta a la lista de preguntas.
agregarPregunta([],Pregunta,[Pregunta]).
agregarPregunta(Preguntas,Pregunta,[Pregunta|Preguntas]).%Se agrega la pregunta al comienzo como si fuera un stack.

%Pertenencia
%Comprobar si una pregunta existe o no.
existePregunta([[IDpreg|_]|_], IDpreg) :- !.
existePregunta([_|Cola], IDpreg) :- existePregunta(Cola, IDpreg).

%Selectores
%Selector de una pregunta en especifico.
getPregunta([Pregunta|_], IDpreg,Out) :- getID(Pregunta,ID),ID==IDpreg,Out=Pregunta,!.
getPregunta([_|Cola], IDpreg,PregOut) :- getPregunta(Cola, IDpreg,PregOut).

getIDanterior([],ID):-ID=0.
getIDanterior([[ID|_]|_],ID).

%Modificador
%Acualizar una pregunta del listado de Preguntas
actualizarPregunta([Pregunta|Cola],ID,PregMod,Out):-getID(Pregunta,ID1),ID1==ID,
    Out=[PregMod|Cola],!.
actualizarPregunta([Cabeza|Cola],ID,PregMod,[Cabeza|NuevaCola]):-actualizarPregunta(Cola,ID,PregMod,NuevaCola).
%--------------------------------------------------------------------------------
%TDA Usuario Activo
/*Dominio
UsuarioActivo: Lista con los datos del usuario que inicio sesion.
Nombre: Nombre del usuario que inicio sesion.
Password: Contraseña del usuario que inicio sesion.
ListaUsuarios: Lista de los usuarios registrados.
*/

/*Predicados
agregarActivo(Nombre,Password,ListaUsuarios,UsuarioActivo).
hayActivo(ListaActivo).
getNombreActivo(ListaActivo,Nombre).
*/

/*Metas
Principales: agregarActivo, hayActivo y getNombreActivo.
*/

/*Clausulas
Reglas y Hechos*/
%Agregar el usuario que se esta logeando a usuario activo.
agregarActivo(Nombre,Password,ListaUsuarios,UsuarioActivo):-getPuntos(ListaUsuarios,Nombre,Puntos),UsuarioActivo=[Nombre,Password,Puntos].

%Verificar si hay un usuario activo, osea con la sesion iniciada.
hayActivo(ListaActivo):-is_list(ListaActivo),not(listaVacia(ListaActivo)).

getNombreActivo([Nombre|_],Nombre).
%--------------------------------------------------------------------------------
%TDA Stack
%Representacion:Stack=[[Lista de Usuarios],[Lista de Preguntas/Respuestas],[Usuario Activo]]
%stackVacio([[],[],[]]).
/*Dominio
Usuarios: Lista de los usuarios registrados.
Preguntas: Lista con las preguntas y respuestas.
Activo: Datos del usuario activo si es que lo hay.
Stack2: Stack de salida o Stack que se desea comparar cuando al stack inicial se le hace una modificacion.
*/

%--------------------------------------------------------------------------------
%Predicado para registrar un usuario
/*Dominio
Stack: Lista que cotiene a los TDAs Usuarios, Preguntas y Activo.
Username: Nombre del usuario que se desea registrar.
Password: Contraseña del usuario que se desea registrar.
Stack2: Stack de salida o Stack el cual se compara con el Stack tras el registro, sun si es una variable o ya esta definido.

stack X string X string X stack
*/

/*Predicados
stackRegister(Stack1,Username,Password,Stack2).
*/

/*Metas
Principales: stackRegister.
*/

stackRegister([Usuarios,Preguntas,Activo],Username,Password,Stack2):-
    not(existeUsuario(Usuarios,Username)),crearUsuario(Username,Password,UserOut),  %Comprobar si el usuario a registrar ya existe o no.
        esUsuario(UserOut),agregarUsuario(Usuarios,UserOut,NvoUsuarios),Temp=[NvoUsuarios,Preguntas,Activo], %Comprobar si el usuario creado es correcto, y si lo es se agrega a usuarios.
        ((is_list(Stack2),Temp==Stack2);(not(is_list(Stack2)),Stack2=Temp)).    %Verificar si Stack2 es una lista, en caso que lo sea comparan Temp y Stack2, si no lo es Stack2=Temp.

%--------------------------------------------------------------------------------
%Predicado de login
/*Dominio
Stack: Lista que cotiene a los TDAs Usuarios, Preguntas y Activo.
Username: Nombre del usuario que se desea logear.
Password: Contraseña del usuario que se desea logear.
Stack2: Stack de salida con el usuario activo si es que el login fue correcto.

stack X string X string X stack
*/

/*Predicados
stackLogin(Stack, Username, Password, Stack2).
*/

/*Metas
Principales: stackLogin.
*/
stackLogin([Usuarios,Preguntas,Activo],Username,Password,Stack2):-
    listaVacia(Activo),existeUsuarioLogin(Usuarios,Username,Password),agregarActivo(Username,Password,Usuarios,ActivoOut),
    Stack2=[Usuarios,Preguntas,ActivoOut].

%--------------------------------------------------------------------------------    
%Predicado de ask
/*Dominio
Stack: Lista que cotiene a los TDAs Usuarios, Preguntas y Activo.
Fecha: Fecha de la creacion de la pregunta.
TextoPregunta: Texto sobre la pregunta.
Etiquetas: Etiquetas de la pregunta.
Stack2: Stack de salida si es una variable, caso contrario se comprueba que al realizar la pregunta el Stack es igual al Stack2.

stack X string X string X list X stack
*/

/*Predicados
ask(Stack,Fecha,TextoPregunta,ListaEtiquetas,Stack2).
*/

/*Metas
Principales :ask.
*/
ask([Usuarios,Preguntas,Activo],Fecha,TextoPregunta,Etiquetas,Stack2):-
    hayActivo(Activo),getIDanterior(Preguntas,ID),getNombreActivo(Activo,Nombre),IDnew is ID+1,
    crearPregunta(IDnew,TextoPregunta,Fecha,Etiquetas,[],0,0,0,Nombre,0,0,PreguntaOut),esPregunta(PreguntaOut),!,
    agregarPregunta(Preguntas,PreguntaOut,PreguntasOut),Temp=[Usuarios,PreguntasOut,[]],
    ((is_list(Stack2),Temp==Stack2);(not(is_list(Stack2)),Stack2=Temp)),!.

%--------------------------------------------------------------------------------    
%Predicado de answer
/*Dominio
Stack: Lista que cotiene a los TDAs Usuarios, Preguntas y Activo.
Fecha: Fecha de creacion de la respuesta.
IDPregunta: Identificador de la pregunta a la cual se quiere responder.
TextoRespuesta: Texo de la respuesta.
Stack2: Es la salida si Stack2 es una variable, en caso de estar definido se comprueba si el Stack resultante tras realizar la respuesta es igual al Stack2.

stack X string X int X string X list X stack
*/

/*Predicados
answer(Stack, Fecha, IdPregunta, TextoRespuesta, ListaEtiquetas, Stack2).
*/

/*Metas
Principales: answer.
*/
answer([Usuarios,Preguntas,Activo],Fecha,IDPregunta,TextoRespuesta,Etiquetas,Stack2):-
	hayActivo(Activo),getNombreActivo(Activo,Nombre),existePregunta(Preguntas,IDPregunta),!,
    getPregunta(Preguntas,IDPregunta,Preg),
    crearRespuesta(Preguntas,IDPregunta,TextoRespuesta,Fecha,Etiquetas,Nombre,0,0,Resp),
    esRespuesta(Resp),setNuevaRespuesta(Preg,Resp,PregOut),actualizarPregunta(Preguntas,IDPregunta,PregOut,PreguntasOut),
    Temp=[Usuarios,PreguntasOut,[]],
    ((is_list(Stack2),Temp==Stack2);(not(is_list(Stack2)),Stack2=Temp)),!.

%--------------------------------------------------------------------------------
%Predicado de accept
/*Dominio
Stack: Lista que cotiene a los TDAs Usuarios, Preguntas y Activo.
IDPregunta: Identificador de la pregunta a la cual se quiere aceptar una respuesta.
IDRespuesta: Identificador de la respuests que quiere ser marcada como aceptada.
Stack2: Stack resultante tras aceptar la respuesta.

stack X int X int X stack
*/

/*Predicados
accept(Stack,IdPregunta,IdRespuesta,Stack2).
*/

/*Metas
Principales: accept.
*/
accept([Usuarios,Preguntas,Activo],IDPregunta,IDRespuesta,Stack2):-
    hayActivo(Activo),getNombreActivo(Activo,Nombre),existePregunta(Preguntas,IDPregunta),
    getPregunta(Preguntas,IDPregunta,Preg),getAutor(Preg,Autor),Autor==Nombre,
    getRespuesta(Preg,Resp),existeRespuesta(Resp,IDRespuesta),!,getRecompensa(Preg,Recom1),
    Recom is Recom1 + 15,%Agregar los +15 a la recompensa
    getUsuario(Usuarios,Nombre,UsuarioA),cambiarPuntos(UsuarioA,2,UsuarioModA),
    actualizarUsuario(Usuarios,Nombre,UsuarioModA,UsuariosOut1),
    getNombreRespuesta(Resp,IDRespuesta,NombreR),getUsuario(UsuariosOut1,NombreR,UsuarioR),
    cambiarPuntos(UsuarioR,Recom,UsuarioMod),actualizarUsuario(UsuariosOut1,NombreR,UsuarioMod,UsuariosOut),
    setRecompensa(Preg,0,PregMod),
    setResAceptada(PregMod,IDRespuesta,PregMod2),actualizarPregunta(Preguntas,IDPregunta,PregMod2,PreguntasOut),
    Stack2=[UsuariosOut,PreguntasOut,[]].
    

%--------------------------------------------------------------------------------
 respuestaToString([ID,Respuesta,Fecha,Tags,Autor,VotosF,VotosC],RespuestaStr):-
    number_string(ID,IDstr),etiquetaToString(Tags,TagStr),number_string(VotosF,VFstr),number_string(VotosC,VCstr),
    string_concat("ID respuesta: ",IDstr,Str1),string_concat("\nAutor: ",Autor,Str2),
	string_concat("\nFecha: ",Fecha,Str3),string_concat("\nEtiquetas: ",TagStr,Str4),string_concat("\nRespuesta: ",Respuesta,Str5),
    string_concat("\nVotos positivos: ",VFstr,Str6),string_concat("\nVotos negativos: ",VCstr,Str7),
    concatenar([Str1,Str2,Str3,Str4,Str5,Str6,Str7,"\n\n"],RespuestaStr).

etiquetaToString(Tags,TagStr):-atomics_to_string(Tags,", ",TagStr). %Predicado para las etiquetas de pregunta y respuesta

usuarioToString([Username,Password,Puntos],UsuarioStr):-
	number_string(Puntos,PuntosStr),
	string_concat("Usuario: ",Username,Str1),string_concat("\nContrasenia: ",Password,Str2),
	string_concat("\nPuntos: ",PuntosStr,Str3),
	concatenar([Str1,Str2,Str3,"\n\n"],UsuarioStr).

preguntaToString([ID,Pregunta,Fecha,Tags,Respuestas,Estado,ResAccept,Recompensa,Autor,VotoF,VotoC],PreguntaStr):-
    number_string(ID,IDStr),etiquetaToString(Tags,TagsStr),
    ((listaVacia(Respuestas),string_concat("","",RespuestasStr));(not(listaVacia(Respuestas)),respuestasToString(Respuestas,RespuestasStr1)
                                                                 ,concatenar(RespuestasStr1,RespuestasStr))),
    number_string(ResAccept,RacceptStr),number_string(Recompensa,RecomStr),number_string(VotoF,VotoFStr),
    number_string(VotoC,VotoCStr),
    string_concat("ID pregunta: ",IDStr,Str1),string_concat("\nAutor: ",Autor,Str2),
    string_concat("\nFecha: ",Fecha,Str3),string_concat("\nEtiquetas: ",TagsStr,Str4),
    string_concat("\nPregunta: ",Pregunta,Str5),((Estado==0,string_concat("\nEstado: ","No resuelta",Str6));
                                                 (Estado==1,string_concat("\nEstado: ","Resuelta",Str6))),
    ((ResAccept==0,string_concat("\nRespuesta aceptada: ","",Str7));(ResAccept\=1,string_concat("\nRespuesta aceptada: ",RacceptStr,Str7))),
    string_concat("\nVotos positivos: ",VotoFStr,Str8),string_concat("\nVotos negativos: ",VotoCStr,Str9),
    string_concat("\nRecompensa: ",RecomStr,Str10),string_concat("\nRespuestas:\n\n",RespuestasStr,Str11),
    concatenar([Str1,Str2,Str3,Str4,Str5,Str6,Str7,Str8,Str9,Str10,Str11,"\n\n"],PreguntaStr).

respuestasToString([],[]).
respuestasToString([Respuesta|Cola],[RespStr|Cola1]):- respuestaToString(Respuesta,RespStr),
    respuestasToString(Cola,Cola1).

usariosToString([],[]).
usariosToString([Usuario|Cola],[UsuarioStr|Cola1]):- usuarioToString(Usuario,UsuarioStr),
    usariosToString(Cola,Cola1).

preguntasToString([],[]).
preguntasToString([Pregunta|Cola],[PreguntaStr|Cola1]):- preguntaToString(Pregunta,PreguntaStr),
    preguntasToString(Cola,Cola1).

activoToString([Usuario,Password,Puntos],ActivoStr):-
    number_string(Puntos,PuntosStr),string_concat("Usuario: ",Usuario,Str1),
    string_concat("\nContrasenia: ",Password,Str2),string_concat("\nPuntos: ",PuntosStr,Str3),
    concatenar([Str1,Str2,Str3],ActivoStr).
    
filtrarPreguntas([],_,[]).
filtrarPreguntas([Pregunta|Cola],Autor,[Pregunta|Cola1]):-getAutor(Pregunta,Nombre),Nombre==Autor,
    filtrarPreguntas(Cola,Autor,Cola1).
filtrarPreguntas([Pregunta|Cola],Autor,Cola1):-getAutor(Pregunta,Nombre),Nombre\=Autor,
    filtrarPreguntas(Cola,Autor,Cola1),!.

%Predicado de stackToString
/*Dominio
Stack: Lista que cotiene a los TDAs Usuarios, Preguntas y Activo.
StrOut: Si en el stack se encuentra un usuario activo, esta variable pasa a ser la union de las preguntas realizadas
por el usuario logeado y su informacion como usuario como string. Caso en que no este logeado un usuario el stack completo pasa a ser string.

stack X string
*/

/*Predicados
stackToString(Stack,StrOut).
*/

/*Metas
Principales: stackToString.
*/  
stackToString([Usuarios,Preguntas,Activo],StrOut):-
    usariosToString(Usuarios,UsuariosStr1),preguntasToString(Preguntas,PreguntasStr1),
    concatenar(UsuariosStr1,UsuariosStr),concatenar(PreguntasStr1,PreguntasStr),
    %Todo el stack.
    ((listaVacia(Activo),string_concat("","",ActivoStr),string_concat("Usuarios:\n",UsuariosStr,Str1),
         string_concat("\nPreguntas:\n",PreguntasStr,Str2),string_concat("Usuario activo:\n",ActivoStr,Str3),
         concatenar([Str1,Str2,Str3],StrOut))
    ;
    %Solo las preguntas del usuario logeado y sus datos.
    (not(listaVacia(Activo)),activoToString(Activo,ActivoStr),getNombreActivo(Activo,Nombre),
        filtrarPreguntas(Preguntas,Nombre,PregFil),preguntasToString(PregFil,PreguntasStrMod),
        concatenar(PreguntasStrMod,PreguntasStrMod1),string_concat("\nPreguntas:\n",PreguntasStrMod1,Str1),
        string_concat("Usuario activo:\n",ActivoStr,Str2),concatenar([Str1,Str2],StrOut))
    ),!.
    


%--------------------------------------------------------------------------------
%Predicado de vote
/*Dominio
Stack: Lista que cotiene a los TDAs Usuarios, Preguntas y Activo.
AskAns: La pregunta o respuesta como tal a la cual se quiere votar.
IDPreg: ID de la pregunta SOLO para el caso de cuando se quiera votar a una respuesta, caso que sea una pregunta este valor no importa.
Voto: voto positivo o negativo.(true es positivo y false negativo).
Stack2: Stack de salida tras realizar la votacion.

stack X Pregunta o Respuesta X int X boolean X stack
*/

/*Predicados
vote(Stack,AskAns,IDPreg,Voto,Stack2).
*/

/*Metas
Principales: vote.
*/
vote([Usuarios,Preguntas,Activo],AskAns,IDPreg,Voto,Stack2):-
    getNombreActivo(Activo,NombreA),
    ((esPregunta(AskAns),getAutor(AskAns,AutorP),NombreA==AutorP,!,%No tiene mucho sentido votar a su propia pregunta, VOTAR PREGUNTA
         ((Voto==true, getUsuario(Usuarios,AutorP,UsuarioP),cambiarPuntos(UsuarioP,10,UsuarioModP), %Positivo
              getID(AskAns,ID),actualizarUsuario(Usuarios,AutorP,UsuarioModP,UsuariosOut),
              setVotoF(AskAns,AskAnsOut),actualizarPregunta(Preguntas,ID,AskAnsOut,PreguntasOut),
              Stack2=[UsuariosOut,PreguntasOut,[]]);
          (Voto==false,getUsuario(Usuarios,AutorP,UsuarioP),cambiarPuntos(UsuarioP,-2,UsuarioModP), %Negativo
			getID(AskAns,ID),actualizarUsuario(Usuarios,AutorP,UsuarioModP,UsuariosOut1),%Actualizar al autor de la pregunta            
              setVotoC(AskAns,AskAnsOut),getUsuario(UsuariosOut1,NombreA,UsuarioA),cambiarPuntos(UsuarioA,-1,UsuarioModA),
              actualizarUsuario(UsuariosOut1,NombreA,UsuarioModA,UsuariosOut),
              actualizarPregunta(Preguntas,ID,AskAnsOut,PreguntasOut),
              Stack2=[UsuariosOut,PreguntasOut,[]])),!)
    	;
    (esRespuesta(AskAns),getPregunta(Preguntas,IDPreg,Preg),getAutor(Preg,AutorP),AutorP==NombreA,!, %VOTAR RESPUESTA
        getAutorR(AskAns,AutorR),getID(AskAns,IDr),getRespuesta(Preg,Respuestas),
        ((Voto==true,getUsuario(Usuarios,AutorR,UsuarioR),cambiarPuntos(UsuarioR,10,UsuarioModR), %Positivo
            actualizarUsuario(Usuarios,AutorR,UsuarioModR,UsuariosOut),
            setVotoFRes(AskAns,AskAnsOut),actualizarRespuesta(Respuestas,IDr,AskAnsOut,RespuestasOut),
            setRespuestas(Preg,RespuestasOut,PregMod),actualizarPregunta(Preguntas,IDPreg,PregMod,PreguntasOut),
            Stack2=[UsuariosOut,PreguntasOut,[]]);
       	 (Voto==false,getUsuario(Usuarios,AutorR,UsuarioR),cambiarPuntos(UsuarioR,-2,UsuarioModR), %Negativo
            actualizarUsuario(Usuarios,AutorR,UsuarioModR,UsuariosOut1),getUsuario(UsuariosOut1,NombreA,UsuarioA),
             cambiarPuntos(UsuarioA,-1,UsuarioModA),actualizarUsuario(UsuariosOut1,NombreA,UsuarioModA,UsuariosOut),
             setVotoCRes(AskAns,AskAnsOut),actualizarRespuesta(Respuestas,IDr,AskAnsOut,RespuestasOut),
            setRespuestas(Preg,RespuestasOut,PregMod),actualizarPregunta(Preguntas,IDPreg,PregMod,PreguntasOut),
    		Stack2=[UsuariosOut,PreguntasOut,[]])),!)),!.

/*Dominio
Stack: Lista que cotiene a los TDAs Usuarios, Preguntas y Activo.
IDPreg: Identificador de la pregunta que se quiere obtener.
Preg: Pregunta obtenida.
IDRes: Identificador de la respuesta que se quiere obtener.
Res: Respuesta obtenida.
*/

/*Predicados
getQuestion(Stack,IDPreg,Preg).
getAnswer(Stack,IDPreg,IDRes,Res).
*/

/*Metas
Principales:getQuestion y getAnswer.
*/
%Reglas
getQuestion([_,Preguntas,_],IDPreg,Preg):- getPregunta(Preguntas,IDPreg,Preg),!.

getAnswer([_,Preguntas,_],IDPreg,IDRes,Res):-getPregunta(Preguntas,IDPreg,Preg),
    getRespuesta(Preg,Respuestas),getRespuestaAlt(Respuestas,IDRes,Res),!.
%--------------------------------------------------------------------------------
/*3 EJEMPLOS POR CADA PREDICADO
REGISTER
stackRegister([[],[],[]],"PepitoTroll","ClaveImposible",Stack1).
stack(1,Stack),stackRegister(Stack,"Mogwai","123456",StackOut).
stack(2,Stack),stackRegister(Stack,"Sambomaster","1342Spax",StackOut).

LOGIN
stack(1,Stack),stackLogin(Stack,"Jorge","Clave1",StackOut).
stack(1,Stack),stackLogin(Stack,"Felipe","Clave3",StackOut).
stack(2,Stack),stackLogin(Stack,"Alejandra","Clave4",StackOut).

ASK
stack(1,Stack),stackLogin(Stack,"Jorge","Clave1",StackOut),ask(StackOut,"05-12-2020","Pregunta ejemplo1",[],StackOut2).
stack(1,Stack),stackLogin(Stack,"Felipe","Clave3",StackOut),ask(StackOut,"05-12-2020","Pregunta ejemplo2",["e1","e2"],StackOut2).
stack(2,Stack),stackLogin(Stack,"Alejandra","Clave4",StackOut),ask(StackOut,"06-12-2020","Pregunta ejemplo3",["e1","e2","e3"],StackOut2).

ANSWER
stack(1,Stack),stackLogin(Stack,"Jorge","Clave1",StackOut),answer(StackOut,"05-12-2020",3,"Respuesta ejemplo1",[],StackOut2).
stack(1,Stack),stackLogin(Stack,"Felipe","Clave3",StackOut),answer(StackOut,"06-12-2020",4,"Respuesta ejemplo2",["e1","e2"],StackOut2).
stack(2,Stack),stackLogin(Stack,"Alejandra","Clave4",StackOut),answer(StackOut,"07-12-2020",1,"Respuesta ejemplo3",["e1","e2","e3"],StackOut2).

ACCEPT
stack(1,Stack),stackLogin(Stack,"Jorge","Clave1",StackOut),accept(StackOut,4,1,StackOut2).
stack(1,Stack),stackLogin(Stack,"Felipe","Clave3",StackOut),accept(StackOut,1,1,StackOut2).
stack(2,Stack),stackLogin(Stack,"Kamila","Clave2",StackOut),accept(StackOut,5,3,StackOut2).

STACK TO STRING
stack(1,Stack),stackToString(Stack,Str).
stack(2,Stack),stackToString(Stack,Str).
stack(1,Stack),stackLogin(Stack,"Felipe","Clave3",Stack2),stackToString(Stack2,Str).

VOTE
EL PRIMER EJEMPLO ES EL DE EL AUTOR A SU PROPIA PREGUNTA SEGUN ENUNCIADO SOLO ESTO ES VALIDO 
stack(1,Stack),stackLogin(Stack,"Jorge","Clave1",StackOut),getQuestion(StackOut,4,Preg),vote(StackOut,Preg,27832,false,StackOut2). %27832 no es nada para cuando se quiere votar a pregunta.
stack(1,Stack),stackLogin(Stack,"Felipe","Clave3",StackOut),getAnswer(StackOut,1,1,Res),vote(StackOut,Res,1,true,StackOut2).
stack(2,Stack),stackLogin(Stack,"Jorge","Clave1",StackOut),getAnswer(StackOut,4,2,Res),vote(StackOut,Res,4,true,StackOut2).

*/
