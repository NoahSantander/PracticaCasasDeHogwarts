% Parte 1. Sombrero Seleccionador
alumno(harry, mestiza, [corajudo, amistoso, orgulloso, inteligente], slytherin).
alumno(draco, pura, [orgulloso, inteligente], hufflepuff).
alumno(hermione, impura, [responsable, orgulloso, inteligente], _).

casa(gryffindor).
casa(slytherin).
casa(hufflepuff).
casa(ravenclaw).

% masApropiada/2 saber casas apropiadas para un alumno en base a sus caracteristicas
masApropiada(gryffindor, Alumno):-
    alumno(Alumno, _, Caracteristicas,_),
    member(corajudo, Caracteristicas).

masApropiada(slytherin, Alumno):-
    alumno(Alumno, _, Caracteristicas,_),
    member(orgulloso, Caracteristicas),
    member(inteligente, Caracteristicas).

masApropiada(ravenclaw, Alumno):-
    alumno(Alumno, _, Caracteristicas,_),
    member(inteligente, Caracteristicas), 
    member(responsable,Caracteristicas).

masApropiada(hufflepuff, Alumno):-
    alumno(Alumno, _, Caracteristicas,_),
    member(amistoso, Caracteristicas).

% 1
% noEsSangreImpura/1 saber si un mago no es sangre impura
noEsSangreImpura(Alumno):-    
    alumno(Alumno, Sangre,_,_),
    Sangre \= impura.

% permiteEntrar/2 saber si un alumno puede entrar a una casa
permiteEntrar(Alumno, slytherin):-
    noEsSangreImpura(Alumno).

permiteEntrar(Alumno, Casa):-
    casa(Casa),
    alumno(Alumno, _,_,_),
    Casa \= slytherin.
% 2
% tieneCaracterApropiado/2 saber si un alumno tiene caracter apropiado para una casa
tieneCaracterApropiado(Alumno, Casa):-
    masApropiada(Casa, Alumno).

% 3
% podriaQuedarSeleccionado/2 relaciona al alumno con la casa a la que podria quedar seleccionado
podriaQuedarSeleccionado(Alumno, Casa):-
    tieneCaracterApropiado(Alumno, Casa),
    permiteEntrar(Alumno, Casa),
    alumno(Alumno, _,_, CasaQueNoLeGusta),
    Casa \= CasaQueNoLeGusta.

podriaQuedarSeleccionado(hermione, gryffindor).

% 4
% esAmistoso/1 dice si un mahgo es amistoso
esAmistoso(Alumno):-
    alumno(Alumno, _, Caracteristicas,_),
    member(amistoso, Caracteristicas).

% cadenaDeAmistades/1 para una lista de magos si todos ellos se caracterizan por ser amistosos y cada uno podría estar en la misma casa que el siguiente
cadenaDeAmistades([Amigo]):-
    esAmistoso(Amigo).

cadenaDeAmistades([Amigo|Amigos]):-
    esAmistoso(Amigo),
    podriaQuedarSeleccionado(Amigo, Casa),
    nth0(0, Amigos, AmigoSiguiente),
    podriaQuedarSeleccionado(AmigoSiguiente, Casa),
    cadenaDeAmistades(Amigos).

% Parte 2. La Copa de las Casas
% esDe/2 que relaciona a la persona con su casa
esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

% lugarProhibido/2 relaciona el lugar con los puntos
lugarProhibido(bosque, -50).
lugarProhibido(seccionProhibidaBiblioteca, -10).
lugarProhibido(tercerPiso, -75).
pregunta(dondeSeEncuentraBezoar, 20, snape).
pregunta(comoHacerLevitarPluma, 25, flitwick).

% accion/2 relaciona la mala accion con los puntos
accion(fueraDeLaCamaNoche, -50).
accion(lugarProhibido(MalaAccion, _), Puntos):-
    lugarProhibido(MalaAccion, Puntos).

accion(respondioPregunta(Pregunta), Puntos):-
    pregunta(Pregunta, Dificultad, snape),
    Puntos is Dificultad/2.

accion(respondioPregunta(Pregunta), Puntos):-
    pregunta(Pregunta, Puntos, Profesor),
    Profesor \= snape.

accion(derrotarVoldemort, 60).
accion(ganarAjedrezMagico, 50).
accion(salvarAmigos, 50).

% realizoAccion/2 relaciona el mago con la accion que realizo
realizoAccion(hermione, respondioPregunta(dondeSeEncuentraBezoar)).
realizoAccion(hermione, respondioPregunta(comoHacerLevitarPluma)).
realizoAccion(harry, fueraDeLaCamaNoche).
realizoAccion(hermione, lugarProhibido(seccionProhibidaBiblioteca, _)).
realizoAccion(hermione, lugarProhibido(tercerPiso, _)).
realizoAccion(harry, lugarProhibido(bosque, _)).
realizoAccion(harry, lugarProhibido(tercerPiso, _)).
realizoAccion(draco, mazmorras).
realizoAccion(ron, ganarAjedrezMagico).
realizoAccion(hermione, salvarAmigos).
realizoAccion(harry, derrotarVoldemort).

% 1.a
esMalaAccion(Accion):-
    accion(Accion, Puntos),
    Puntos < 0.

esBuenAlumno(Alumno):-
    realizoAccion(Alumno, _),
    forall(realizoAccion(Alumno, Accion), not(esMalaAccion(Accion))).

% 1.b
accionRecurrente(Accion):-
    realizoAccion(Alumno1, Accion),
    realizoAccion(Alumno2, Accion),
    Alumno1 \= Alumno2.

% 2
obtenerPuntajePorAccion(Alumno, Puntaje):-
    realizoAccion(Alumno, Accion),
    accion(Accion, Puntaje).

puntajeAlumno(Casa, PuntajeAlumno):-
    esDe(Alumno, Casa),
    findall(Puntos, obtenerPuntajePorAccion(Alumno, Puntos), ListaPuntos),
    sumlist(ListaPuntos, PuntajeAlumno).

puntajeDeLaCasa(Casa, Puntaje):-
    casa(Casa),
    findall(PuntajeAlumno, puntajeAlumno(Casa, PuntajeAlumno), ListaPuntajeAlumnos),
    sumlist(ListaPuntajeAlumnos, Puntaje).

% 3
ganadorCopa(Ganador1, Ganador2):-
    puntajeDeLaCasa(Ganador1, Puntaje1),
    puntajeDeLaCasa(Ganador2, Puntaje2),
    Puntaje1 > Puntaje2.

casaGanadora(Casa):-
    casa(Casa),
    not(ganadorCopa(_, Casa)).
