:- dynamic person/2. % poner el dynamic de cuatro para las coordenadas

% para crear las coordenadas, hacerlo aleatoriamente de 0 al tamaño de la matriz(50 o 100, ya veré)
% añadir tambień las coordenadas de la gente a la hora de imprimir en pantalla las personas

% Delete people
delete_people :-
  retractall(person(_, _)).

% To get the list of id of the people
get_people_list(N_PEOPLE) :-
  findall((ID, STATE), person(ID, STATE), N_PEOPLE).

% To get the number of people
get_number_people(PEOPLE) :-
  get_people_list(N_PEOPLE),
  length(N_PEOPLE, PEOPLE).

% To show the list of the people
show_people_list([]).
show_people_list([(ID, STATE)|Y]) :-
  write("Person : "), write(ID), write(" State: "), write(STATE), nl,
  show_people_list(Y).

% To write the people on screen
write_people :-
  get_people_list(PEOPLE),
  show_people_list(PEOPLE).

% To add the number of people the user inserted
adding_persons(COUNTER, COUNTER).
adding_persons(N_PEOPLE, COUNTER) :-
  COUNTER < N_PEOPLE,
  add_person(COUNTER, healthy),
  N_COUNTER is COUNTER + 1,
  adding_persons(N_PEOPLE, N_COUNTER).
  
% To add people dynamically
add_person(ID, STATE) :-
  assert(person(ID, STATE)).

% To start simulation
start :-
  write("Welcome to an epidemiology simulation. Please insert how many people do you want in your simulation"), nl,
  read(N_PEOPLE),
  adding_persons(N_PEOPLE, 0),
  get_number_people(PEOPLE),
  write("Okey, you just added : "), write(PEOPLE), nl,
  write("The people : "), nl,
  write_people,
  delete_people,
  write("SIMULATION FINISHED").


  % cuantas personas quieres añadir
  % pedir numero y llamar funcion para añadir
  % prueba de mostrar cantidad de gente










% Hecho dinámico para almacenar información sobre personas y su estado de salud
% :- dynamic persona/2.

% % Reglas para la simulación de infección epidemiológica
% transmite_enfermedad(X, Y) :-
%     persona(X, enfermo),
%     persona(Y, sano),
%     contacto(X, Y).

% contacto(X, Y) :- conoce(X, Y).
% contacto(X, Y) :- conoce(Y, X).
% contacto(X, Y) :- vive_en_misma_ciudad(X, Y).

% conoce(juan, maria).
% conoce(maria, pedro).
% conoce(pedro, ana).

% vive_en_misma_ciudad(juan, maria).
% vive_en_misma_ciudad(maria, pedro).
% vive_en_misma_ciudad(pedro, ana).

% % Reglas para propagar la enfermedad
% propagar_enfermedad :- 
%     transmite_enfermedad(X, Y),
%     assert(persona(Y, enfermo)),
%     write(Y), write(' ha sido infectado por '), write(X), nl,
%     fail.
% propagar_enfermedad.

% % Ejemplo de inicio con un individuo enfermo
% iniciar_simulacion :-
%     assert(persona(juan, enfermo)),
%     write('Comienza la simulación de la infección epidemiológica.'), nl,
%     propagar_enfermedad,
%     write('Fin de la simulación.'), nl,
%     retractall(persona(_, _)). % Limpiar hechos después de la simulación

% % Predicado para agregar una nueva conexión entre personas
% agregar_conexion(X, Y) :-
%     assert(conoce(X, Y)),
%     assert(conoce(Y, X)).

% % Predicado para eliminar una conexión entre personas
% eliminar_conexion(X, Y) :-
%     retract(conoce(X, Y)),
%     retract(conoce(Y, X)).