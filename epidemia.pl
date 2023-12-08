:- dynamic person/4. % person(ID, STATE, COORDX, COORDY)

% STATE (healty/infected/cured)

% The size of the matrix
matrix_x(150).
matrix_y(150).

% The distante the people move every time 
movement_distance(15).

% The distance where the people gets possibly ill
infection_distance(5).

% The probability to get infected (min = 0.1 and max = 1)
infection_probability(0.5).

% Delete people
delete_people :-
  retractall(person(_, _, _, _)).

% To get the list of all the people
get_people_list(N_PEOPLE) :-
  findall((ID, STATE, COORDX, COORDY), person(ID, STATE, COORDX, COORDY), N_PEOPLE).

% To get the number of people (length of the list)
get_number_people(PEOPLE) :-
  get_people_list(N_PEOPLE),
  length(N_PEOPLE, PEOPLE).

% To show the list of the people
show_people_list([]).
show_people_list([(ID, STATE, COORDX, COORDY)|Y]) :-
  write("Person : "), write(ID), write(" State: "), write(STATE), write(" Coord ("), write(COORDX), write(", "), write(COORDY), write(")"), nl,
  show_people_list(Y).

% To write the people on screen
write_people :-
  get_people_list(PEOPLE),  % To get a list of all the people(ID, STATE, COORDX, COORDY)
  show_people_list(PEOPLE). % To show that list

% To create random coordenates (X and Y the number created, MAX_X and MAX_Y the highest random number)
random_coordenates(X, Y, MAX_X, MAX_Y) :-
  random_between(0, MAX_X, X),
  random_between(0, MAX_Y, Y).

% To add the number of people the user inserted. It is a loop, starting the COUNTER by 0
adding_persons(COUNTER, COUNTER).
adding_persons(N_PEOPLE, COUNTER) :- 
  COUNTER < N_PEOPLE,
  matrix_x(MAX_X),
  matrix_y(MAX_Y),
  random_coordenates(X, Y, MAX_X, MAX_Y),             % To create random coordenates
  add_person(COUNTER, healthy, X, Y),   % Add one person healthy    
  N_COUNTER is COUNTER + 1,
  adding_persons(N_PEOPLE, N_COUNTER).  % Recursive call
  
% To add people dynamically
add_person(ID, STATE, COORDX, COORDY) :-
  assert(person(ID, STATE, COORDX, COORDY)).

% To modify a person created by its id
modify_person(ID, STATE, COORDX, COORDY) :-
  retract(person(ID, _, _, _)),
  assert(person(ID, STATE, COORDX, COORDY)).


% To get which 'overflow' (less than 0, or more than the matrix size)  0 is correct, 1 is < 0 and 2 is > MAX
is_overflow(COORD, MAX_COORD, FLOW) :-
  ((COORD < 0, FLOW is 1);
  (COORD > MAX_COORD, FLOW is 2);
  (FLOW is 0)).

% Fix the coordenate corresponding to the flow
fix_coordenate(COORD, FLOW, FIX_COORD) :-
  movement_distance(DIST),
  ((FLOW = 0, FIX_COORD is COORD);
  (FLOW = 1, FIX_COORD is COORD + DIST + DIST);
  (FLOW = 2, FIX_COORD is COORD - DIST - DIST)).

% Check if the coordenates are correct and fixes if it is not the case
check_coordenates(AUX_X, AUX_Y, NEW_AUX_X, NEW_AUX_Y) :-
  matrix_x(MAX_X),
  matrix_y(MAX_Y),
  is_overflow(AUX_X, MAX_X, FLOW1),
  is_overflow(AUX_Y, MAX_Y, FLOW2),
  fix_coordenate(AUX_X, FLOW1, NEW_AUX_X),
  fix_coordenate(AUX_Y, FLOW2, NEW_AUX_Y).
  
% To get the new coordenates
new_coordenates(NEW_X, NEW_Y, X, Y, COORDX, COORDY) :-
  random(OP1),                  % Random number 0 or 1
  OPERATION1 is floor(OP1 * 2), % To get the int        
  random(OP2),
  OPERATION2 is floor(OP2 * 2),
  ((OPERATION1 = 0, OPERATION2 = 0, AUX_X is X + COORDX, AUX_Y is Y + COORDY);
  (OPERATION1 = 1, OPERATION2 = 1, AUX_X is COORDX - X, AUX_Y is COORDY - Y);
  (OPERATION1 = 0, OPERATION2 = 1, AUX_X is X + COORDX, AUX_Y is COORDY - Y);
  (OPERATION1 = 1, OPERATION2 = 0, AUX_X is COORDX - X, AUX_Y is Y + COORDY)),
  check_coordenates(AUX_X, AUX_Y, NEW_X, NEW_Y).
  

% To move a person individually
move_person([]).
move_person([(ID, STATE, COORDX, COORDY)|Z]) :-
  movement_distance(MAX_X),
  movement_distance(MAX_Y),
  random_coordenates(X, Y, MAX_X, MAX_Y),
  new_coordenates(NEW_X, NEW_Y, X, Y, COORDX, COORDY),
  modify_person(ID, STATE, NEW_X, NEW_Y),   % Modify the coordenates of its person by the id
  move_person(Z).

% To move randomly all the people in the matrix  (HACER EL MOVIMIENTO TANTO POSITIVO COMO NEGATIVO)
move_people :-
  get_people_list(PEOPLE),
  move_person(PEOPLE).

% To calculate the distance between two coordenates
euclidean_distance(COORDX, COORDY, COORDX2, COORDY2, RESULT) :-
  DELTAX is COORDX2 - COORDX,
  DELTAY is COORDY2 - COORDY,
  RESULT is sqrt(DELTAX * DELTAX + DELTAY * DELTAY).

% To know it the person will get ill or not
% If the infection probability is 0.5, we multiply by 10 and then we do the difference by 10
% Then generate a random number between that value, and if the value is equal to the result we had, it will get infected(1)
% Instead, will not get infected(0)
% Example : 0.5 * 10 = 5  |  10 - 5 = 5 | random_between(0,5, RES) | RES = 5, infected | RES != 5, no infected
random_infection(EUCL_DISTANCE, INFECT) :-
  infection_distance(INFECT_DISTANCE),
  infection_probability(INFECT_PROB),
  ((EUCL_DISTANCE > INFECT_DISTANCE, INFECT = 0);
  (EUCL_DISTANCE <= INFECT_DISTANCE, INFECT_PROB = 1, INFECT = 1);
  (EUCL_DISTANCE <= INFECT_DISTANCE,
  AUX1 is INFECT_PROB * 10,
  AUX2 is AUX1 - 10,
  random_between(1, AUX2, RES),
  ((AUX2 = RES, INFECT = 1);(INFECT = 0))  
  )
  )


% To check the distance between the people
check_distance2((_,_,_,_),[]).
check_distance2((ID, STATE, COORDX, COORDY),[(ID2, STATE2, COORDX2, COORDY2)|Z]) :-
  ((STATE = cured);
  (STATE = infected);                                                    % if the first person is infected, just finalise
  (ID = ID2, check_distance2((ID, STATE, COORDX, COORDY), Z));            % if it is the same id, go to the next one
  (STATE2 \= infected, check_distance2((ID, STATE, COORDX, COORDY), Z));  % if the person is not infected, go to the next one
  (euclidean_distance(COORDX, COORDY, COORDX2, COORDY2, EUCL_DIST),
    write("Distance : "), write(EUCL_DIST), nl,
    check_distance2((ID, STATE, COORDX, COORDY), Z),
    %infect_person()
  )

  ).
  %check_distance2()


% To check the distance between the people
check_distance([]).
check_distance([X|Z]) :-
  get_people_list(PEOPLE),
  check_distance2(X, PEOPLE),
  check_distance(Z).

%infect_person(ID, )

% To spread the disease among the people
infect_people :-
  get_people_list(PEOPLE),
  check_distance(PEOPLE).


% To start all the movement of the people, the spread of the epidemia...
%epidemia_simulation :-



% To start simulation
start :-
  write("Welcome to an epidemiology simulation. Please insert how many people do you want in your simulation"), nl,
  read(N_PEOPLE),
  adding_persons(N_PEOPLE, 0),
  get_number_people(PEOPLE),
  write("Okey, you just added : "), write(PEOPLE), nl,
  write_people,
  move_people, %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  write("The people : "), nl,
  write_people,
  infect_people,
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