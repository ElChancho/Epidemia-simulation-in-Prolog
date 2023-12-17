:- dynamic person/5. % person(ID, STATE, COORDX, COORDY, TIME)

% STATE (healty/infected/cured)
%TIME : The number of the days the person is infected

% The size of the matrix
matrix_x(150).
matrix_y(150).

% The distante the people move every time 
movement_distance(15).

% The distance where the people gets possibly ill
infection_distance(20).

% The probability to get infected (min = 0.1 and max = 1)
infection_probability(1).

% The days a person is infected
infection_time(3).

% Delete people
delete_people :-
  retractall(person(_, _, _, _, _)).

% To get the list of all the people
get_people_list(N_PEOPLE) :-
  findall((ID, STATE, COORDX, COORDY, TIME), person(ID, STATE, COORDX, COORDY, TIME), N_PEOPLE).

% To get the list of the infected people
get_infected_people(N_PEOPLE) :-
  findall((ID, infected, COORDX, COORDY, TIME), person(ID, infected, COORDX, COORDY, TIME), N_PEOPLE).

% To get the list of the healthy people
get_healthy_people(N_PEOPLE) :-
  findall((ID, healthy, COORDX, COORDY, TIME), person(ID, healthy, COORDX, COORDY, TIME), N_PEOPLE).

% To get the list of the cured people
get_cured_people(N_PEOPLE) :-
  findall((ID, cured, COORDX, COORDY, TIME), person(ID, cured, COORDX, COORDY, TIME), N_PEOPLE).

% To get the number of people (length of the list)
get_number_people(PEOPLE) :-
  get_people_list(N_PEOPLE),
  length(N_PEOPLE, PEOPLE).

% To show the list of the people
show_people_list([]).
show_people_list([(ID, STATE, COORDX, COORDY, TIME)|Y]) :-
  write("Person : "), write(ID), write(" State: "), write(STATE), write(" Coord ("), write(COORDX), write(", "), write(COORDY), write(")"), write(" Time : "), write(TIME), nl,
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
  infection_time(TIME),
  random_coordenates(X, Y, MAX_X, MAX_Y),             % To create random coordenates
  ((COUNTER = 0, add_person(COUNTER, infected, X, Y, TIME));  % The first person introduced, is infected
  (add_person(COUNTER, healthy, X, Y, 0))                 % Add one person healthy
  ),
  N_COUNTER is COUNTER + 1,
  adding_persons(N_PEOPLE, N_COUNTER).  % Recursive call
  
% To add people dynamically
add_person(ID, STATE, COORDX, COORDY, TIME) :-
  assert(person(ID, STATE, COORDX, COORDY, TIME)).

% To modify a person created by its id
modify_person(ID, STATE, COORDX, COORDY, TIME) :-
  retract(person(ID, _, _, _, _)),
  assert(person(ID, STATE, COORDX, COORDY, TIME)).


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
move_person([(ID, STATE, COORDX, COORDY, TIME)|Z]) :-
  movement_distance(MAX_X),
  movement_distance(MAX_Y),
  random_coordenates(X, Y, MAX_X, MAX_Y),
  new_coordenates(NEW_X, NEW_Y, X, Y, COORDX, COORDY),
  modify_person(ID, STATE, NEW_X, NEW_Y, TIME),   % Modify the coordenates of its person by the id
  move_person(Z).

% To move randomly all the people in the matrix  (HACER EL MOVIMIENTO TANTO POSITIVO COMO NEGATIVO)
move_people :-
  get_people_list(PEOPLE),
  move_person(PEOPLE).

% To calculate the distance between two coordenates
euclidean_distance(COORDX, COORDY, COORDX2, COORDY2, RESULT) :-
  %DELTAX is COORDX2 - COORDX,
  %DELTAY is COORDY2 - COORDY,
  %RESULT is sqrt(DELTAX * DELTAX + DELTAY * DELTAY).
  RESULT is sqrt((COORDX2 - COORDX) * (COORDX2 - COORDX) + (COORDY2 - COORDY) * (COORDY2 - COORDY)).

% To know it the person will get ill or not
% If the infection probability is 0.5, we multiply by 10 and then we do the difference by 10
% Then generate a random number between that value, and if the value is equal to the result we had, it will get infected(1)
% Instead, will not get infected(0)
% Example : 0.6 * 10 = 6  |  10 - 6 = 4 | random_between(1,4, RES) | RES = 4, infected | RES != 4, no infected
random_infection(EUCL_DISTANCE, INFECT) :-
  infection_distance(INFECT_DISTANCE),
  infection_probability(INFECT_PROB),
  
  %ROUND_INFECT_PROB is round(INFECT_PROB * 10) / 10,       %ESTO EN CASO DE QUE EL USUARIO PONGA LA PROBABILIDAD DE INFECCIÓN  
  ((EUCL_DISTANCE > INFECT_DISTANCE, INFECT is 0);
  (EUCL_DISTANCE =< INFECT_DISTANCE, INFECT_PROB = 1, INFECT is 1);
  (EUCL_DISTANCE =< INFECT_DISTANCE,
  %trace,
  AUX1 is 10 - (INFECT_PROB * 10),
  AUX2 is round(AUX1),
  random_between(1, AUX2, RES),
  ((AUX2 = RES, INFECT is 1);(INFECT is 0))  
  )
  ).


% To check the distance between the people
check_distance2((_,_,_,_,_),[]).
check_distance2((ID, STATE, COORDX, COORDY, _),[(_, _, COORDX2, COORDY2, _)|Z]) :-
  euclidean_distance(COORDX, COORDY, COORDX2, COORDY2, EUCL_DIST),
  random_infection(EUCL_DIST, INFECT),
  ((INFECT = 1, infection_time(TIME), modify_person(ID, infected, COORDX, COORDY, TIME));
  (INFECT = 0)),
  check_distance2((ID,STATE, COORDX, COORDY, _), Z).
  
  
  
  %(                                                   % if the first person is infected, just finalise
  %(ID = ID2, check_distance2((ID, STATE, COORDX, COORDY), Z));            % if it is the same id, go to the next one
  %(STATE2 \= infected, check_distance2((ID, STATE, COORDX, COORDY), Z));  % if the person is not infected, go to the next one
  %(euclidean_distance(COORDX, COORDY, COORDX2, COORDY2, EUCL_DIST),
    %write("Distance : "), write(EUCL_DIST), nl,
  %  random_infection(EUCL_DIST, INFECT),
  %  ((INFECT = 1, infection_time(TIME), modify_person(ID, infected, COORDX, COORDY, TIME)); (INFECT = 0)),     % CAMBIAR AQUI EL TIEMPO DE INFECCION
  %  check_distance2((ID, STATE, COORDX, COORDY, _), Z)
  %)

  %).
  


% To check the distance between the people
check_distance([], _).
check_distance([X|Z], I_PEOPLE) :-          % A LO MEJOR PASARLE POR UN LADO LA GENTE HEALTHY Y POR OTRO LA INFECTADA
  %get_people_list(PEOPLE),
  check_distance2(X, I_PEOPLE),
  check_distance(Z, I_PEOPLE).

% To spread the disease among the people
infect_people :-
  %trace,
  get_healthy_people(H_PEOPLE),
  get_infected_people(I_PEOPLE),
  check_distance(H_PEOPLE, I_PEOPLE).
  %get_people_list(PEOPLE),        %A LO MEJOR PASARLE POR UN LADO LA GENTE HEALTHY Y POR OTRO LA INFECTADA
  %check_distance(PEOPLE).

% To check the days of every infected people
check_day([]).
check_day([(ID, STATE, COORDX, COORDY, TIME)|Z]) :-
  ((TIME = 1, modify_person(ID, cured, COORDX, COORDY, 0));
  (NEW_TIME is TIME - 1, modify_person(ID, STATE, COORDX, COORDY, NEW_TIME))),
  check_day(Z).


% To reduce in one day the person that is infected
cure_people :-
  get_infected_people(INF_PEOPLE),
  check_day(INF_PEOPLE).

% To start all the movement of the people, the spread of the epidemia...
finalise_epidemia(N_PEOPLE, N_PEOPLE).
%epidemia_simulation(15, N_PEOPLE).
epidemia_simulation(DAY, N_PEOPLE) :-   %%% PARA AHORRAR, PASARLE POR PARÁMETRO EL NÚMERO TOTAL DE PERSONAS
  %trace, 
  move_people,
  cure_people,
  %write_people,
  %trace,
  infect_people,
  %write_people,
  get_infected_people(INF_PEOPLE),
  get_healthy_people(HEA_PEOPLE),
  get_cured_people(CUR_PEOPLE),
  length(HEA_PEOPLE, N_HEA_PEOPLE),
  length(INF_PEOPLE, N_INF_PEOPLE),
  length(CUR_PEOPLE, N_CUR_PEOPLE),
  sleep(1),
  write("---- DAY: "), write(DAY), write( "----"), nl,
  write("People infected: "), write(N_INF_PEOPLE), nl,
  write("People healthy: "), write(N_HEA_PEOPLE), nl,
  write("People cured: "), write(N_CUR_PEOPLE), nl, nl,
  NEW_DAY is DAY + 1,
 
  ((finalise_epidemia(N_PEOPLE, N_CUR_PEOPLE));         % If all the people is cured
  (N_INF_PEOPLE = 0);                                   % If there are no infected people
  (epidemia_simulation(NEW_DAY, N_PEOPLE))).            % else continues

% To start simulation
start :-
  write("Welcome to an epidemiology simulation. Please insert how many people do you want in your simulation"), nl,
  read(N_PEOPLE),
  adding_persons(N_PEOPLE, 0),
  get_number_people(PEOPLE),
  nl,
  write("Okey, you just added : "), write(PEOPLE), write(" people"), nl, nl,
  epidemia_simulation(1, N_PEOPLE),


  % PEDIR DISTANCIA DE INFECCIÓN, MOVIMIENTO DE LAS PERSONAS, POSIBILIDAD DE INFECCIÓN, TIEMPO DE INFECCIÓN, TAMAÑO DEL BLOQUE
  % PONER CIERTOS LIMITES, INDICARLO, SI SE PASA PONER EL PREDETERMINADO
  % TAMBIEN PONERLE EL "RECOMENDADO" O EL PREDETERMINADO DEL PROGRAMA



  %write_people,
  %move_people, %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %write("The people : "), nl,
  %write_people,
  %infect_people,
  %write_people,
  %get_infected_people(INF_PEOPLE),
  %show_people_list(INF_PEOPLE),
  %length(INF_PEOPLE, N_INF_PEOPLE),
  %write(N_INF_PEOPLE), nl,
  delete_people,
  write("SIMULATION FINISHED").

