% Main entry point for the game
main :- wumpus_world.

% Introduction and initial setup for the game
wumpus_world :-
    write('Welcome to the World of Wumpus!'), nl,
    write('Your goal is to slay the Wumpus'), nl,
    write('To accomplish this, you must first find a bow and arrow'), nl,
    whelp, 
    write('Type "whelp." to see the command list again'), nl,
    write('Type "quit." to leave the game'), nl, nl,
    look,
    command_loop.

% Help command that lists possible actions
whelp :-
    write('Use simple English sentences to enter commands.'), nl,
    write('The actions you can take are:'), nl, nl,
    write('   look around           (ex. look)'), nl,
    write('   inventory your things (ex. inventory)'), nl,
    write('   talk to someone       (ex. talk to the man)'), nl,
    write('   go to a location      (ex. go to the town)'), nl,
    write('   fight a monster       (ex. fight the troll)'), nl,
    write('   buy an item           (ex. buy the sword)'), nl,
    write('   sell an item          (ex. sell the boat)'), nl,
    write('   take an item          (ex. take the matches)'), nl,
    write('   ride                  (ex. ride the boat)'), nl,
    write('   light an item         (ex. light the torch)'), nl,
    write('   quit                  (ex. quit)'), nl, nl,
    write('The examples are verbose; terser commands and synonyms'), nl,
    write('are usually accepted.'), nl, nl.

% Command loop to process user commands
command_loop :-
    repeat,
    get_command(X),
    do(X),
    (wumpusdead; X == quit).

% Handle player actions based on commands
do(whelp) :- whelp,!.
do(look) :- look,!.
do(inventory) :- inventory,!.
do(talk(X)) :- talkto(X),!.
do(go(X)) :- goto(X),!.
do(fight(X)) :- fight(X),!.
do(buy(X)) :- buy(X),!.
do(sell(X)) :- sell(X),!.
do(take(X)) :- take(X),!.
do(ride(X)) :- ride(X),!.
do(light(X)) :- light(X),!.
do(quit) :- quit,!.
do(_) :- write('I dont understand, try again or type whelp.'), nl, !.

% Winning condition when the Wumpus is dead
wumpusdead :-
    dead(wumpus),
    write('Congratulations, you defeated the Wumpus.'), nl,
    write('Now the realm is safe from its stench.'), nl, nl.

% Quit the game
quit :-
    write('The lands live on without a hero'), nl, nl.

% Define paths, NPCs, enemies, locations and some other stuff
path(river, cave).
path(forest, woods).
path(cave, sinkhole).
path(woods, town).
path(woods, prairie).
path(sinkhole, prairie).
path(canyon, prairie).
path(prairie, foothills).
path(dock, town).
path(town, foothills).
path(town, shop).
path(foothills, mountain).

riverPath(river, dock).
mount(boat).

npc(archer).
npc(fisherman).
npc(blacksmith).

enemy(wumpus).
enemy(troll).
:- dynamic dead/1.

:- dynamic location/2.
location(boat, river).
location(diamond, river).
location(wumpus, forest).
location(hay, woods).
location(archer, canyon).
location(fisherman, dock).
location(matches, foothills).
location(blacksmith, shop).
location(troll, mountain).
location(arrow, mountain).

:- dynamic protected/1.
protected(arrow).

:- dynamic here/1.
here(prairie).

defeats(sword, troll).
defeats(bow, wumpus) :-
    have(arrows).

:- dynamic lit/1.

connect(X, Y) :- 
    path(X, Y).
connect(X, Y) :- 
    path(Y, X).

% Look command to display current location and available actions
look :-
    here(Here),
    respond(['You are in the ', Here]),
    write('You can see the following things:'), nl,
    list_things(Here),
    write('You can go to the following areas:'), nl,
    list_connections(Here).

% List items at the current location
list_things(Place) :-
    location(X, Place),
    tab(2), write(X), nl,
    fail.
list_things(_).

% List connections (paths) from the current location. Special case for cave
list_connections(Place) :-
    
    connect(Place, X),
    tab(2), write(X), nl,
    fail.
list_connections(_).

% Take an item from the current location
take(Thing) :-
    here(Place),
    is_here(Thing),
    is_takable(Thing),
    asserta(have(Thing)),
    retract(location(Thing, Place)),
    respond(['You now have the ', Thing]).

is_here(Thing) :-
    here(Here),
    location(Thing, Here),
    !.
is_here(Thing) :-
    respond(['There is no ', Thing, ' here']),
    fail.

is_takable(Thing):-
    npc(Thing),
    respond(['You cant pick up the ',Thing]),
    !,fail.
is_takable(Thing):-
    enemy(Thing),
    respond(['You cant pick up the ',Thing]),
    !,fail.
is_takable(Thing):-
    mount(Thing),
    respond(['You cant pick up the ',Thing]),
    !,fail.
is_takable(Thing):-
    protected(Thing),
    respond(['Your path to the ', Thing, ' is blocked']),
    !,fail.
is_takable(_). 

% Display the player's inventory
inventory:-
  have(_),                         % make sure you have at least one thing
  write('You have: '),nl,
  list_possessions.
inventory:-
  write('You have nothing'),nl.

list_possessions:-
  have(X),
  tab(2),write(X),nl,
  fail.
list_possessions.

:- dynamic have/1.

% Talk to an NPC
talkto(NPC) :-
    npc(NPC),
    here(Here),
    location(NPC, Here),
    dialogue(NPC),!.
talkto(NPC) :-
    respond(['There is no ', NPC, ' here.']), nl.

dialogue(archer) :-
    respond(['Ill trade you my bow for a diamond, if you have one']).
dialogue(fisherman) :-
    respond(['Ill buy that boat from you for a pretty penny']).
dialogue(blacksmith) :-
    respond(['I can make you a sword if youve got the coin']).
dialogue(_).

% Go to a different location
goto(Locale):-
  can_go(Locale),                 % check for legal move
  puzzle(goto(Locale)),           % check for special conditions
  moveto(Locale),                 % go there and tell the player
  look.
goto(_):- look.

can_go(Locale):-                  % if there is a connection it 
  here(Here),                   % is a legal move.
  connect(Here,Locale),!.
can_go(Locale):-
  respond(['You cant get to ',Locale,' from here']),fail.

moveto(Locale):-                  % update the logicbase with the
  retract(here(_)),             % new room
  asserta(here(Locale)).

puzzle(goto(river)):-
    here(cave),
    lit(hay),!.
puzzle(goto(river)):-
    here(cave),
    write('Its too dark to see anything in the cave'),nl,
    !,fail.
puzzle(_).

% Light an item. I made too many cases for this one. I just thought seeing 'you cant light the matches' was confusing
light(hay) :-
    lit(hay),
    respond(['The hay has already been set alight']), nl,
    !.
light(hay) :-
    have(hay),
    have(matches),
    here(cave),
    asserta(lit(hay)),
    retract(have(hay)),
    respond(['You light the hay, banishing the darkness']), nl,
    !.
light(hay) :-
    have(hay),
    have(matches),
    respond(['There is no need to light the hay here']), nl,
    !.
light(matches) :-
    have(matches),
    here(cave),
    respond(['The matches alone dont produce enough light']), nl,
    !.
light(matches) :-
    have(matches),
    respond(['There is no need to light the matches here']), nl,
    !.
light(X) :-
    have(X),
    respond(['You cant light ', X]), nl,
    !.
light(X) :-
    respond(['You dont have ', X]), nl.

% Fight command implementation
fight(troll) :-
    here(mountain),
    have(sword),
    respond(['You fight the troll with your new sword']),
    succeed_fight(troll),  % Call the function to handle fight outcome
    !.
fight(wumpus) :-
    here(forest),
    have(bow),
    have(arrow),
    respond(['You engage in epic battle against the Wumpus with your bow and arrow']),
    succeed_fight(wumpus),  % Call the function to handle fight outcome
    !.
fight(Enemy) :-
    enemy(Enemy),
    respond(['There is no ', Enemy, ' here or you dont have a weapon.']), nl.

% Handle the outcome of the fight
succeed_fight(wumpus) :-
    respond(['You have defeated the Wumpus!']),
    retract(location(wumpus, _)),
    asserta(dead(wumpus)),
    !.
succeed_fight(troll) :-
    respond(['You have defeated the Troll!']),
    retract(location(troll, _)),
    retract(protected(arrow)),
    asserta(dead(troll)),
    !.

buy(sword) :-
    here(shop),
    have(money),
    retract(have(money)),
    asserta(have(sword)),
    respond(['You have purchased a sword']).
buy(bow) :-
    here(canyon),
    have(diamond),
    retract(have(diamond)),
    asserta(have(bow)),
    respond(['You have exchanged the diamond for a bow']).
buy(Item) :-
    respond(['You cant buy the ', Item, ' right now']), nl, !.

sell(boat) :-
    here(dock),
    location(boat, dock),
    retract(location(boat, dock)),
    asserta(have(money)),
    respond(['You have sold the boat for some money']).
sell(diamond) :-
    here(canyon),
    have(diamond),
    retract(have(diamond)),
    asserta(have(bow)),
    respond(['You have exchanged the diamond for a bow']).
sell(Item) :-
    have(Item),
    respond(['You cant sell your ', Item, ' right now']), nl, !.
sell(Item) :-
    respond(['You dont have a ', Item]), nl, !.

% Riding a transportation item
ride(boat) :-
    here(Place),
    location(boat, Place),
    riverPath(Place, X),
    moveto(X),
    retract(location(boat, Place)),
    asserta(location(boat, X)),
    respond(['You ride the boat to the ', X]),
    !.
ride(Item) :-
    respond(['You cant ride the ', Item]), nl.

% respond simplifies writing a mixture of literals and variables

respond([]):-
  write('.'),nl,nl.
respond([H|T]):-
  write(H),
  respond(T).

% simple natural language processing of the commands.
get_command(C) :- 
      	read_line_to_string(user_input, String), %reads a string from the console
        tokenize_atom(String, L), %converts the string into a list
       	command(C,L), !. %converts the list into a predicate and arguments
get_command(_) :- 
       	write('I dont understand, try again or type help'), nl, fail.

% The =.. command converts a list to a predicate and arguments (also works in reverse).
% Basically we are taking a string of words (like "go to the office.") and converting it
% into a predicate and argument (like "go(office).")
command(C,L) :- elim(NL,L), C =..  NL, !.

% The elim predicate takes out words such as "to" and "the" and the period (".") from a list.
% We don't need them to process commands, we only need the verb(V) and the noun(N).
elim(NL, [V,on,the,N,.]) :- NL = [V,N], !.
elim(NL, [V,in,the,N,.]) :- NL = [V,N], !.
elim(NL, [V,to,the,N,.]) :- NL = [V,N], !.
elim(NL, [V,to,N,.]) :- NL = [V,N], !.
elim(NL,[V,the,N,.]) :- NL = [V,N], !.
elim(NL, [V,.]) :- NL = [V], !.
