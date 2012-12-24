% Clue Helper (Project 2)
% by Cate Nestor
% University of British Columbia
% CPSC 312 Term 1 Winter 2012


start :- catch(start2,stopped,(nl,write('You have terminated the program.'),nl)).
start2 :- catch(start3,notimplemented,(nl,write('Sorry, the program reached a point that has not been implemented yet.'),nl)).
start3 :- catch(start4,gameover,(nl,write('Game over!'),nl)).
start4 :- initialize,play(player1).

% GAME SETUP.
initialize :- info,getnumber,getplayers,getmycards,!,write('Okay, let\'s start playing.'),nl.


info :- write('Welcome to Clue Helper by Cate Nestor and Nick Zhu.'),nl,nl,
		write('Please type all answers to prompts in lowercase.'),nl,
		write('You may type "stop." at any time to terminate the helper.'),nl,
		write('This program recognizes suspects by their last name only: for example, type Professor Plum as "plum".'),nl,
		write('Rooms are recognized as one word: for example, type Dining Room as "diningroom".'),nl,nl.

myread(X) :- read(X),check(X).
check(X) :- X=stop,throw(stopped).
check(X).
implement_me(X) :- throw(notimplemented).

getnumber :- write('How many people are playing this game of Clue?'),nl,
			myread(_N),validplayers(_N),assert(players(_N));
			write('Sorry, valid player numbers are typed as integers and between 2 and 6. Please try again.'),nl,getnumber.

getplayers :- write('What is the name of the first player?'),nl,getp1,
			  write('What is the name of the second player?'),nl,getp2,(players(2) -> getmainplayer; getplayer3).
getplayer3 :- write('What is the name of the third player?'),nl,getp3,(players(3) -> getmainplayer; getplayer4).
getplayer4 :- write('What is the name of the fourth player?'),nl,getp4,(players(4) -> getmainplayer; getplayer5).
getplayer5 :- write('What is the name of the fifth player?'),nl,getp5,(players(5) -> getmainplayer; getplayer6).
getplayer6 :- write('What is the name of the sixth player?'),nl,getp6,getmainplayer.

getp1 :- myread(_P1),assert(playername(player1,_P1)).
getp2 :- myread(_P2),assert(playername(player2,_P2)).
getp3 :- myread(_P3),assert(playername(player3,_P3)).
getp4 :- myread(_P4),assert(playername(player4,_P4)).
getp5 :- myread(_P5),assert(playername(player5,_P5)).
getp6 :- myread(_P6),assert(playername(player6,_P6)).

getmainplayer :- write('What is the name of the player I will be assisting?'),nl,
				myread(_MP),player(X),playername(X,_MP),assert(mainplayer(X));
				write('Sorry, that is not a player you entered. Try again.'),nl,getmainplayer.

getmycards :- write('Please enter a card in your hand.'),nl,
			myread(_C),card(_C),!,assert(mycard(_C)),getnextcard;
			write('Sorry, that is not a valid card. Try again.'),nl,getmycards.

getnextcard :- write('Do you have any other cards?'),nl,
				myread(_A),yesorno(_A),!;
				write('Please answer "yes" or "no".'),nl,getnextcard.

yesorno(X) :- X = no,!.
yesorno(X) :- X = yes,!,getmycards.
suggestoraccuse(R,X) :- R = accuse,!,accuse(X).
suggestoraccuse(R,X) :- R = suggest,!,suggest(X).
skiporshow(R,X,S,Y) :- R = skip,!,next(Y,Z), assert(skipped(Y,S)), suggest_try(X,S,Z).
skiporshow(R,X,S,Y) :- R = show,!,assert(showed_card(Y,S)).
skiporshow2(R,S,Y) :- R = skip,!,next(Y,Z), assert(skipped(Y,S)), suggest_try(X,S,Z).
skiporshow2(R,S,Y) :- R = show,!,whatcard(Y).




%MAIN GAME PLAY

play(X) :- turn(X),next(X,Y),play(Y).

turn(X) :- not(mainplayer(X)),!,turn_other(X).
turn(X) :- mainplayer(X),!,turn_me(X).

turn_other(X) :- failedplayer(X),!.

turn_other(X) :- not(failedplayer(X)),!,playername(X,Name),write('It\'s '),write(Name),write('\'s turn. Did '),write(Name),write(' suggest or accuse?'),nl,
			myread(_A),suggestoraccuse(_A,X),!;
			write('Please type either "suggest" or "accuse".'),nl,turn_other(X).

suggest(X) :- write('Who was the suggested suspect?'),nl,myread(_S),suspect(_S),
			write('What was the suggested murder weapon?'),nl,myread(_W),weapon(_W),
			write('What was the suggested location of the crime?'),nl,myread(_R),room(_R),!,
			next(X,Y),
			suggest_try(X,[_S,_W,_R],Y);
			write('Something you entered was incorrect. Try again.'),nl,suggest(X).

suggest_try(X,S,X).
suggest_try(X,S,Y) :- X\=Y,not(mainplayer(Y)),!,suggest_try_other(X,S,Y).
suggest_try(X,S,Y) :- mainplayer(Y),!,suggest_try_me(X,S,Y).


suggest_try_other(X,S,Y) :- playername(Y,Name),write('Did '),write(Name),write(' skip or show (a card)?'),nl,
					myread(_A),skiporshow(_A,X,S,Y),!;
					write('Please type either "skip" or "show".'),nl,suggest_try_other(X,S,Y).

suggest_try_me(X,[S,W,R],Y) :- write('It\'s your turn to disprove the suggestion. '),
					not(has_card(Y,S)),not(has_card(Y,W)),not(has_card(Y,R)),
					write('You have none of these cards, so skip to the next player.'),!,nl,next(Y,Z),suggest_try(X,S,Z);
					write('Please show one of these cards... '),write_cards(Y,[S,W,R]),nl.

write_cards(Y,[C]) :- not(has_card(Y,C)).
write_cards(Y,[C]) :- has_card(Y,C),write(C),write(' ').
write_cards(Y,[H|T]) :- has_card(Y,H),write(H),write(' '),write_cards(Y,T).
write_cards(Y,[H|T]) :- not(has_card(Y,H)),write_cards(Y,T).


turn_me(X) :- newmove([M,S,W,R]),write('It is your turn. You should '),write(M),
			write(' that '),write(S),write(' committed murder in the '),write(R),
			write(' with the '),write(W),write('.'),
			nl,turn_me2(X,M,[S,W,R]).

turn_me2(X,accuse,S) :- throw(gameover).
turn_me2(X,suggest,S) :- next(X,Y), suggest_try_me(S,Y).
		 
suggest_try_me(S,Y) :- mainplayer(Y),!.
suggest_try_me(S,Y) :- playername(Y,Name),write('Did '),write(Name),write(' skip or show (a card)?'),nl,
					myread(_A),skiporshow2(_A,S,Y);
					write('Please type either "skip" or "show".'),suggest_try(X,S,Y).

whatcard(Y) :- write('What card was shown to you?'),nl,
			myread(_C),card(_C),!,assert(showed_me_card(Y,_C));
			write('Sorry, that is not a valid card. Try again.'),nl,whatcard(Y).


accuse(X) :- write('Who was the accused suspect?'),nl,myread(_S),suspect(_S),
			write('What was the accused murder weapon?'),nl,myread(_W),weapon(_W),
			write('What was the accused location of the crime?'),nl,myread(_R),room(_R),!,
			accuse2(X,[_S,_W,_R]); 
			write('Something you entered was incorrect. Try again. '),nl,accuse(X).
			
accuse2(X,S) :- write('Was the accusation "t" (true) or "f" (false)?'),nl,
			myread(_A),torf(_A,X,S);
			write('Please either type "t" or "f"'),nl,accuse2(X,S).

torf(R,X,S) :- R=f,!,assert(failed_accusation(S)),assert(failedplayer(X)).
torf(R,X,S) :- R=t,!,throw(gameover).

gameover :- throw(gameover).


%DEFINE VALID CARDS

suspect(scarlett).
suspect(mustard).
suspect(white).
suspect(green).
suspect(peacock).
suspect(plum).

weapon(knife).
weapon(candlestick).
weapon(pistol).
weapon(rope).
weapon(bat).
weapon(ax).

room(kitchen).
room(patio).
room(spa).
room(theatre).
room(livingroom).
room(observatory).
room(hall).
room(guesthouse).
room(diningroom).

card(X) :- room(X).
card(X) :- suspect(X).
card(X) :- weapon(X).


%keep track of players
validplayers(2).
validplayers(3).
validplayers(4).
validplayers(5).
validplayers(6).

%players
next(player1,player2).
next(player2,player3) :- player(player3).
next(player2,player1) :- players(2).
next(player3,player4) :- player(player4).
next(player3,player1) :- players(3).
next(player4,player5) :- player(player5).
next(player4,player1) :- players(4).
next(player5,player6) :- player(player6).
next(player5,player1) :- players(5).
next(player6,player1) :- players(6).

player(player1).
player(player2).
player(player3) :- players(3).
player(player3) :- players(4).
player(player3) :- players(5).
player(player3) :- players(6).
player(player4) :- players(4).
player(player4) :- players(5).
player(player4) :- players(6).
player(player5) :- players(5).
player(player5) :- players(6).
player(player5) :- players(6).


:- dynamic players/1.
:- dynamic playername/2.
:- dynamic mainplayer/1.
:- dynamic failedplayer/1.


%SETS AND MOVES.

newmove([accuse,S,W,R]) :- accusation([S,W,R]),!.
newmove([suggest,S,W,R]) :- valid_murder_set([S,W,R]),not(has_one_of(P,[S,W,R])),good_suggestion([S,W,R]);valid_murder_set([S,W,R]),not(has_one_of(P,[S,W,R])).

good_suggestion(S) :- narrow_down(C),member(C,S).


murder_set([S,W,R]) :- suspect(S),weapon(W),room(R).
valid_murder_set([S,W,R]) :- murder_set([S,W,R]),not(failed_accusation([S,W,R])),valid(S),valid(W),valid(R).
accusation([S,W,R]) :- valid_murder_set([S,W,R]),valid(S),murder_suspect(S),valid(W),murder_weapon(W),valid(R),murder_room(R).



%INPUTS

:-dynamic mycard/1.
:-dynamic skipped/2.
:-dynamic showed_card/2.
:-dynamic showed_me_card/2.
:-dynamic failed_accusation/1.
failed_accusation(a,b,c).

%RULES

valid(X) :- card(X),not(known_card(X)).

lacks_card(P,C) :- player(P),card(C),X \= P,has_card(X,C).
lacks_card(P,C) :- player(P),card(C),skipped(P,S),member(C,S).

known_card(X) :- card(X),player(Y),has_card(Y,X).

murder_suspect(X) :- suspect(X),invalid_suspects([A,B,C,D,E]),not(member(X,[A,B,C,D,E])),diff([A,B,C,D,E]),!.
murder_suspect(X) :- suspect(X), all_lack_card(X).
murder_weapon(X) :- weapon(X), all_lack_card(X),!.
murder_weapon(X) :- weapon(X),invalid_weapons([A,B,C,D,E]),not(member(X,[A,B,C,D,E])),diff([A,B,C,D,E]),!.
murder_room(X) :- room(X),invalid_rooms([A,B,C,D,E,F,G,H]),not(member(X,[A,B,C,D,E,F,G,H])),diff([A,B,C,D,E,F,G,H]),!.
murder_room(X) :- room(X), all_lack_card(X).


invalid_weapons([X]) :- weapon(X),known_card(X).
invalid_weapons([X|Y]) :- weapon(X),known_card(X),invalid_weapons(Y),not(member(X,Y)).

invalid_suspects([X]) :- suspect(X),known_card(X).
invalid_suspects([X|Y]) :- suspect(X),known_card(X),invalid_suspects(Y),not(member(X,Y)).

invalid_rooms([X]) :- room(X),known_card(X).
invalid_rooms([X|Y]) :- room(X),known_card(X),invalid_rooms(Y),not(member(X,Y)).

has_card(P,C) :- mainplayer(P),mycard(C).
has_card(P,C) :- showed_me_card(P,C).
has_card(P,C) :- player(P),card(C),has_one_of(P,[C,W,R]),lacks_card(P,W),lacks_card(P,R).
has_card(P,C) :- player(P),card(C),has_one_of(P,[S,C,R]),lacks_card(P,S),lacks_card(P,R).
has_card(P,C) :- player(P),card(C),has_one_of(P,[S,W,C]),lacks_card(P,S),lacks_card(P,W).

has_one_of(P,X) :- player(P),murder_set(X),showed_card(P,X).

narrow_down(C) :- has_one_of(P,X),member(C,X),member(D,X),C\=D,valid(C),known_card(D).

all_lack_card(C) :- not(all_do_not_lack_card(C)).
all_do_not_lack_card(C) :- player(P),not(lacks_card(P,C)).


% HELPER FUNCTIONS
member(X,[X|T]).
member(X,[H|T]) :- member(X,T).
diff([H]).
diff([H|T]) :- not(member(H,T)),diff(T).


:- write('----> !!! Enter "start" to begin the helper. !!! <----').


