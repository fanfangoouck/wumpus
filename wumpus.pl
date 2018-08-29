:- module(wumpus,[initialState/5, guess/3, updateState/4]).

initialState(NR, NC, XS, YS, State0) :-
    writeln('section initial '),
    State0 = sta((row-NR),(col-NC),current(XS/YS),initial(XS/YS),
               route([]),pos_guess([XS/YS]),pit([]),wall([]),w([]),move([]),no([]),h([])).


% nl, write('agent at: '), write(Pos0),
% guess(sta((row-5),(col-5),current(1/1),[h([])]),X,Y).
guess(State0, State, Guess) :-
  State0 = sta((row-NR),(col-NC),current(Pos0),initial(Posinitial),
            route(Route0),pos_guess(List0),pit(Pit),wall(Wall),w(W),move(Move0),no(No0),h(Hist0)),
  nl,write('section2 ,重新 在section2中传入了state'),
  nl,write('section5 显示传入的墙'-Wall),
  (pos_near_wall_foot(Pos0,State0,North,West,East,South),
    nl,write('看看选择中的section2-1满不满足条件呢'),
   guess_produce(Pos0,Hist0,Pit,North,West,East,South,Guess1),
   nl,write('section2-1,执行选择中的第一部分'),write(Guess1),
   Pos = Pos0,Action = [],
   shoot_hist(Pos,Guess1,Hist1),
   nl,write(Hist1),
   Route = Route0,
   Move = Move0,
   reverse(Route,Routerev),
   write('section2-3 ,Routerev'-Routerev),
   append(Routerev,Guess1,Guess),
   append(Move,Guess1,Guess),
   List = List0,
   No = No0
  ;pos_neighbour(Pos0,Pos,NR,NC),
   nl,write('section2-2,找邻居pos '),write(Pos),
   \+ memberchk(Pos-_,Hist0),
   \+ memberchk(Pos,Pit),
   \+ memberchk(Pos,Wall),
   \+ memberchk(Pos,W),
   %记得加这块
   %\+ memberchk(Pos,No)，
   append([Pos],List0,List),
   write('section2-2 ,生成List'),write(List),
   guess_action(Pos0,Pos,Action1),
   append(Move0,[Action1],Move),
   %和我们的guess 相反
   write('section2-2 ,生成Route'),write(Route),
   append([Action1],Route0,Route),
   pos_near_wall_foot(Pos,State0,North,West,East,South),
   guess_produce(Pos,Hist0,Pit,North,West,East,South,Guess1),
   shoot_hist(Pos,Guess1,Hist1),
   %这块还要加上之前的走过的，一直走到current，p0的
   reverse(Route,Routerev),
   append(Routerev,Guess1,Guess),
   write('section2-2 ,生成Guess'),write(Guess),
   % append([Action],Route0,Route),
   No = No0
  ;% Pos0  是输出
   writeln('section2-3 ,开始'),write(Route0-List0-Pit-Wall-W-No0-Hist0),
   retrace(NR,NC,Route0,List0,Pit,Wall,W,No0,Hist0,Poslast,Pos,Route1,List1,No1),
   write('section2-3 ,生成Poslast,Pos'),write(Poslast),write(Pos),
   append([Pos],List1,List),
   write('section2-3 ,生成List'-List),
   guess_action(Poslast,Pos,Action1),
   write('section2-3 ,生成List'-Action1),
   append([Action1],Route1,Route),
   write('section2-3 ,生成List'-Route),
   pos_near_wall_foot(Pos,State0,North,West,East,South),
   write('section2-3 ,生成东南西北'-North-West-East-South),
   guess_produce(Pos,Hist0,Pit,North,West,East,South,Guess1),
   write('section2-3 ,生成Guess1'-Guess1),
   shoot_hist(Pos,Guess1,Hist1),
   reverse(Route,Routerev),
   write('section2-3 ,Routerev'-Routerev),
   append(Routerev,Guess1,Guess),
  write(' 生成总guess'-Guess),
   % append([Action],Route0,Route),
   No = No1
  ),
  append([Hist1],Hist0,Hist),
  State = sta((row-NR),(col-NC),current(Pos),initial(Posinitial),route(Route),
          pos_guess(List),pit(Pit),wall(Wall),w(W),move(Move),no(No),h(Hist)).

%回溯，直到找到合适的pos 可以射击
retrace(NR,NC,Route0,List0,Pit,Wall,W,No0,Hist0,Poslast,Pos,Route,List,No):-
    nl,write(No0),
    List0 = [Listfirst|List1],
    nl,write(Listfirst),write(List1),
    append([Listfirst],No0,No1),
    nl,write('生成 No'),write(No1),
    Route0 = [_|Route1],
    nl,write('生成 Route1'),write(Route1),
     (List1 = [Poslast|_],
     nl,write('Poslast'),write(Poslast),
     pos_neighbour(Poslast,Pos,NR,NC),
     nl,write('找邻居'),write(Pos),
     \+ memberchk(Pos-_,Hist0),
     nl,write('符合不在历史中'),write(Hist0),
     \+ memberchk(Pos,Pit),
     nl,write('符合不在坑中'),
     \+ memberchk(Pos,Wall),
     nl,write('符合不在墙中'),
     \+ memberchk(Pos,W),
     nl,write('符合不在怪兽中'),
     %记得加这块``
     nl,write('No1是'-No1),
     nl,write('Pos是'-Pos),
     \+ memberchk(Pos,No1),
      nl,write('符合不在no中'),
     Route = Route1,
      nl,write('Route1'-Route1),
     List = List1,
      nl,write('List1'-List1),
      No = No1
    ;nl,write('调用了新的retrace'),retrace(NR,NC,Route1,List1,Pit,Wall,W,No1,Hist0,Poslast,Pos,Route,List,No)
    ).



  updateState(State0, Guess, Feedback, State):-
   nl,write('section3 ，开始updata State'),
    State0 = sta((row-NR),(col-NC),current(Pos),initial(Posinitial),route(Route),
            pos_guess(Guess0),pit(Pit0),wall(Wall0),w(W0),move(Move),no(No),h(Hist)),
    concatenation(Guess,Feedback,List),
    nl,write('section4,这次有没有遇见坑,墙 '),
    find_wall_pit_w(Posinitial,List,Wall1,Pit1,W1),
    nl,write(Wall1-Pit1-W1),
    append(Wall0,Wall1,Wall),
    append(Pit0,Pit1,Pit),
    append(W0,W1,W),
    write('section5 更新完了坑,墙'),
    nl,write('section5  显示墙'-Wall),
    State = sta((row-NR),(col-NC),current(Pos),initial(Posinitial),route(Route),
                pos_guess(Guess0),pit(Pit),wall(Wall),w(W),move(Move),no(No),h(Hist)).





% concat two lists
concatenation(Lista,[], Lista).
concatenation([ Elem1 | Lista1],[Elem2 |Lista2], [Elem1-Elem2 | Lista3]):-
    concatenation(Lista1, Lista2, Lista3).
/*
find_wall_pit_w(_,[shoot],_,_,_).
find_wall_pit_w(Pos,[Elem1-Elem2 | Lista],Wall,Pit,W):-
    find(Pos,Elem1-Elem2,Wall0,Pit0,W0),
    append(Wall0,Wall1,Wall),
*/

find_wall_pit_w(_,[X],[],[],[]):-
      memberchk(X, [shoot,north,south,west,east,shoot-miss]).
find_wall_pit_w(_,[X,_],[],[],[]):-
      memberchk(X, [shoot,north,south,west,east,shoot-miss]).
find_wall_pit_w(Pos0,[Elem1-Elem2 | Lista],Wall,Pit,W):-
 writeln([Elem1-Elem2 | Lista]),
 writeln(Pos0),
 writeln(Elem1-Elem2),
    find(Pos0,Elem1-Elem2,Pos,Wall0,Pit0,W0),% nl,write('添加新坑,qiang'-Wall0-Pit0-W0),
    % nl,write('section7 继续追溯找坑，墙 '),
    find_wall_pit_w(Pos,Lista,Wall1,Pit1,W1),
    append(Wall0,Wall1,Wall),
    append(Pit0,Pit1,Pit),
    append(W0,W1,W).

    find(Pos0,Elem1-empty,Pos,Wall0,Pit0,W0):-
        guess_next_position(Pos0,Pos,Elem1),
       Wall0 = [],Pit0 = [],W0 = [].

    find(Pos0,Elem1-wall,Pos,Wall0,Pit0,W0):-
            guess_next_position(Pos0,Pos1,Elem1),
            Wall0 = [Pos1],
            Pos = Pos0,
            Pit0 = [],W0 = [].

    find(Pos0,Elem1-pit,Pos,Wall0,Pit0,W0):-
            guess_next_position(Pos0,Pos1,Elem1),
            Pit0 = [Pos1],
            Pos = Pos0,
            Wall0 = [],W0 = [].

    find(Pos0,Elem1-wumpus,Pos,Wall0,Pit0,W0):-
            guess_next_position(Pos0,Pos1,Elem1),
            W0 = [Pos1],
            Pos = Pos0,
            Wall0 = [],
            Pit0 = [].

    find(Pos0,Elem1-_,Pos,Wall0,Pit0,W0):-
     guess_next_position(Pos0,Pos,Elem1),
     Wall0 = [],Pit0 = [],W0 = [].

  /*
    find(Pos0,Elem1-empty,Wall0,Pit0,W0，Pos):-
        guess_next_position(Pos0,Pos,Elem1).

    find(Pos0,Elem1-wall,Wall0,Pit0,W0，Pos):-
        guess_next_position(Pos0,Pos,Elem1).



  check_if(Guess0,Feedback,Pos,W0,Wall0,Pit0,W,Wall,Pit).

check_if([south,north,shoot],Feedback,Pos,W0,Wall0,Pit0,W,Wall,Pit):-
    check_north_shoot(Feedback,Pos,W0,Pit0,Wall0,W,Pit,Wall).

check_north_shoot([pit],Pos0,W0,Pit0,Wall0,W,Pit,Wall):-
    Pos0 = [Pos],
    guess_next_position(Pos,Pit1,south),
    Pit = [Pit1|Pit0],
    Wall = Wall0,
    W = W0.

check_north_shoot([wall,pit],Pos0,W0,Pit0,Wall0,W,Pit,Wall):-
    Pos0 = [Pos],
    guess_next_position(Pos,Wall1,south),
    Wall = [Wall1|Wall0],
    guess_next_position(Pos,Pit1,north),
    Pit = [Pit1|Pit0],
    W = W0.

check_north_shoot([wall,wall],Pos0,W0,Pit0,Wall0,W,Pit,Wall):-
  Pos0 = [Pos],
  guess_next_position(Pos,Wall1,south),
  Wall2 = [Wall1|Wall0],
  guess_next_position(Pos,Wall3,north),
  Wall = [Wall3|Wall2],
  Pit = Pit0,
  W = W0.

check_north_shoot([wall,wumpus],Pos0,W0,Pit0,Wall0,W,Pit,Wall):-
    Pos0 = [Pos],
    guess_next_position(Pos,Wall1,south),
    Wall = [Wall1|Wall0],
    guess_next_position(Pos,W1,north),
    W = [W1|W0],
    Pit = Pit0.

check_north_shoot([wall,empty,shoot],Pos0,W0,Pit0,Wall0,W,Pit,Wall):-
        Pos0 = [Pos],
        guess_next_position(Pos,Wall1,south),
        Wall = [Wall1|Wall0],
        guess_next_position(Pos,W1,north),
        W = [W1|W0],
        Pit = Pit0.

*/














shoot_hist(Pos,[south,north,shoot],Pos-north_shoot).
shoot_hist(Pos,[west,east,shoot],Pos-east_shoot).
shoot_hist(Pos,[east,west,shoot],Pos-west_shoot).
shoot_hist(Pos,[north,south,shoot],Pos-south_shoot).


%检查位置在不在坑里
pit_check(Pos,List):-
       memberchk(Pos,List).


%检查位置在不在墙上

%  加上在这里没有坑 两边
guess_produce(Pos0,Hist,Pit,North,West,East,South,Guess):-
(\+ memberchk(Pos0-north_shoot,Hist),North = 0 -> Guess = [south,north,shoot]
;\+ memberchk(Pos0-west_shoot,Hist),West = 0,
  guess_near_pit(Pos0,west,Pit) -> Guess = [east,west,shoot]
;\+ memberchk(Pos0-east_shoot,Hist),East = 0,
  guess_near_pit(Pos0,east,Pit) -> Guess = [west,east,shoot]
;\+ memberchk(Pos0-south_shoot,Hist),South = 0,
  guess_near_pit(Pos0,south,Pit)-> Guess = [north,south,shoot]
).

% in order to move to the next position, the action is

guess_near_pit(Pos,north,Pit):-
    guess_next_position(Pos,Posforward,south),
    \+ memberchk(Posforward,Pit).

guess_near_pit(Pos,west,Pit):-
    guess_next_position(Pos,Posforward,east),
    \+ memberchk(Posforward,Pit).

guess_near_pit(Pos,east,Pit):-
    guess_next_position(Pos,Posforward,west),
    \+ memberchk(Posforward,Pit).

guess_near_pit(Pos,south,Pit):-
    guess_next_position(Pos,Posforward,north),
    \+ memberchk(Posforward,Pit).


    guess_action(X/Y, X1/Y,east) :- X1 > X.
    guess_action(X/Y, X/Y1,north):- Y1 < Y.
    guess_action(X/Y, X1/Y,west) :- X1 < X.
    guess_action(X/Y, X/Y1,south) :- Y1 > Y.


      % 这块可以改一下
       guess_next_position(X0/Y0,X/Y,west):-
           X is X0 - 1,Y is Y0.
       guess_next_position(X0/Y0,X/Y,east):-
           X is X0 + 1,Y is Y0.
       guess_next_position(X0/Y0,X/Y,north):-
           X is X0,Y is Y0 - 1.
       guess_next_position(X0/Y0,X/Y,south):-
           X is X0,Y is Y0 + 1.
/*
guess_action(X0/Y0,X/Y,Action):-
  (X<X0,Y = Y0->Action = west
  ;X>X0,Y = Y0->Action = east
  ;X = X0,Y<YO->Action = north
  ;X = X0,Y>Y0->Action = south).
*/

% find the position's neighbour.
% 这块可以优化
pos_neighbour(X/Y, NX/NY,NR,NC) :-
        (   NX is X - 1, NY = Y,NX > 0,NY > 0,\+ NX > NC,\+ NY > NR
        ;   NX is X + 1, NY = Y,NX > 0,NY > 0,\+ NX > NC,\+ NY > NR
        ;   NX = X, NY is Y - 1,NX > 0,NY > 0,\+ NX > NC,\+ NY > NR
        ;   NX = X, NY is Y + 1,NX > 0,NY > 0,\+ NX > NC,\+ NY > NR
        ).


/* the following function check if the position is near the wall root.*/
pos_near_wall_foot(Pos,State,North,West,East,South):-
        pos_near_wall_north(Pos,North),
        pos_near_wall_west(Pos,West),
        pos_near_wall_east(Pos,State,East),
        pos_near_wall_south(Pos,State,South).


% the position is near the north wall.
pos_near_wall_north(Pos,Result):-
    Pos   = _/Y,
    (Y = 1 -> Result is 1; Result is 0
    ).
% the position is near the west wall.
pos_near_wall_west(Pos,Result):-
    Pos = X/_,
    (X = 1 -> Result is 1; Result is 0
    ).

% the position is near the east wall.
pos_near_wall_east(Pos,State,Result):-
    Pos   = X/_,
    State = sta(_,(col-NC),_,_,_,_,_,_,_,_,_,_),
    (X= NC -> Result is 1; Result is 0
    ).
%% the position is near the sorth wall.
pos_near_wall_south(Pos,State,Result):-
    Pos   = _/Y,
    State = sta((row-NR),_,_,_,_,_,_,_,_,_,_,_),
    (Y = NR -> Result is 1; Result is 0
    ).
