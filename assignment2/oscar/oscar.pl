/*
 *      oscar.pl
 *
 *		Students edit this program to complete the assignment.
 */


candidate_number(61545).


solve_task(Task,Cost):-
	agent_current_position(oscar,P),
	P = p(X0,Y0),
	(Task = go(p(X1,Y1)) -> H is abs(X0-X1)+abs(Y0-Y1) % Manhattan distance
	; otherwise -> H is 0), % 0 when target position unknown
	F1 is H,
	solve_task_bt(Task,[[c(F1,0,P),P]],0,R,Cost,_NewPos),!,	% prune choice point for efficiency
	reverse(R,[_Init|Path]),
	agent_do_moves(oscar,Path).

%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :- 
	achieved(Task,Current,RPath,Cost,NewPos).

solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
	% extract current best path
	Current = [Head|Tail],
	Head = [c(F,G,P)|RPath],

	% search for neighbours with best path
	findall(N, get_neighbour(P,Task,N,RPath),Neighbours),
		

	% Append neighbours with tail, to remove current node
	append(Neighbours,Tail,Queue),

	% Sort list then continue
	setof(Object, is_member(Object,Queue), Agenda),
	%print_list(Current),print("current"),nl,
	%print_list(Tail),print("tail"),nl,
	%print_list(Queue),print("+neighs"),nl,
	print_list(Agenda),print("sort"),nl,
	%read(X),
	%sleep(0.1),

	solve_task_bt(Task,Agenda,D1,RR,Cost,NewPos).

is_member(Object, List):-
	% Do not use membercheck as we want setof to give member
	member(Object, List).

print_list([]).
print_list([Head|Tail]):-
	print(Head),nl,
	print_list(Tail).

achieved(go(Exit),Current,RPath,Cost,NewPos) :-
	% Get head assuming sorted by lowest cost
	Current = [H|T],
	H = [c(Cost,G,NewPos)|RPath],
	( Exit=none -> true
	; otherwise -> RPath = [Exit|_]
	).

achieved(find(O),Current,RPath,Cost,NewPos) :-
	Current = [H|T],
	H = [c(Cost,G,NewPos)|RPath],
	( O=none    -> true
	; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
	).

search(P,N,N,1):-
	map_adjacent(P,N,empty).

%Pos, NewPos, TargetPos, Cost, Path
get_neighbour(P,Task,N,RPath):-
	map_adjacent(P,P1,empty),
	\+ member(P1,RPath), % check we have not been here already
	length(RPath, G),
	( Task = go(T) -> map_distance(P1,T,H)
	; otherwise -> H is 0
	),
	F is G+H,
	GG is -G,
	N = [c(F,GG,P1),P1|RPath].

%%% command shell %%%

shell:-
	get_input(Input),
	handle_input(Input).

handle_input(Input):-
	( Input = stop -> true
	; Input = reset -> ailp_reset,shell
	; Input = [H|T] -> handle_input(H),handle_input(T),shell
	; callable(Input,G,R) -> ( call(G) -> show_response(R) ; show_response('This failed.') ),shell
	; otherwise -> show_response('Unknown command, please try again.'),shell
	).

% get input from user
get_input(Input):-
	write('? '),read(Input).

% show answer to user
show_response(R):-
	( R=shell(Response)   -> writes('! '),writes(Response),writes(nl)
	; R=console(Response) -> term_to_atom(Response,A),do_command([oscar,console,A])
	; R=both(Response)    -> show_response(shell(Response)),show_response(console(Response))
	; R=agent(Response)   -> term_to_atom(Response,A),do_command([oscar,say,A])
	; R=[H|T]             -> show_response(H),show_response(T)
	; R=[]                -> true
	; otherwise           -> writes(['! ',R])
	).

writes(A):-
	( A=[]      -> nl
	; A=nl      -> nl
	; A=[H|T]   -> writes(H),writes(T)
	; A=term(T) -> write(T)
	; otherwise -> write(A)
	).

% callable(+Command, +Goal, ?Response)
callable(call(G),call(G),G).
callable(topup(S),agent_topup_energy(oscar,S),agent(topup)).
callable(energy,agent_current_energy(oscar,E),both(current_energy(E))).
callable(position,agent_current_position(oscar,P),both(current_position(P))).
callable(ask(S,Q),agent_ask_oracle(oscar,S,Q,A),A).
callable(Task,solve_task(Task,Cost),[console(Task),shell(term(Cost))]):-
	task(Task).

task(go(_Pos)).
task(find(_O)).	% oracle o(N) or charging station c(N)
