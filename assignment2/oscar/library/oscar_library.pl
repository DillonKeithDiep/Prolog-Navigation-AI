/*
 * oscar_library.pl
 *
 * Contains predicates which allow the communication between the agent/client and the server through http post requests.
 * All predicates allowed to be sent to the server are indicated by possible_query/2.
 *
 * Only use predicates exported in module heading in your code!
 */


:- module(oscar_library,		
	[query_world/2
	,possible_query/2
    ,my_agent/1
	,leave_game/0
	,join_game/1
	,start_game/0
	,reset_game/0
	,map_adjacent/3
	,map_distance/3
	]).


:- dynamic
	 ailp_internal/1.

:- set_homepage('oscar.html').


referee_queries_path('http://127.0.0.1:8000/agent/queries').
%referee_queries_path('http://137.222.102.101:8000/agent/queries').


query_world(Pred, Args):-
	possible_query(Pred,Args),
	referee_queries_path(Path),
	term_to_atom(Args, NewArgs),
	http_post(Path,
		form_data([ pred = Pred,
					args = NewArgs
				  ]),
		Reply, []),
	term_to_atom(TermReply,Reply),
	( TermReply = fail -> fail
	; otherwise-> Args = TermReply
	).


possible_query(check_pos, [_Pos, _OID]).              	    % ?-query_world( check_pos, [p(1,2), empty])
possible_query(agent_ask_oracle, [_Agent,_OID,_Q,_L]).      %     	   ( agent_ask_oracle, [4, o(3), link, L)
possible_query(agent_current_energy, [_Agent,_E]).          % 		   ( agent_current_energy, [6,E]).
possible_query(agent_current_position, [_Agent,_P]).        % 		   ( agent_current_position, [oscar,P]).
possible_query(agent_topup_energy, [_Agent,_ChargStation]). % 		   ( agent_topup_energy, [2, c(1)]).
possible_query(agent_check_oracle, [_Agent, _Oracle]).      % 		   ( agent_check_oracle, [9, o(1)]).
possible_query(agent_do_moves, [_Agent, _Path]).	        %		   ( agent_do_moves, [ 1, Path]). 
possible_query(internal_leave_game, [_Agent]).	            %		   ( internal_leave_game, [ 2]). 
possible_query(internal_join_game, [_Agent]). 		        %		   ( agent_join_game, [Agent]).
possible_query(game_status, [_Status]). 		            %		   ( game_status, [stopped]).
possible_query(internal_start_game, []).
possible_query(ailp_reset, []).

join_game(Agent):-
	( \+query_world(game_status,[running]) ->
		( my_agent(Agent) -> format('Your agent has already joined the game')
		; otherwise -> query_world(internal_join_game, [Agent]),
			           assert(ailp_internal(agent(Agent)))
		)
	; otherwise -> format('Cannot join! Game has already started')
	).

leave_game:-
	my_agent(Agent),
	retract(ailp_internal(agent(Agent))),
	query_world(internal_leave_game, [Agent]).

start_game:-
	query_world(internal_start_game, []).

my_agent(Agent):-
	ailp_internal(agent(Agent)).

reset_game:-
	query_world(ailp_reset, []).

% map_adjacent(+Pos, ?AdjPos, ?Occ)
% Occ = empty / c(42) / o(37) - charging station / oracle and ids
map_adjacent(Pos, AdjPos, OID) :-
        nonvar(Pos),
        internal_poss_step( Pos, _M, AdjPos, 1),
        query_world( check_pos, [AdjPos, OID]).
        
internal_poss_step(p(X,Y), M, p(X1,Y1), I) :-
	member(M, [s,e,n,w]), % try moves in this order
	( M = s -> X1 =  X,    Y1 is Y+I
	; M = e -> X1 is X+I,  Y1 =  Y
	; M = n -> X1 =  X,    Y1 is Y-I
	; M = w -> X1 is X-I,  Y1 =  Y
	).

% map_distance(+Pos1, +Pos2, ?Distance)
% Manhattan distance between two grid squares
map_distance(p(X,Y),p(X1,Y1), D) :-
	D is abs(X - X1) + abs(Y - Y1).

