:- use_module(library(http/http_open)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(www_browser)).


%%% Wikipedia stuff

:- dynamic wp_cache/2.	% used to cache all Wikipedia pages fetched during one Prolog session

% wp(Q,WT) <- issue query Q to Wikipedia and return the page in wikitext format
wp(Q,WT):-
	wp_cache(Q,WT),!.
wp(Q,WT):-
	wp_query2URL(Q,URL),
  http_open(URL, R, []),
  json_read(R, RR, []),
  close(R),
	%http_get(URL,R,[]),
	%atom_json_term(R,RR,[]),
	wt_get(RR,WT0),
	( atomic_list_concat1(_L,'#REDIRECT',WT0) -> wt_link(WT0,QQ),wp(QQ,WT)
	; otherwise -> WT=WT0
	),
	assert(wp_cache(Q,WT)).

% wp(Q) <- issue query Q to Wikipedia and write the page in wikitext format to stdout
wp(Q):-
	wp(Q,WT),
	atomic_list_concat(L,'\n',WT),	% split text T into segments separated by newlines
	writelns(L).	% write the list of segments

% wppd(Q,PD) <- issue query Q to Wikipedia and return the person's persondata
wppd(Q,PD):-
	wp(Q,WT),
	wt_persondata(WT,PD).

% assemble query Q into a URL to retrieve the page in JSON format
wp_query2URL(Q,URL):-
	atomic_list_concat(QW,' ',Q),	% split query Q into words QW
	atomic_list_concat(QW,'%20',QQ),	% reassemble query QQ with '%20' between words from QW
	atomic_list_concat([
		'http://en.wikipedia.org/w/api.php?format=json&action=query&titles=',
		QQ,
		'&prop=revisions&rvprop=content&rawcontinue'
		],URL).


%%% Wikitext stuff

% decompose JSON Prolog term T until wikitext component is located
wt_get(J,Text):-
	( J = ('*'=Text) -> true
	; J = (json(L)) -> wt_get(L,Text)
	; J = (_=json(L)) -> wt_get(L,Text)
	; J = (_=L) -> wt_get(L,Text)
	; J = [H|T] -> ( wt_get(H,Text) ; wt_get(T,Text) )
	).

% find bracketed elements; only works if unnested
wt_element(WT,Begin,End,Element):-
	atomic_list_concat1(Ls,Begin,WT),
	member(X,Ls),
	atomic_list_concat1([Element|_],End,X),
	Element \= ''.

wt_link(WT,Link):-
	wt_link(WT,Link,_Anchor,_WT_Link).

wt_link(WT,Link,Anchor,WT_Link):-
	wt_element(WT,'[[',']]',Link0),
	( atomic_list_concat1([Link,Anchor],'|',Link0) -> true
	; otherwise -> Link=Link0, Anchor=Link0
	),
	atomic_list_concat(['[[',Link0,']]'],WT_Link).

wt_template(WT,Template,WT_Template):-
	wt_element(WT,'{{','}}',Template),
	atomic_list_concat(['{{',Template,'}}'],WT_Template).

wt_ref(WT,Ref,WT_Ref):-
	wt_element(WT,'<ref>','</ref>',Ref),
	atomic_list_concat(['<ref>',Ref,'</ref>'],WT_Ref).

wt_persondata(WT,PD):-
	wt_template(WT,Template,_WT_Template),
	( atomic_list_concat(['',PD0],'Persondata',Template) -> atomic_list_concat(PD1,'|',PD0)
	; atomic_list_concat(['',_,PD0],'Persondata',Template) -> atomic_list_concat(PD1,'|',PD0)
	),get_persondata(PD1,PD).

get_persondata([],[]).
get_persondata([X|Xs],Out):-
	( atomic_list_concat1(L,=,X) ->
		( L = [_A,'\n'] -> Out = Ys
		; L = [_A,' ']  -> Out = Ys
		; L = [A,B]     -> Out = [A=B|Ys]
		)
	; otherwise -> Out = Ys	% skip X if it doesn't contain =
	),
	get_persondata(Xs,Ys).


%%% Utilities

% write a list of items with a newline after each
writelns([]):-nl.
writelns([H|T]):-
	write(H),nl,
	writelns(T).

% version of atomic_list_concat/3 that fails if separator doesn't occur
atomic_list_concat1(L, S, A):-
	atomic_list_concatN(N, L, S, A),
	N>0.

atomic_list_concatN(N, L, S, A):-
	atomic_list_concat(L, S, A),
	length(L,N0), N is N0-1.


%%% Actors and links

actor('Billy Bob Thornton').
actor('Frances McDormand').
actor('Gabriel Byrne').
actor('George Clooney').
actor('Holly Hunter').
actor('Jeff Bridges').
actor('John Goodman').
actor('John Turturro').
actor('Julianne Moore').
actor('Scarlett Johansson').
actor('Steve Buscemi').
actor('Tom Hanks').
actor('William H. Macy').

link('Barack Obama').
link('Barton Fink').
link('Coen brothers').
link('Golden Globe Award for Best Supporting Actor – Motion Picture').
link('Hollywood Walk of Fame').
link('Inside the Actors Studio').
%link('Manhattan').
%link('Miller\'s Crossing').
link('New York City').
link('O Brother, Where Art Thou?').
link('Rotten Tomatoes').
link('Saturday Night Live').
link('Screen Actors Guild Award').
link('The Big Lebowski').
%link('The New York Times').
link('Tony Award').
link('Los Angeles').

random_actor(A):-
	findall(A,actor(A),L),
	random_member(A,L).

random_link(A,L):-
	actor(A),
	findall(L,(link(L),wp(A,WT),wt_link(WT,L)),Ls),
	random_member(L,Ls).
	

% Check if actor A has link L
check(A,L):-
	wp(A, WT),
	findall(A_L, (link(A_L),wt_link(WT,A_L)),Ls),
	member(L, Ls).

% Recursively ask agents if they have link L,
% adding successful agents to the Ys list
ask_agents(Xs,L,Ys, Zs) :-
	Xs = [A | As],
	(check(A,L) ->
		ask_agents(As, L, [A | Ys], Zs),! ;
		ask_agents(As, L, Ys, Zs),!
	).

% Set the Zs list to the completed Ys list and return
ask_agents(_,_,Ys,Zs) :-
	Zs = Ys.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% task 3 - modify to work on grid    
find_identity(A) :-
    clear_memory(Os,CHs),
    % full list of potential actors
	findall(X, actor(X), Xs),
    agent_current_energy(oscar,Energy),
    find_charge(1,A,Xs).
    
find_charge(CID,A,Xs) :-
    solve_task(find(c(CID)),_,NewPos),
    print('Found station'),nl,
    agent_topup_energy(oscar,c(CID)),
    print('Topped up'),nl,
    (CID == 2 -> 
    find_identity(A,Xs,NewPos,Energy,1);
    find_charge(2,A,Xs)).
    
find_identity(A,Xs,Pos,Energy,N) :-
    (N > 10 -> 
        print('all oracles visited'),nl,terminate;
        true),
    solve_task(find(c(1)),_),
    agent_topup_energy(oscar,c(1)),
    agent_current_energy(oscar, Energy),   
% go to an oracle without dying pls
    solve_task(find(o(N)),_),
    agent_current_position(oscar, NewPos),
    print(NewPos),nl,
    (agent_check_oracle(oscar, o(N)) ->
        N1 is N+1, find_identity(A,Xs,NewPos,Energy,N1); %keep on navigating
        print('querying'),nl,query_oracle(A,Xs,NewPos,N) % how to handle the Os ??
        ). % address Agent=oscar and OID=o(1), what does it return exactly ??? 

% a single oracle query    
query_oracle(A,Xs,Pos,N) :-
    % ask an oracle
    agent_ask_oracle(oscar,o(N),link,L),
	ask_agents(Xs, L, [], Zs),
	(Length == 1 ->
		found_identity(Zs) ;
        N1 is N+1,print(N1),nl,find_identity(A,Xs,Pos,Energy,N1)
	).
    
% terminate here
found_identity(Zs) :-
    print('success'), nl,
    Zs = [A],
    print(A),
    ! .
   
% use to save oracles and charging stations   
save_pos(Pos,Ps,NewPs) :-
    NewPs = [Ps|Pos].
    
% clear memory
clear_memory(Os,CHs) :-
    Os = [],   % oracles
    print(Os),nl,
    CHs = [],
    print(CHs),nl .  % charging stations
    
% terminate if all oracles are visited
terminate :-
    print('You fail.'),
    nl, false.
   
% modified solve_task so it returns the new position of the agent   
solve_task(Task,Cost,NewPos):-
	agent_current_position(oscar,P),
	P = p(X0,Y0),
	(Task = go(p(X1,Y1)) -> H is abs(X0-X1)+abs(Y0-Y1) % Manhattan distance
	; otherwise -> H is 0), % 0 when target position unknown
	F1 is H,
	solve_task_bt(Task,[[c(F1,0,P),P]],0,R,Cost,NewPos),!,	% prune choice point for efficiency
	reverse(R,[_Init|Path]),
	agent_do_moves(oscar,Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% recursively ask questions, reducing the list of potential actors
% until only one remains
find_identity(A, Xs):-
    % each oracle can be queried once
	agent_ask_oracle(oscar,o(1),link,L),
	ask_agents(Xs, L, [], Zs),
	length(Zs,N),
	(N == 1 ->
		Zs= [A] ;
		find_identity(A,Zs)
	).

% clear internal memory on start:
% memory contains all charging stations so it can visit the nearest,
% all oracles so it doesn't move to an oracle that's already been visited
init_oscar_memory :-
    % store charging stations in structure of the sort [(X,Y)]
    CHs = [],
    % store visited oracles in structure of the sort [(X,Y)]
    Os = [].
    % how to do with predicate/0 ???
    
%%% Testing

:- dynamic ailp_identity/1.

% asserts a random actor identity
init_identity :-
	random_actor(A),
	init_identity(A).

% can be used to backtrack over all actor identities
init_identity(A) :-
	actor(A),
	retractall(ailp_identity(_)),
	assert(ailp_identity(A)).

:-init_identity.

% failure-driven loop to test all identities
test:-
	init_identity(_A),	% initialise; don't reveal identity
	find_identity(A),	% program under test
	ailp_identity(I),
	( A=I -> writeln('I am':A)
	; otherwise -> writeln('Wrong answer':A)
	),fail.
test.
