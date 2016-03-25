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
%link('New York City').
link('O Brother, Where Art Thou?').
%link('Rotten Tomatoes').
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% task 3 - modify to work on grid    
find_identity(A) :-
    print('moo'),
    agent_current_position(oscar,P),
    print('moo'),
    clear_memory,
    print('moo'),
    % full list of potential actors
	findall(X, actor(X), Xs),
    print('moo'),
    find_identity(A,Xs,Pos,Energy).
    
find_identity(A,Xs,Pos,Energy) :-
    print('moo2'),
    % get the agent's current position
    % potentially done in oscar.pl
    agent_current_position(oscar, Pos),
    print('moo2'),
    % get the agent's current energy
    % potentially done in oscar.pl
    agent_current_energy(oscar, Energy),
    print('moo2'),
% go to an oracle without dying pls: HOW TO SEARCH?????
    % consistently check energy
    % if a charging station is come across, save its position, how to address charging stations??
    %agent_current_position(OID, CH), % OID ???
    print('moo22'),
    %save_pos(CH, CHs),
    % if energy is going down, navigate to a charging station
        % there are two, go to the closer one if known
    % navigate to oracle
    (agent_check_oracle(oscar, o(1)) ->
        query_oracle(A,Xs,Pos); % how to handle the Os ??
        find_identity(A,Xs,Pos,Energy) %keep on navigating
        ). % address Agent=oscar and OID=o(1), what does it return exactly ??? 

% a single oracle query    
query_oracle(A,Xs,Pos) :-
    % save its position
    agent_current_position(o(1),O),
    save_pos(O,Os),
        % ask an oracle
    agent_ask_oracle(oscar,o(1),link,L),
	ask_agents(Xs, L, [], Zs),
	length(Zs,N),
	(N == 1 ->
		found_identity(Zs) ;
        find_identity(A,Xs,Pos,Energy)
	).
    
% terminate here
found_identity(Zs) :-
    Zs = [A],
    print(A),
    ! .
   
% use to save oracles and charging stations   
save_pos(Pos,Ps) :-
    Ps = [Ps|Pos].
    
% clear memory
clear_memory :-
    Os = [],   % oracles
    CHs = [].  % charging stations


    
find_identity_modified(A, Xs):-
    % each oracle can be queried once
	agent_ask_oracle(oscar,o(1),link,L),
	ask_agents(Xs, L, [], Zs),
	length(Zs,N),
	(N == 1 ->
		Zs= [A] ;
		find_identity(A,Zs)
	).

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
