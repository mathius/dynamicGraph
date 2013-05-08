% DynamicGraph, generator module
%
% Description to be added.
%
% date: 2013-05-08
% authors: Vladimir Still (graphGenerate/1, graphGenerate/0)
%
:- module( generator, [ graphGenerate/1, graphGenerate/0 ] ).

:- use_module( library( lists ) ).
:- use_module( library( random ) ).
:- use_module( queue ).
:- use_module( utilities ).
:- use_module( time ).

graphGenerate :- graphGenerate( user ).

graphGenerate( File ) :-
      seeing( OldFile )
    , see( File )
    , loadGeneratorPreds( Preds )
    , seen
    , see( OldFile )
    , ! % we dont want to backtrack to user input if validity fails!
    , checkValidity
    , runGenerator( Edges )
    , writeFile( Edges )
    , unload( Preds ).

loadGeneratorPreds( Preds ) :- read( X ), load( X, [], Preds ).

load( end_of_file, Preds, Preds ).
load( X0, P0, Preds ) :-
      assertz( X0 )
    , P1 = [ X0 | P0 ]
    , read( X1 )
    , load( X1, P1, Preds ).

unload( [] ).
unload( [ P | PS ] ) :- retract( P ), unload( PS ).

checkValidity :- clause( vertices(_), _ ), clause( edges(_,_), _ ), clause( newEdge(_,_), _ )
    , clause( removeEdge(_,_,_), _ ), clause( duration(_,_), _ ).

% TODO
runGenerator( Closed ) :-
      getGenerator( Gen )
    , ! % disallow backtracting to getGenerator which is deterministic
    , genForEachMinute( Gen, GenOut )
    , getClosed( GenOut, Closed ).

getGenerator( gen( Edges, QA, FromTime, ToTime, [], [] ) ) :-
      getEdges( Edges )
    , queueFromList( Edges, QA )
    , duration( FT, TT )
    , !
    , timeToInt( FT, FromTime )
    , timeToInt( TT, ToTime ).

getClosed( gen( _,_,_,_,_, ClosedEdges ), ClosedEdges ).
setClosed( gen( E, QA, FT, TT, OE, _CE ), ClosedEdges, gen( E, QA, FT, TT, OE, ClosedEdges ) ).

getOpen( gen( _,_,_,_, OpenEdges, _ ), OpenEdges ).
setOpen( gen( E, QA, FT, TT, _OE, CE ), OpenEdges, gen( E, QA, FT, TT, OpenEdges, CE ) ).

getQueue( gen( _E, QA, _FT, _TT, _OE, _CE ), QA ).
setQueue( gen( E, _QA, FT, TT, OE, CE ), QA, gen( E, QA, FT, TT, OE, CE ) ).

getEdges( Edges ) :- vertices( V ), !, V1 is V - 1, getEdges( V1, V1, Edges ).

getEdges( 0, 0, [] ).
getEdges( F, F, Edges ) :- vertices( V ), !, F1 is F - 1, V1 is V - 1, getEdges( F1, V1, Edges ).
getEdges( F, T, [ e( F, T ) | Edges ] ) :- F < T, !, T1 is T - 1, getEdges( F, T1, Edges ).

addMinute( gen( E, QA, F, T, O, C ), gen( E, QA, F1, T, O, C ) ) :- F1 is F + 1.

getFromTime( gen( _, _, F, _, _, _ ), F ).

genForEachMinute( G, G ) :-
      G = gen( _, _, FromTime, ToTime, _, _ )
    , FromTime > ToTime
    , !
    , write( '.' ).
genForEachMinute( Gen, GenOut ) :-
      genMinute( Gen, Gen0 )
    , !
    , write( ':' )
    , addMinute( Gen0, Gen1 )
    , genForEachMinute( Gen1, GenOut ).

genMinute( GenIn, GenOut ) :-
      removeEdges( GenIn, Gen1 )
    , getFromTime( Gen1, Minute )
    , newEdge( Minute, Probability )
    , !
    , emptyQueue( Qa )
    , genNonEmptyQueue( Gen1, Qa, Probability, GenOut ).

removeEdges( GenIn, GenIn ) :- getOpen( GenIn, [] ), !. % no open edges
removeEdges( GenIn, GenOut ) :-
      getOpen( GenIn, OS )
    , getClosed( GenIn, C )
    , getFromTime( GenIn, Minute )
    , removeEdges( Minute, OS, OSOut, Closed, C )
    , setOpen( GenIn, OSOut, Gen1 )
    , setClosed( Gen1, Closed, GenOut ).

removeEdges( _, [], [], C, C ) :- !. % finally append already closed edges and finish opened
% edge is removed -> it is closed
removeEdges( Minute, [ e( V1, V2, TStart ) | OS ], OSOut, [ e( V1, V2, TStart, Minute ) | CS ], C ) :-
%    write( ', processing ' ), write( e( V1, V2, TStart ) ),
%    ( ground( V1 ) ; write( OS ) ),
      TD is Minute - TStart
    , removeEdge( Minute, TD, Probability )
    , maybe( Probability ) % fails with probability 1 - Probability
    , !
%    , write( ', closed: ' ), write( e( V1, V2, TStart, Minute ) )
    , removeEdges( Minute, OS, OSOut, CS, C ).

removeEdges( Minute, [ E | OS ], [ E | OSOut ], CS, C ) :- % edge is not to be removed
      removeEdges( Minute, OS, OSOut, CS, C ).


genNonEmptyQueue( G, Qa, Probability, GOut ) :-
      edges( Min, Max )
    , !
    , getOpen( G, Open )
    , length( Open, Cnt )
    , genNonEmptyQueue( G, Qa, Probability, Min, Cnt, Max, GOut ).

% in case of empty queueu add at least minimum
genNonEmptyQueue( G, Qa, Probability, Min, Cnt, _, GOut ) :-
      getQueue( G, Q )
    , emptyQueue( Q )
    , !
    , fillMinimum( G, Qa, Probability, Min, Cnt, GOut ).
% queue is not empty
% we reached maximum of edges -> merge queues and end
genNonEmptyQueue( G, Qa, _, _, Max, Max, GOut ) :-
      !
    , getQueue( G, Q )
    , mergeQueue( Q, Qa, Q1 )
    , setQueue( G, Q1, GOut ).
% dequeue and maybe add
genNonEmptyQueue( G, Qa, Probability, Min, Cnt, Max, GOut ) :-
      maybe( Probability )
    , ! % adding edge
    , getQueue( G, QA )
    , dequeue( QA, e( V1, V2 ), QA1 )
    , getOpen( G, OS )
    , getFromTime( G, Minute )
%    , write( ', adding ' ), write( e( V1, V2, Minute ) )
    , setOpen( G, [ e( V1, V2, Minute ) | OS ], G1 )
    , setQueue( G1, QA1, G2 )
    , Cnt1 is Cnt + 1
    , genNonEmptyQueue( G2, Qa, Probability, Min, Cnt1, Max, GOut ).
genNonEmptyQueue( G, Qa, Probability, Min, Cnt, Max, GOut ) :- % not adding
      getQueue( G, QA )
    , dequeue( QA, E, QA1 )
    , enqueue( E, Qa, Qa1 )
    , setQueue( G, QA1, G1 )
    , genNonEmptyQueue( G1, Qa1, Probability, Min, Cnt, Max, GOut ).

fillMinimum( G, Qa, _, _, _, G ) :- emptyQueue( Qa ), !. % ??? Hm
% generate up to minimum, generator queue is empty
fillMinimum( G, Qa, _, Min, Cnt, GOut ) :-
      Min =< Cnt
    , !
    , setQueue( G, Qa, GOut ).
fillMinimum( G, Qa, Probability, Min, Cnt, GOut ) :- % Min > Cnt
      maybe( Probability )
    , ! % adding
    , dequeue( Qa, e( V1, V2 ), Qa1 )
    , getOpen( G, OS )
    , getFromTime( G, Minute )
%    , write( ', filling ' ), write( e( V1, V2, Minute ) )
    , setOpen( G, [ e( V1, V2, Minute ) | OS ], G1 )
    , Cnt1 is Cnt + 1
    , fillMinimum( G1, Qa1, Probability, Min, Cnt1, GOut ).
fillMinimum( G, Qa, Probability, Min, Cnt, GOut ) :- % Min > Cnt & not adding
      dequeue( Qa, E, Qa1 )
    , enqueue( E, Qa1, Qa2 )
    , fillMinimum( G, Qa2, Probability, Min, Cnt, GOut ).

writeFile( [] ) :- write( 'empty' ).
writeFile( ES ) :-
      name( File )
    , write( 'writing to ' ), write( File ), write( '...' )
    , telling( OldFile )
    , told
    , tell( File )
    , writeFile1( ES )
    , told
    , write( 'finished.' )
    , tell( OldFile ).

writeFile1( [] ).
writeFile1( [ e( V1, V2, F, T ) | ES ] ) :-
      intToTime( F, From )
    , Dur is T - F
    , write( e( V1, V2, From, Dur ) )
    , write( '.' )
    , nl
    , writeFile1( ES ).
