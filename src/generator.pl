% SICStus prolog
%
% part of DynamicGraph
%

:- module( generator, [ graphGenerate/1, graphGenerate/0 ] ).

:- use_module( library( lists ) ).
:- use_module( queue ).
:- use_module( utility ).

graphGenerate :- graphGenerate( user ).

graphGenerate( File ) :-
      seeing( OldFile )
    , see( File )
    , loadGeneratorPreds( Preds )
    , seen
    , see( OldFile )
    , ! % we dont want to backtrack to user input !
    , checkValidity
    , runGenerator
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
runGenerator :-
      getEdges( Edges )
    , write( Edges )
    , queueFromList( Edges, Q )
    , duration( FT, TT )
    , !
    , timeToInt( FT, FromTime )
    , timeToInt( TT, ToTime )
    , genForEachMinute( FromTime, ToTime, Q ).

getEdges( Edges ) :- vertices( V ), !, V1 is V - 1, getEdges( V1, V1, Edges ).

getEdges( 0, 0, [] ).
getEdges( F, F, Edges ) :- vertices( V ), !, F1 is F - 1, V1 is V - 1, getEdges( F1, V1, Edges ).
getEdges( F, T, [ e( F, T ) | Edges ] ) :- F < T, !, T1 is T - 1, getEdges( F, T1, Edges ).

genForEachMinute( To, To, _ ).
genForEachMinute( From, To, Q ) :-
      genMinute( From, Q, Q1 )
    , !
    , F1 is From + 1
    , genForEachMinute( F1, To, Q1 ).

genMinute( Minute, Q, OutQ ) :-
      removeEdges( Minute, Q, Q1 )
    , newEdge( Minute, Probability )
    , emptyQueue( Qa )
    , genNonEmptyQueue( Q1, Qa, Probability, OutQ ).
    , true.

genNonEmptyQueue( Q, Qa, Probability, OutQ ). 
