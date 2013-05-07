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
    , genForEachMinute( Gen )
    , getClosed( Gen, Closed ).

getGenerator( gen( Edges, QA, FromTime, ToTime, OpenEdges, ClosedEdges ) ) :-
      getEdges( Edges )
    , queueFromList( Edges, QA )
    , duration( FT, TT )
    , !
    , timeToInt( FT, FromTime )
    , timeToInt( TT, ToTime )
    , OpenEdges = []
    , ClosedEdges = [].

getClosed( gen( _,_,_,_,_, ClosedEdges ), ClosedEdges ).

getOpen( gen( _,_,_,_, OpenEdges, _ ), OpenEdges ).
setOpen( gen( E, QA, FT, TT, _OE, CE ), OpenEdges, gen( E, QA, FT, TT, OpenEdges, CE ) ).

getQ( gen( _E, QA, _FT, _TT, _OE, _CE ), QA ).
setQ( gen( E, _QA, FT, TT, OE, CE ), QA, gen( E, QA, FT, TT, OE, CE ) ).

getEdges( Edges ) :- vertices( V ), !, V1 is V - 1, getEdges( V1, V1, Edges ).

getEdges( 0, 0, [] ).
getEdges( F, F, Edges ) :- vertices( V ), !, F1 is F - 1, V1 is V - 1, getEdges( F1, V1, Edges ).
getEdges( F, T, [ e( F, T ) | Edges ] ) :- F < T, !, T1 is T - 1, getEdges( F, T1, Edges ).

addMinute( gen( E, QA, F, T, O, C ), gen( E, QA, F1, T, O, C ) ) :- F1 is F + 1.

genForEachMinute( gen( _, _, FromTime, ToTime, _, _ ) ) :- FromTime > ToTime.
genForEachMinute( Gen ) :-
      genMinute( Gen, Gen0 )
    , !
    , addMinute( Gen0, Gen1 )
    , genForEachMinute( Gen1 ).

genMinute( GenIn, GenOut ) :-
      removeEdges( GenIn, Minute, Gen1 )
    , newEdge( Minute, Probability )
    , emptyQueue( Qa )
    , genNonEmptyQueue( Gen1, Qa, Probability, GenOut ).

removeEdges( GenIn, _, GenIn ) :- getOpen( GenIn, [] ), !. % no open edges
removeEdges( GenIn, Minute, GenOut ) :-
      getOpen( GenIn, OS )
    , getClosed( GenIn, C )
    , removeEdges( Minute, OS, OSOut, Closed, C )
    , setOpen( GenIn, OSOut, Gen1 )
    , setClosed( Gen1, Closed, GenOut ).

removeEdges( _, [], _, C, C ). % finally append already closed edges
removeEdges( Minute, [ oE( V1, V2, TStart ) | OS ], OSOut, [ e( V1, V2, TStart, Minute ) | CS ], C ) :-
      TD is Minute - TStart
    , removeEdge( Minute, TD, Probability )
    , maybe( Probability ) % fails with probability 1 - Probability
    , !
    , removeEdges( Minute, OS, OSOut, CS, C ).

removeEdges( _, Minute, [ E | OS ], [ E | OSOut ], CS, C ) :- % edge is not to be removed
      removeEdges( Minute, OS, OSOut, CS, C ).


genNonEmptyQueue( G, Qa, Probability, GOut ) :-
      edges( Min, Max )
    , getOpen( G, Open )
    , length( Open, Cnt )
    , genNonEmptyQueue( G, Qa, Probability, Min, Cnt, Max, GOut ).

genNonEmptyQueue( G, Qa, Probability, Min, Cnt, _, GOut ) :-
      getQueue( G, Q )
    , emptyQueue( Q )
    , !
    , fillMinimim( G, Probability, Min, Cnt, Qa, GOut ).
genNonEmptyQueue( G, Qa, _, _, Max, Max, GOut ) :-
      getQueue( G, Q )
    , mergeQueue( Q, Qa, Q1 )
    , setQueue( G, Q1, GOut ).
genNonEmptyQueue( G, Qa, Probability, Min, Cnt, Max, GOut ) :-
      maybe( Probability )
    , ! % adding edge
    , getQueue( G, QA )
    , dequeue( QA, E, QA1 )
    , getOpen( G, OS )
    , setOpen( G, [ E | OS ], G1 )
    , setQueue( G1, QA1, G2 )
    , Cnt1 is Cnt + 1
    , genNonEmptyQueue( G2, Qa, Probability, Min, Cnt1, Max, GOut ).
genNonEmptyQueue( G, Qa, Probability, Min, Cnt, Max, GOut ) :- % not adding
      getQueue( G, QA )
    , dequeue( QA, E, QA1 )
    , enqueue( E, Qa, Qa1 )
    , setQueue( G, QA1, G1 )
    , genNonEmptyQueue( G1, Qa1, Probability, Min, Cnt, Max, GOut ).

fillMinimim( G, Qa, _, Min, Cnt, GOut ) :-
      Min =< Cnt
    , !
    , setQueue( G, Qa, GOut ).
fillMinimim( G, Qa, Probability, Min, Cnt, GOut ) :- % Min > Cnt
      maybe( Probability )
    , ! % adding
    , dequeue( Qa, E, Qa1 )
    , getOpen( G, OS )
    , setOpen( G, [ E | OS ], G1 )
    , Cnt1 is Cnt + 1
    , fillMinimim( G1, Qa1, Probability, Min, Cnt1, GOut ).
fillMinimim( G, Qa, Probability, Min, Cnt, GOut ) :- % Min > Cnt & not adding
      dequeue( Qa, E, Qa1 )
    , enqueue( E, Qa1, Qa2 )
    , fillMinimim( G, Qa2, Probability, Min, Cnt, GOut ).

writeFile( [] ).
writeFile( ES ) :-
      name( File )
    , telling( OldFile )
    , told
    , tell( File )
    , writeFile( ES )
    , told
    , tell( OldFile ).

writeFile( [] ).
writeFile( [ e( V1, V2, F, T ) | ES ] ) :-
      intToTime( F, From )
    , Dur is T - F
    , write( e( V1, V2, From, Dur ) )
    , write( '.' )
    , writeFile( ES ).
