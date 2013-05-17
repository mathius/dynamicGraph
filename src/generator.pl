% DynamicGraph, generator module
%
% Generates random graph, eiter interactivelly or from given description.
% Description must be valid prolog file with predicates described in report.
%
% date: 2013-05-14
% responsible for exported functions:
%       Vladimir Still (graphGenerate/1, graphGenerate/0)
%
:- module( generator, [ graphGenerate/1, graphGenerate/0 ] ).

:- use_module( library( lists ) ).
:- use_module( library( random ) ).
:- use_module( queue ).
:- use_module( utilities, [ concatenateAtoms/2, prefixToLast/3, numberToAtom/2
                          , openFileForReading/1 ] ).
:- use_module( time, [ timeConversion/2 ] ).
:- use_module( messaging, [ outputMessage/2, messages/2 ] ). 

/* graphGenerate
* iteractive graph generation
*/
graphGenerate :-
      getName
    , getNodes
    , getEdges
    , getNewEdge
    , getRemoveEdge
    , getDuration
    , runGenerator( Edges )
    , writeFile( Edges )
    , unloadUser
    ; unloadUser, fail.

getName :-
      ask( askName )
    , read( N )
    , 
    (   atom( N )
      , assertz( name( N ) )
      , !
    ;   invalidInput( nameMustBeAtom ), fail
    ).

getNodes :-
      ask( askNodes )
    ,
    (   read( N )
      , number( N )
      , assertz( vertices( N ) )
      , !
    ;   invalidInput( expectedNumber ), fail
    ).

getEdges :- 
      ask( askEdges )
    ,
    (   read( Min )
      , read( Max )
      , number( Min )
      , number( Max )
      , Min < Max
      , assertz( edges( Min, Max ) )
      , !
    ;   invalidInput( expected2Numbers ), fail
    ).

isProb( X ) :- number( X ), 0 < X, X =< 1.

getNewEdge :-
      ask( askNewEdge )
    ,
    (   read( N )
      , isProb( N )
      , assertz( newEdge( _, N ) )
      , !
    ; invalidInput( expectedProbability ), fail
    ).

getRemoveEdge :-
      ask( askRemoveEdge )
    ,
    (   read( R )
      , isProb( R )
      , assertz( removeEdge( _, _, R ) )
      , !
    ; invalidInput( expectedProbability ), fail
    ).

getDuration :-
      ask( askDuration )
    ,
    (   read( From )
      , read( To )
      , isDuration( From, To )
      , assertz( duration( From, To ) )
      , !
    ; invalidInput( expectedDuration ), fail
    ).

unloadUser :-
      retractall( vertices( _ ) )
    , retractall( name( _ ) )
    , retractall( edges( _, _ ) )
    , retractall( newEdge( _, _ ) )
    , retractall( removeEdge( _, _, _ ) )
    , retractall( duration( _, _ ) ).

isDuration( F, T ) :- timeConversion( FF, F ), timeConversion( TT, T ), FF < TT.

invalidInput( E ) :-
      messages( invalidInput, [ II ] )
    , messages( E, Msg )
    , outputMessage( error, [ II | Msg ] ).

ask( Q ) :- messages( Q, M ), outputMessage( question, M ).

/* graphGenerate( +File )
* generates graph from given description
* @File     description file to be used for generation
*/
graphGenerate( File ) :-
      seeing( OldFile )
    , openFileForReading( File )
    , loadGeneratorPreds( Preds )
    , seen
    , see( OldFile )
    , ! % we dont want to backtrack to user input if validity fails!
    ,
    (   checkValidity
      , optional( File )
      , runGenerator( Edges )
      , writeFile( Edges )
      , unload( Preds )
    ;   unload( Preds )
      , messages( nothingGenerated, [ MSG ] )
      , outputMessage( error, [ MSG ] )
      , fail
    ).

loadGeneratorPreds( Preds ) :- read( X ), load( X, [], Preds ).

load( end_of_file, Preds, Preds ).
load( X0, P0, Preds ) :-
      assertz( X0 )
    , P1 = [ X0 | P0 ]
    , read( X1 )
    , load( X1, P1, Preds ).

unload( [] ).
unload( [ P | PS ] ) :- retract( P ), unload( PS ).

% mandatory predicates
checkValidity :-
      checkPredicate( vertices(_), 'vertices/1' )
    , checkPredicate( edges(_,_), 'edges/2' )
    , checkPredicate( newEdge(_,_), 'newEdge/2' )
    , checkPredicate( removeEdge(_,_,_), 'removeEdge/3' )
    , checkPredicate( duration(_,_), 'duration/2' ).

checkPredicate( Clause, Emsg ) :-
    ( clause( Clause, _ ), !
    ; emsgMissing( Emsg ), fail ).

emsgMissing( Emsg ) :-
      messages( missingPredicate, [ Missing ] )
    , concatenateAtoms( [ Missing, Emsg, '.' ], MSG )
    , outputMessage( error, [ MSG ] ).

% check optional or assertz defaults
optional( File ) :- 
    (   clause( name(_), _ )
      , ! 
    ;   prefixToLast( File, '.', Prefix )
      , concatenateAtoms( [ Prefix, '.graph.pl' ], Name )
      , assertz( name( Name ) )
    ).

runGenerator( Closed ) :-
      messages( generating, G )
    , outputMessage( info, G )
    , getGenerator( Gen )
    , ! % disallow backtracting to getGenerator which is deterministic
    , genForEachMinute( Gen, GenOut )
    , getClosed( GenOut, Closed ).

getGenerator( gen( Edges, QA, FromTime, ToTime, [], [] ) ) :-
      getEdges( Edges )
    , queueFromList( Edges, QA )
    , duration( FT, TT )
    , !
    , timeConversion( FromTime, FT )
    , timeConversion( ToTime, TT ).

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
    , !.
genForEachMinute( Gen, GenOut ) :-
      genMinute( Gen, Gen0 )
    , !
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
    , setClosed( Gen1, Closed, Gen2 )
    , queueClosed( Gen2, Closed, C, GenOut ).

% queue up to edges closed before
queueClosed( G, [], _, G ).
queueClosed( G, [ E | _ ], [ E | _ ], G ).
queueClosed( G0, [ e( X, Y, _, _ ) | ES ], Before, GOut ) :- 
      getQueue( G0, Q )
    , enqueue( e( X, Y ), Q, Q1 )
    , setQueue( G0, Q1, G1 )
    , queueClosed( G1, ES, Before, GOut ).

removeEdges( _, [], [], C, C ) :- !. % finally append already closed edges and finish opened
% edge is removed -> it is closed
removeEdges( Minute, [ e( V1, V2, TStart ) | OS ], OSOut, [ e( V1, V2, TStart, Minute ) | CS ], C ) :-
      TD is Minute - TStart
    , removeEdge( Minute, TD, Probability )
    , maybe( Probability ) % fails with probability 1 - Probability
    , !
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

fillMinimum( G, Qa, Probability, Min, Cnt, GOut ) :-
      emptyQueue( Qa )
    , !
    , getQueue( G, Q )
    , ( emptyQueue( Q ), G = GOut ; fillMinimum( G, Q, Probability, Min, Cnt, GOut ) ).
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
    , setOpen( G, [ e( V1, V2, Minute ) | OS ], G1 )
    , Cnt1 is Cnt + 1
    , fillMinimum( G1, Qa1, Probability, Min, Cnt1, GOut ).
fillMinimum( G, Qa, Probability, Min, Cnt, GOut ) :- % Min > Cnt & not adding
      dequeue( Qa, E, Qa1 )
    , enqueue( E, Qa1, Qa2 )
    , fillMinimum( G, Qa2, Probability, Min, Cnt, GOut ).

writeFile( [] ) :- write( 'empty' ).
writeFile( ES ) :-
      name( Name )
    , concatenateAtoms( [ Name, '.pl' ], File )
    , messages( writtingFile, [ MSG ] )
    , concatenateAtoms( [ MSG, File, '...' ], FinMsg )
    , outputMessage( info, [ FinMsg ] )
    , telling( OldFile )
    , told
    , tell( File )
    , write( 'name( \'' ), write( Name ), write( '\' ).' )
    , nl
    , writeFile1( ES )
    , told
    , messages( finished, [ Fin ] )
    , outputMessage( info, [ Fin ] )
    , tell( OldFile ).

writeFile1( [] ).
writeFile1( [ e( V1, V2, F, T ) | ES ] ) :-
      timeConversion( F, From )
    , Dur is T - F
    , numberToAtom( V1, V1A )
    , numberToAtom( V2, V2A )
    , concatenateAtoms( [ 'n', V1A ], Vertex1 )
    , concatenateAtoms( [ 'n', V2A ], Vertex2 )
    , write( e( Vertex1, Vertex2, From, Dur ) )
    , write( '.' )
    , nl
    , writeFile1( ES ).
