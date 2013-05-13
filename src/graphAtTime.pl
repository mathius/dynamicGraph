% DynamicGraph, graphAtTime module
%
% 
%
% date: 2013-05-13
% responsible for exported functions:
%       Vladimír Štill ( initialize/0, edgeNow/2, advanceMinute/1 )
%
:- module( graphAtTime, [ initialize/0, edgeNow/2, advanceMinute/1 ] ).

:- use_module( utilities, [ concatenateAtoms/2, numberToAtom/2 ] ).
:- use_module( changeList, [ getChangeList/1 ] ).

:- dynamic e/2.

/* edgeNow( -From, -To )
*
* @From
* @To
*/
edgeNow( From, To ) :- e( From, To ).

/* advanceMinute( -NextMinute )
* 
* @NextMinute 
*/
advanceMinute( NextMinute ) :-
      retract( changesTail( Changes ) )
    , !
    , advanceMinute( Changes, NextMinute ).

/* initialize
*/
initialize :- 
      retractGraph
    , getChangeList( ChangeList )
    , assertz( ChangeList )
    , !
    , makeInitialGraph( ChangeList ).

retractGraph :-
      retractall( changeList( _, _, _ ) )
    , retractall( startOfTime( _ ) )
    , retractall( endOfTime( _ ) )
    , retractall( currentTime( _ ) )
    , retractall( changesTail( _ ) )
    , retractall( e( _, _ ) )
    , !.

makeInitialGraph( changeList( StartOfTime, EndOfTime, Changes ) ) :-
      assertz( startOfTime( StartOfTime ) )
    , assertz( endOfTime( EndOfTime ) )
    , !
    , Changes = [ minute( M, NowChanges ) | NextChanges ]
    , assertz( currentTime( M ) )
    , assertz( changesTail( NextChanges ) )
    , !
    , makeMinute( NowChanges ).

makeMinute( [] ).
makeMinute( [ addEdge( X, Y ) | XS ] ) :-
      assertz( e( X, Y ) )
    , !
    , makeMinute( XS ).
makeMinute( [ deleteEdge( X, Y ) | XS ] ) :-
      retract( e( X, Y ) )
    , !
    , makeMinute( XS ).

advanceMinute( [], EndOfTime ) :- endOfTime( EndOfTime ).
advanceMinute( [ minute( Minute, NowChanges ) | NextChanges ], Minute ) :-
      retractall( currentTime( _ ) )
    , assertz( currentTime( Minute ) )
    , !
    , retractall( changesTail( _ ) )
    , assertz( changesTail( NextChanges ) )
    , !
    , makeMinute( NowChanges ).

