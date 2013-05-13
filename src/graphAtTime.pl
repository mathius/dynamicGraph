% DynamicGraph, graphAtTime module
%
% 
%
% date: 2013-05-13
% responsible for exported functions:
%       Vladimír Štill ( initialize/1, edgeNow/2, advanceMinute/1 )
%
:- module( graphAtTime, [ initialize/1, edgeNow/2, advanceMinute/1 ] ).

:- use_module( utilities, [ concatenateAtoms/2, numberToAtom/2 ] ).
:- use_module( changeList, [ getChangeList/1 ] ).

:- dynamic e/2.

/* edgeNow( -From, -To )
*
* @From
* @To
*/
edgeNow( From, To ) :- e( From, To ), ! ; e( To, From ).

/* advanceMinute( -NextMinute )
* 
* @NextMinute 
*/
advanceMinute( NextMinute ) :-
      retract( changesTail( Changes ) )
    , !
    , advanceMinute( Changes, NextMinute ).

/* initialize( -InitialTime )
*/
initialize( InitialTime ) :- 
      retractGraph
    , getChangeList( ChangeList )
    , assertz( ChangeList )
    , !
    , makeInitialGraph( InitialTime, ChangeList ).

retractGraph :-
      retractall( changeList( _, _, _ ) )
    , retractall( startOfTime( _ ) )
    , retractall( endOfTime( _ ) )
    , retractall( currentTime( _ ) )
    , retractall( changesTail( _ ) )
    , retractall( e( _, _ ) )
    , !.

makeInitialGraph( InitialTime, changeList( StartOfTime, EndOfTime, Changes ) ) :-
      assertz( startOfTime( StartOfTime ) )
    , assertz( endOfTime( EndOfTime ) )
    , !
    , Changes = [ minute( InitialTime, NowChanges ) | NextChanges ]
    , assertz( currentTime( InitialTime ) )
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

% no end case -- advanceMinute( [], EndOfTime ) -- as last minute
% is generated when list is singleton
advanceMinute( [ minute( Minute, NowChanges ) | NextChanges ], Minute ) :-
      retractall( currentTime( _ ) )
    , assertz( currentTime( Minute ) )
    , !
    , retractall( changesTail( _ ) )
    , assertz( changesTail( NextChanges ) )
    , !
    , makeMinute( NowChanges ).

