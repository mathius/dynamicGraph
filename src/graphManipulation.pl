% DynamicGraph, graphManipulation module
%
% Module basic graph manipulations:
% - load graph for the moment to database
% - depth first search in this graph
%
% date: 2013-05-15
% responsible for exported functions:
%       Martin Ukrop (graphInMoment/1, edge/2)
%       Vladimír Štill ( initialize/1, advanceMinute/1, edge/2 )
%
:- module( graphManipulation, [graphInMoment/1, edge/2, initialize/1, advanceMinute/1 ] ).

:- use_module( changeList, [ getChangeList/1 ] ).
:- use_module( graph, [edge/4] ).


/* edge( +-From, +-To ) 
* public interface to dynamically loaded state of graph
*/
edge( From, To ) :- edgePrivate( From, To ).

/* edgePrivate( +-Source, +-Destionation )
dynamic predicate representing edge in graph in given moment
*/
:- dynamic edgePrivate/2.

/* graphInMoment( +TimeMoment )
loads edges for the required moment from the currently loaded graph
edges are saved as edge/2 predicates into the database
always succeeds exactly once
@param +TimeMoment      desired time moment (dynamicGraph format)
*/
graphInMoment( TimeMoment ) :-
    deleteGraphMoment,
    setupMoment( TimeMoment ),
    (   edge( Source, Destination, BeginTime, EndTime),
        BeginTime =< TimeMoment,
        EndTime >= TimeMoment,
        assertz( edgePrivate( Source, Destination ) ),
        fail
    ;
        true
    ),
    !.

/* deleteGraphMoment/0
retract all edge/2 predicates currently in database
*/
deleteGraphMoment :-
    retractall( edgePrivate( _Source, _Destination) ).


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
    , retractall( edgePrivate( _, _ ) )
    , !.

% makeInitialGraph( -InitialTime, +ChangeList )
makeInitialGraph( InitialTime, changeList( StartOfTime, EndOfTime, Changes ) ) :-
      assertz( startOfTime( StartOfTime ) )
    , assertz( endOfTime( EndOfTime ) )
    , !
    , Changes = [ minute( InitialTime, NowChanges ) | NextChanges ]
    , assertz( currentTime( InitialTime ) )
    , assertz( changesTail( NextChanges ) )
    , !
    , makeMinute( NowChanges ).

% makeMinute( +Changes )
makeMinute( [] ).
makeMinute( [ addEdge( X, Y ) | XS ] ) :-
      assertz( edgePrivate( X, Y ) )
    , !
    , makeMinute( XS ).
makeMinute( [ deleteEdge( X, Y ) | XS ] ) :-
      retract( edgePrivate( X, Y ) )
    , !
    , makeMinute( XS ).

% advanceMinute( +Changes, -Minute )
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

% setupMoment( Moment )
% fast-forward/backward 
setupMoment( Moment ) :-
      retract( currentTime( CTime ) )
    ,
    ( Moment < CTime, !, resetChanges
    ; true
    )
    , fastForwardChanges( Moment ).

resetChanges :-
      retractall( changesTail( _ ) )
    , changeList( _, _, Changes )
    , assertz( changesTail( Changes ) ).

fastForwardChanges( Moment ) :-
      retract( changesTail( Changes ) )
    , fastForwardChanges( Moment, Changes, ForwardChanges )
    , assertz( changesTail( ForwardChanges ) ).

fastForwardChanges( _, [], [] ) :- !.
fastForwardChanges( Moment, [ minute( Moment, _ ) | NextChanges ], NextChanges ) :- !.
fastForwardChanges( Moment, [ _ | NextChanges ], ForwardChanges ) :-
    fastForwardChanges( Moment, NextChanges, ForwardChanges ).



