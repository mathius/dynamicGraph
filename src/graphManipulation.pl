% DynamicGraph, graphManipulation module
%
% Module basic graph manipulations:
% - load graph for the moment to database
% - depth first search in this graph
%
% date: 2013-05-15
% responsible for exported functions:
%       Martin Ukrop (graphInMoment/1, edge/2)
%       Vladimír Štill ( initializeGraph/1, advanceMinute/1, edge/2
%                      , startOfTime/1, endOfTime/1 )
%
:- module( graphManipulation, [ graphInMoment/1, edge/2, initializeGraph/1
                              , advanceMinute/1, startOfTime/1, endOfTime/1 ] ).

:- use_module( changeList, [ getChangeList/1 ] ).
:- use_module( messaging, [ messages/2, outputMessage/2 ] ).
:- use_module( time, [ timeToAtom/2 ] ).
:- use_module( utilities, [ concatenateAtoms/2 ] ).

% needed for graphInMoment/1
:- use_module( graph, [ edge/4 ]).

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
* Change graph to next minute which has different graph
* @NextMinute      Time of graph state after call
*/
advanceMinute( NextMinute ) :-
      retract( changesTail( Changes ) )
    , !
    ,  advanceMinute( Changes, NextMinute ). 
      %  ;
      %  assertz( changesTail( Changes ) ), fail
      %).

/* initializeGraph( -Success )
* Loads initial configuration of graph
* this predicate should be called one for graph, after it is loaded
*
* @Success      is set to either success or error if there are errors in loaded graph
*/
initializeGraph( Success ) :- 
      retractGraph
    ,
    ( getChangeList( ChangeList ) ->
        assertz( ChangeList )
      , !
      , validateGraph( ChangeList, Success )
      , makeInitialGraph( ChangeList )
    ; Success = error
    ).

/* startOfTime( -StartTime )
* gives time of first occurence of first edge
* @StartTime
*/
startOfTime( StartTime ) :- startOfTimePrivate( StartTime ).

/* endOfTime( -EndTime )
* gives time of last occurence of last edge
* @endOfTime
*/
endOfTime( EndTime ) :- endOfTimePrivate( EndTime ).

:- dynamic startOfTimePrivate/1.
:- dynamic endOfTimePrivate/1.

retractGraph :-
      retractall( changeList( _, _, _ ) )
    , retractall( startOfTimePrivate( _ ) )
    , retractall( endOfTimePrivate( _ ) )
    , retractall( currentTime( _ ) )
    , retractall( changesTail( _ ) )
    , retractall( edgePrivate( _, _ ) )
    , !.

% makeInitialGraph( +ChangeList )
makeInitialGraph( changeList( StartOfTime, EndOfTime, Changes ) ) :-
      assertz( startOfTimePrivate( StartOfTime ) )
    , assertz( endOfTimePrivate( EndOfTime ) )
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
    , fastForwardChanges( Moment )
    , assertz( currentTime(Moment) ).
    
resetChanges :-
      retractall( changesTail( _ ) )
    , changeList( _, _, Changes )
    , assertz( changesTail( Changes ) ).

fastForwardChanges( Moment ) :-
      retract( changesTail( Changes ) )
    , fastForwardChanges( Moment, Changes, ForwardChanges )
    , assertz( changesTail( ForwardChanges ) ).

fastForwardChanges( _, [], [] ) :- !.
%  fastForwardChanges( Moment, [ minute( Moment, _ ) | NextChanges ], NextChanges ) :- !.
fastForwardChanges( Moment, [ minute( Time, Ch ) | NextChanges ],  
                            [ minute( Time, Ch ) | NextChanges ] ) :- Time > Moment, !.
fastForwardChanges( Moment, [ _ | NextChanges ], ForwardChanges ) :-
    fastForwardChanges( Moment, NextChanges, ForwardChanges ).

% we validate graph by playing it with checks
% validate( +ChangeList, -Success )
validateGraph( changeList( _, _, CH ), Success ) :-
      validateGraph42( CH, Success )
    , retractall( edgePrivate( _, _ ) ).

% validateGraph42( +Changes, -Success )
validateGraph42( [], success ).
validateGraph42( [ minute( M, NowChanges ) | NextChanges ], Success ) :-
    validateGraph42( NowChanges, NextChanges, M, Success ).

% validateGraph42( +NowChanges, +NextChanges, +Minutem -Success )
validateGraph42( [], NextChanges, _, Success ) :-
    validateGraph42( NextChanges, Success ).
validateGraph42( [ addEdge( X, Y ) | CS ], NextChanges, M, Success ) :-
    ( edgePrivate( X, Y ) -> Success = error, duplicate( M, X, Y )
    ; assertz( edgePrivate( X, Y ) ), validateGraph42( CS, NextChanges, M, Success )
    ).
validateGraph42( [ deleteEdge( X, Y ) | CS ], NextChanges, M, Success ) :-
    ( edgePrivate( X, Y )
    -> retract( edgePrivate( X, Y ) ), validateGraph42( CS, NextChanges, M, Success )
    ;  Success = error, deletingNonexisten( M, X, Y )
    ).

duplicate( Minute, X, Y ) :-
      messages( duplicatedEdge, [ MSG ] )
    , timeToAtom( Minute, AMin )
    , concatenateAtoms( [ MSG, AMin, ': ( ', X, ', ', Y, ' ).' ], FinalMsg )
    , outputMessage( error, [ FinalMsg ] ).
deletingNonexisten( Minute, X, Y ) :-
      messages( deletingNonexistenEdge, [ MSG ] )
    , timeToAtom( Minute, AMin )
    , concatenateAtoms( [ MSG, AMin, ': ( ', X, ', ', Y, ' ).' ], FinalMsg )
    , outputMessage( error, [ FinalMsg ] ).

