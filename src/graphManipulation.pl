% DynamicGraph, graphManipulation module
%
% Module basic graph manipulations:
% - load graph for the moment to database
% - depth first search in this graph
%
% date: 2013-05-14
% responsible for exported functions:
%       Martin Ukrop (graphInMoment/1, edge/2)
%
:- module( graphManipulation, [graphInMoment/1, edge/2] ).

:- use_module( graph, [edge/4] ).

/* edge( +-Source, +-Destionation )
dynamic predicate representing edge in graph in given moment
*/
:- dynamic edge/2.

/* graphInMoment( +TimeMoment )
loads edges for the required moment from the currently loaded graph
edges are saved as edge/2 predicates into the database
always succeeds exactly once
@param +TimeMoment      desired time moment (dynamicGraph format)
*/
graphInMoment( TimeMoment ) :-
    deteleGraphMoment,
    (   edge( Source, Destination, BeginTime, EndTime),
        BeginTime =< TimeMoment,
        EndTime >= TimeMoment,
        assertz( edge( Source, Destination ) ),
        fail
    ;
        true
    ),
    !.

/* deleteGraphMoment/0
retract all edge/2 predicates currently in database
*/
deteleGraphMoment :-
    retractall( edge( _Source, _Destination) ).
    