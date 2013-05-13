% DynamicGraph, graph module
%
% Description to be added.
%
% date: 2013-05-08
% responsible for exported functions:
%       none
%
:- module( graph, [test/0, edge/4] ).

:- use_module( time, [timeIntervalBegin/1, timeIntervalEnd/1] ).

edge(a,b,0,1). % test edge
edge(a,c,0,1). % test edge
edge(a,d,0,1). % test edge
edge(a,e,0,1). % test edge

test :-
        timeIntervalBegin(Time),
        write('pokus: '),
        write(Time),
        nl.
