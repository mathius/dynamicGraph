% DynamicGraph, graph module
%
% Description to be added.
%
% date: 2013-05-08
% responsible for exported functions:
%       none
%
:- module( graph, [test/0] ).

:- use_module( time, [timeIntervalBegin/1, timeIntervalEnd/1] ).

test :-
        timeIntervalBegin(Time),
        write('pokus: '),
        write(Time),
        nl.
