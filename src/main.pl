% DynamicGraph, main module
%
% Wrapper module for running the cli
%
% date: 2013-05-14
% responsible for exported functions:
%       Martin Ukrop (dynamicGraph/0)
%
:- module( main, [dynamicGraph/0] ).

:- use_module( cli, [cliMain/0] ).

dynamicGraph :- cliMain.