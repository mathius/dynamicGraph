% SICStus prolog
%
% part of DynamicGraph
%

:- module( generator, [ graphGenerate/1, graphGenerate/0 ] ).

:- use_module( library( lists ) ).
:- use_module( queue ).

graphGenerate :- graphGenerate( user ).

graphGenerate( File ) :-
      seeing( OldFile )
    , see( File )
    , loadGeneratorPreds( Preds )
    , seen
    , see( OldFile )
    , ! % we dont want to backtrack to user input !
    , checkValidity
    , runGenerator
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
runGenerator.
