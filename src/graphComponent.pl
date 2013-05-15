% DynamicGraph, graphComponent module
%
% This module contains predicates for computing components of a graph
% 
% date: 2013-05-15
% responsible for exported functions:
%       Andrej Krejcir()
%

:- module( graphComponent, [component/2, computeComponents/0, 
            computeComponents/1, getComponentList/1] ).


:- use_module( graph, [edge/4] ).
:- use_module( graphManipulation, [graphInMoment/1, edge/2, initialize/1, advanceMinute/1] ).
:- use_module(library(lists)).



/*component/2
    component(NodeName, Label)    
    
    @param NodeName - name of the node
    @param Label    - label of the component
*/
:- dynamic component/2.

/* computeComponent/0
    Computes components for graph with all edges
*/
computeComponents:- computeComponents(getNeighbour).
               
/* computeComponent/1
    computeComponent(NeighbourPred)    Computes component for graph with specified edges
    
    @param NeighbourPred(Name, NextName)
        Preditate to get neighbour of a Node; usable to get only certain edegs                                    
*/
computeComponents(NeighbourPred):-
    clearComponents,
    (labelComponents(NeighbourPred) ; true).
     
     

     
     
                                                                                            
clearComponents:- 
    retract( component(_,_)) , fail
    ;
    true. 
    
labelComponents(NeighbourPred):-
    getUnlabeledNode(NodeName),
    labelNode(NodeName,NodeName),
    propagateLabel(NeighbourPred , NodeName, NodeName),
    fail.


nodeExists(Name):- edge(Name,_,_,_).
nodeExists(Name):- edge(_,Name,_,_).

getUnlabeledNode(Name):-
    nodeExists(Name),
    notLabeled(Name).

labelNode(NodeName, Label):-
    assertz(component(NodeName, Label)).
    
propagateLabel(NeighbourPred, NodeName, Label):-
    call(NeighbourPred, NodeName, Neighbour),
    notLabeled(Neighbour),
    labelNode(Neighbour, Label),
    propagateLabel(NeighbourPred,Neighbour, Label),
    fail.

getNeighbour(Name, Out):- edge(Name, Out).
getNeighbour(Name, Out):- edge(Out, Name).

notLabeled(Name):-
    (component(Name, _),!,
    fail)
    ;
    true.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


getComponentList(Out):-
    findall(component(Name,Label), component(Name,Label), CompList),
    processCompList(CompList, Out).
    
processCompList(CompList,Out):- processCompList(CompList, [], Out).
processCompList([], Out , Out).
processCompList([component(Node,Label)|Rest], Accum, Out):-
    updateCompAccum(Accum, Node, Label, Accum2), !,
    processCompList(Rest, Accum2, Out).

updateCompAccum([], Node, Label, [comp(Label, [Node])]).
updateCompAccum([ comp(Label, List)|_Rest], 
                Node, Label,
                [ comp(Label, [Node|List])|_Rest]).
                
updateCompAccum([ X|Rest], Node, Label, [ X|Out]):-
    updateCompAccum(Rest, Node, Label, Out).                