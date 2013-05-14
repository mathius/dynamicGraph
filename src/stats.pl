% DynamicGraph, stats module
%
% This module contains ....
% 
% date: 2013-05-14
% responsible for exported functions:
%       Andrej Krejcir(statsNodes/0, statsEdges/0)
%
:- module( stats, [statsNodes/0, statsEdges/0] ).

:- use_module( messaging, [outputMessage/2, messages/2] ).
:- use_module( utilities, [concatenateAtoms/2, numberToAtom/2] ).
:- use_module( time, [timeConversion/2, timeInterval/2, timeToAtom/2] ).
:- use_module( graph, [edge/4] ).
:- use_module(library(lists)).
                                               

% utility
id(X,X).

listMax(Pred, [X|Rest], Out) :- listMax(Pred, Rest, X, Out).
listMax(_Pred, [], Out, Out).
listMax(Pred, [X|Rest], Y, Out):-
    call(Pred,X,ValX),
    call(Pred,Y,ValY),
    ValX > ValY, !,
    listMax(Pred, Rest, X, Out).
listMax(Pred, [_|Rest], X, Out) :- listMax(Pred, Rest, X, Out).

listMin(Pred, [X|Rest], Out) :- listMin(Pred, Rest, X, Out).
listMin(_Pred, [], Out, Out).
listMin(Pred, [X|Rest], Y, Out):-
    call(Pred,X,ValX),
    call(Pred,Y,ValY),
    ValX < ValY, !,
    listMin(Pred, Rest, X, Out).
listMin(Pred, [_|Rest], X, Out) :- listMin(Pred, Rest, X, Out).



/* statsNodes/0 
Print statistics about nodes in the graph
*/
statsNodes :-
    findall(ed(X,Y,B,E), edge(X,Y,B,E), Edges),
    getListOfNodes(Edges,Nodes),
    length(Nodes,L),
    messages(numberOfNodes, [LenMsg]),
    numberToAtom(L, AtomL),    
    concatenateAtoms([LenMsg, AtomL], MsgLength),
    outputMessage(info, [MsgLength]),
    printNodes(Nodes),
    maxDegreeNode(Nodes,NMax),
    minDegreeNode(Nodes,NMin),
    messages(nodeMax, [MaxMsg]),
    messages(nodeMin, [MinMsg]),
    outputMessage(info, ['',MaxMsg]),
    printNode(NMax),
    outputMessage(info, ['',MinMsg]),
    printNode(NMin).

printNode(node(Name,Degree)):-
    messages(nodeName, [NameMsg]),
    messages(nodeDegree, [DegreeMsg]),
    numberToAtom(Degree, DegreeA),
    concatenateAtoms([NameMsg, Name], Msg1),
    concatenateAtoms([DegreeMsg, DegreeA], Msg2),
    outputMessage(info, [Msg1, Msg2]).
        
getListOfNodes(Edges,Out):- getListOfNodes(Edges, [], Out).

getListOfNodes([],Out,Out).
getListOfNodes([ed(X,Y,_,_)|Rest],Accum,Out):-
    updateNodeSet(X,Accum,Accum1),
    updateNodeSet(Y,Accum1,Accum2),!,
    getListOfNodes(Rest,Accum2,Out).
    
    
updateNodeSet(Name, [], [ node(Name,1) ]).
updateNodeSet(Name, [node(Name,N)|Rest], [node(Name,N1)|Rest]):- 
    N1 is N + 1.
updateNodeSet(Name, [X|Rest], [X|Out]):-
    updateNodeSet(Name, Rest, Out).    
    
        
printNodes([]).
printNodes([N|Rest]):-
    printNode(N),
    outputMessage(info,['']),
    printNodes(Rest).
    

degree(node(_,Degree),Degree).
maxDegreeNode(List, Out) :- listMax(degree, List, Out).
minDegreeNode(List, Out) :- listMin(degree, List, Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* statsEdges/0
Print statistics about edges in the graph
*/
statsEdges:- 
    findall(ed(X,Y,B,E), edge(X,Y,B,E), Edges),
    length(Edges,L),
    messages(edgeNum, [EdgeNumMsg]),
    numberToAtom(L, LA),
    concatenateAtoms([EdgeNumMsg, LA], Msg),
    outputMessage(info, [Msg]),    
    printEdges(Edges),
    
    longestEdge(Edges, LEdge),
    shortestEdge(Edges, SEdge),
    messages(edgeMax, [EdgeMaxMsg]),
    messages(edgeMin, [EdgeMinMsg]),
    outputMessage(info, [EdgeMaxMsg]),
    printEdge(LEdge),
    outputMessage(info, ['',EdgeMinMsg]),
    printEdge(SEdge).
    

    
printEdge(ed(X,Y,B,E)):-
    messages(edgeInfo1, [MFrom, MTo]),
    messages(edgeInfo2, [MStart, MEnd, MLine, MInt ,MMin]),
    timeToAtom(B,BeginTimeA),
    timeToAtom(E,EndTimeA),
    Minutes is E - B,
    numberToAtom(B,BA),
    numberToAtom(E,EA),
    numberToAtom(Minutes, MinutesA),
    concatenateAtoms([MFrom, X, MTo, Y], Msg1),
    concatenateAtoms([MStart, BeginTimeA, MLine, BA], Msg2),
    concatenateAtoms([MEnd, EndTimeA, MLine, EA], Msg3),
    concatenateAtoms([MInt, MinutesA, MMin], Msg4),
    outputMessage(info, [Msg1, Msg2, Msg3, Msg4]).


printEdges([]).
printEdges([E|Rest]):- 
    printEdge(E),
    outputMessage(info, ['']), 
    printEdges(Rest).    

edgeLength(ed(_,_,B,E), Val) :- Val is E - B.
longestEdge(List, Out) :- listMax(edgeLength, List, Out).
shortestEdge(List,Out) :- listMin(edgeLength, List, Out).
