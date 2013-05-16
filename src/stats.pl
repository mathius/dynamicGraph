% DynamicGraph, stats module
%
% This module contains predicates for printing statistics
% 
% date: 2013-05-14
% responsible for exported functions:
%       Andrej Krejcir(statsNodes/0, statsEdges/0, statsAnalyseNode/1)
%
:- module( stats, [statsNodes/0, statsEdges/0,
             statsAnalyseNode/1, statsComponents/0, statsMaxComponent/0] ).

:- use_module( messaging, [outputMessage/2, messages/2] ).
:- use_module( utilities, [concatenateAtoms/2, numberToAtom/2] ).
:- use_module( time, [timeConversion/2, timeInterval/2, timeToAtom/2] ).
:- use_module( graph, [graphName/1 ,edge/4] ).
:- use_module( graphComponent, [component/2, computeComponents/0, getComponentList/1]).
:- use_module( graphManipulation, [graphInMoment/1, edge/2, 
                advanceMinute/1, startOfTime/1, endOfTime/1] ).
:- use_module( graphviz, [plotGraph/1, plotGraph/3, graphvizFilename/2]).
:- use_module(library(lists)).
                                               

% utility

filter(_Pred, [], []).
filter(Pred, [X|Rest], [X|Out]):-
    call(Pred,X),!,
    filter(Pred,Rest,Out).
filter(Pred, [_|Rest], Out) :- filter(Pred, Rest, Out).

all(_Pred, []).
all(Pred, [H|Rest]):-
    call(Pred,H),!,
    all(Pred, Rest).

map(_Pred, [], []).
map(Pred, [X|Rest], [Y|Out]):-
    call(Pred,X,Y),!,
    map(Pred, Rest, Out).

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
    findall(ed(X,Y), edge(X,Y,_,_), Edges),
    getListOfNodes(Edges,Nodes),
    length(Nodes,L),
    messages(numberOfNodes, [LenMsg]),
    numberToAtom(L, AtomL),    
    concatenateAtoms([LenMsg, AtomL], MsgLength),
    outputMessage(info, [MsgLength]),
    (   L =< 10, !,
        printNodes(Nodes)
        ;
        true
    ),
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
getListOfNodes([ed(X,Y)|Rest],Accum,Out):-
    updateNodeSet(X,Accum,Accum1),
    updateNodeSet(Y,Accum1,Accum2),!,
    getListOfNodes(Rest,Accum2,Out).
    
    
updateNodeSet(Name, [], [ node(Name,1) ]).
updateNodeSet(Name, [node(Name,N)|Rest], [node(Name,N1)|Rest]):- N1 is N + 1.
updateNodeSet(Name, [X|Rest], [X|Out]):- updateNodeSet(Name, Rest, Out).    
    
        
printNodes([]).
printNodes([N|Rest]):-
    printNode(N),
    outputMessage(info,['']), !,
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

%%%%%%%%%%%%%%%%%%%%%%

/* statsAnalyseNode/1
Print information about selected node
*/
statsAnalyseNode(NodeName):-
    nodeExists(NodeName), !,
    getNeighbours(NodeName, Neighbours),
    length(Neighbours, Degree),
    printNode(node(NodeName,Degree)),
    messages(nodeNeighbours, [NeighboursMsg]),
    outputMessage(info, [NeighboursMsg]),
    printNeighbours(Neighbours).

statsAnalyseNode(_NodeName):-
    messages(noNode, Msg),
    outputMessage(error, Msg).    
    
    
nodeExists(Name):- edge(Name,_,_,_).
nodeExists(Name):- edge(_,Name,_,_).

connects(Name, ed(Name,_,_,_)).
connects(Name, ed(_,Name,_,_)).

otherNode(Name, ed(Name,Y,B,E), neighbour(Y,B,E)).
otherNode(Name, ed(X,Name,B,E), neighbour(X,B,E)).

getNeighbours(Name, Out):-
    findall(ed(X,Y,B,E), edge(X,Y,B,E), Edges),
    filter(connects(Name), Edges, NextEdges),
    map(otherNode(Name), NextEdges, Out).
    
printNeighbours([]).
printNeighbours([neighbour(Name,B,E)|Rest]):-
    messages(neighbourMsg, [MFrom, MTo, MLine, MMin]),
    Min is E - B,
    timeToAtom(B, BeginA),
    timeToAtom(E, EndA),
    numberToAtom(Min, MinA),
    concatenateAtoms([Name, MFrom, BeginA, MTo, EndA, MLine, MinA, MMin], Msg),
    outputMessage(info, [Msg]), !,
    printNeighbours(Rest).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* statsComponents/0
Print information about components at the begining of time interval
*/
statsComponents:-
    timeInterval(Begin,_),
    graphInMoment(Begin),
    computeComponents,
    getComponentList(CompList),
    all(printComponent, CompList),
    graphvizFilename(Begin, Filename),
    plotGraph(Filename).
    
/* statsMaxComponent/0
Print information about the biggest component in the time interval
*/
statsMaxComponent:-
    timeInterval(Begin,End),
    findMaxComponent(Begin, End ,Time,Comp),
    timeToAtom(Time, TimeA),
    messages(compTime, [TimeMsg]),
    concatenateAtoms([TimeMsg, TimeA], Msg),
    outputMessage(info, [Msg]),
    printComponent(Comp),
    
    graphInMoment(Time),
    graphvizFilename(Time,Filename),
    comp(_, NodeList) = Comp,
    plotGraph(Filename, NodeList, 'red').
    
   

findMaxComponent(Begin, End, OutTime, OutComp):-
    graphInMoment(Begin),      
    getMaxCompInMoment(Comp), !,
    checkNextMoment(End, Begin, Comp, MaxTime, MaxComp),!,
    (
        inInterval(Begin,End, MaxTime), !, 
        OutComp = MaxComp, OutTime = MaxTime    
        ;
        nodeExists(Node), !,
        OutComp = comp(Node,[Node]), clamp(MaxTime, Begin, End, OutTime)
    ).
    
checkNextMoment(End, PrevTime, PrevComp, OutTime, OutComp):-
    advanceMinute(NewTime),    
    getMaxCompInMoment(NewComp),
    processComponents(End, PrevTime, PrevComp, NewTime, NewComp, Time, Comp), !,
    checkNextMoment(End, Time, Comp, OutTime, OutComp).
    
checkNextMoment(_Interval, OutTime, OutComp, OutTime, OutComp).

getMaxCompInMoment(Comp):-
    computeComponents,
    getComponentList(CompList),
    listMax(compSize, CompList, Comp).

processComponents(End, Time1, Comp1, Time2, Comp2, OutTime, OutComp):-
    Time2 =< End,!,
    compSize(Comp1, Size1),
    compSize(Comp2, Size2),
    (
        Size2 > Size1,!, 
        OutTime = Time2, OutComp = Comp2
        ;
        OutTime = Time1, OutComp = Comp1
    ).
    
processComponents(_End, Time1, Comp1, _Time2, _Comp2, Time1, Comp1).
 
inInterval(B,E,X):- X >= B, X =< E.   
clamp(X,B,_,B):- X < B.
clamp(X,_,E,E):- X > E.
clamp(X,_,_,X). 
 
    
compSize(comp(_, NodeList), Out):- length(NodeList, Out).
    
printComponent( comp(Label, NodeList) ):-
    messages(compLabel, [CompLabelMsg]),
    messages(compSize, [CompSizeMsg]),
    messages(compNodes, [CompNodesMsg]),
    length(NodeList, Len),
    numberToAtom(Len,LenA),
    concatenateAtoms([CompLabelMsg, Label], Msg1),
    concatenateAtoms([CompSizeMsg, LenA], Msg2),
    map(addSeparator(' '), NodeList, MsgList),
    concatenateAtoms(MsgList, MsgNodes),
    concatenateAtoms([CompNodesMsg, MsgNodes], Msg3),
    outputMessage(info, [Msg1]),
    outputMessage(info, [Msg2]),
    outputMessage(info, [Msg3, '']).
    
addSeparator(SepA, Atom, Out):- atom_concat(Atom,SepA,Out).
        
        
