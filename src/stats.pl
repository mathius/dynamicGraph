% DynamicGraph, stats module
%
% This module contains ....
% 
% date: 2013-05-09
% responsible for exported functions:
%       Andrej Krejèír(statsNodes/0)
%

:- module( stats, [statsNodes/0, statsEdges/0] ).
:- use_module( messaging, [outputMessage/2, messages/2] ).
:- use_module( utilities, [concatenateAtoms/2, numberToAtom/2] ).
:- use_module( time, [timeConversion/2, timeInterval/2, timeToAtom/2] ).
:- use_module( graph, [edge/4] ).
:- use_module(library(lists)).
                                                
% node(Name, Degree)

statsNodes :-
    findall(ed(X,Y,B,E), edge(X,Y,B,E), Edges),
    getListOfNodes(Edges,Nodes),
    printNodes(Nodes),
    maxDegreeNode(Nodes,NMax),
    minDegreeNode(Nodes,NMin),
    messages(nodeMax, [MaxMsg]),
    messages(nodeMin, [MinMsg]),
    nodeMsg(NMax, MaxNodeMsg),
    nodeMsg(NMin, MinNodeMsg),
    outputMessage(info, ['']),
    outputMessage(info, [MaxMsg | MaxNodeMsg]),
    outputMessage(info, ['']),
    outputMessage(info, [MinMsg | MinNodeMsg]).


nodeMsg(node(Name,Degree), [Msg1, Msg2]):-
    messages(nodeName, [NameMsg]),
    messages(nodeDegree, [DegreeMsg]),
    numberToAtom(Degree, DegreeA),
    concatenateAtoms([NameMsg, Name], Msg1),
    concatenateAtoms([DegreeMsg, DegreeA], Msg2).
    
        
getListOfNodes(Edges,Out):- getListOfNodes(Edges, [], Out).

getListOfNodes([],Out,Out).
getListOfNodes([ed(X,Y,_,_)|Rest],Accum,Out):-
    updateNodeSet(X,Accum,Accum1),
    updateNodeSet(Y,Accum1,Accum2),
    getListOfNodes(Rest,Accum2,Out).
    
    
    
updateNodeSet(Name, [], [ node(Name,1) ]).
updateNodeSet(Name, [node(Name,N)|Rest], [node(Name,N1)|Rest]):- 
    N1 is N + 1.
updateNodeSet(Name, [X|Rest], [X|Out]):-
    updateNodeSet(Name, Rest, Out).    
    
                
nodeMessages([],[]).
nodeMessages([Node|Rest],[ '', Msg1, Msg2 | Out ]):-
    nodeMsg(Node,[Msg1, Msg2]),
    nodeMessages(Rest, Out).                    
    
                
printNodes(Nodes):-
    length(Nodes,L),
    messages(numberOfNodes, [Msg]),
    numberToAtom(L, AtomL),    
    concatenateAtoms([Msg, AtomL], MsgLength),
    nodeMessages(Nodes, NodeMsgs),
    outputMessage(info, [MsgLength | NodeMsgs]).
    
    
maxDegreeNode([N1 | Nodes], Out):- maxDegreeNode(Nodes, N1 ,Out).
maxDegreeNode([], Out, Out).
maxDegreeNode([node(N1,Deg1)| Nodes], node(N2,Deg2), Out):-
    (Deg1 > Deg2,!, maxDegreeNode(Nodes, node(N1,Deg1), Out) )
    ;
    maxDegreeNode(Nodes, node(N2,Deg2), Out).
            
            
minDegreeNode([N1 | Nodes], Out):- minDegreeNode(Nodes, N1 ,Out).
minDegreeNode([], Out, Out).
minDegreeNode([node(N1,Deg1)| Nodes], node(N2,Deg2), Out):-
    (Deg1 < Deg2,!, minDegreeNode(Nodes, node(N1,Deg1), Out) )
    ;
    minDegreeNode(Nodes, node(N2,Deg2), Out).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

statsEdge:- fail.
    