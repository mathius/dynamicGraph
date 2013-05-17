% DynamicGraph, graphManipulation module
%
% Displays progress of in time interval graph, also exporting it to graphviz if enabled.
%
% date: 2013-05-16
% responsible for exported functions:
%       Vladimir Still ( statsProgress/0 )

:- module( statsProgress, [ statsProgress/0 ] ).

:- use_module( graphManipulation, [ currentTime/1, advanceMinute/2
                                  , graphInMoment/1, changesFromNow/1 ] ).
:- use_module( messaging, [ messages/2, outputMessage/2 ] ).
:- use_module( graphviz, [ plotGraph/1, graphvizEnabled/0, graphvizFilename/2 ] ).
:- use_module( time, [ timeInterval/2, timeToAtom/2 ] ).
:- use_module( utilities, [ concatenateAtoms/2 ] ).

/* statsProgress
* Display times when graph is changing (edge is added or removed). If Graphviz support
* is enabled, graph file for each of these moments is generated. Graph file names consists
* of graph name and time moment of the change.
*/
statsProgress :-
      currentTime( ReturnTime )
    , timeInterval( Begin, End )
    , graphInMoment( Begin )
    , changesFromNow( Changes )
    , notifyGraphviz( Changes, Out )
    , stats( Out, End )
    , graphInMoment( ReturnTime ).

notifyGraphviz( Changes, Out ) :-
      graphvizEnabled
    , !
    , length( Changes, L )
    ,
    (   L =< 10
      , !
    ;   messages( warnBigOuptut, WBO )
      , outputMessage( question, WBO )
      , read( X )
      ,
      ( X == y, Out = y, !
      ; X == 'Y' , Out = y, !
      ; Out = n
      )
    ).
notifyGraphviz( _, n ). % gv not enabled

stats( _, _ ).
stats( Out, End ) :-
      advanceMinute( Min, Changes )
    , 
    (   Min =< End
      , !
      , printChanges( Min, Changes )
      , plot( Out, Min )
      , stats( Out, End )
    ; true
    ).

plot( n, _ ).
plot( y, Min ) :-
      graphvizFilename( Min, File )
    , plotGraph( File ).

printChanges( Minute, Changes ) :- 
      messages( changesMinute, [ Chs ] )
    , timeToAtom( Minute, MA )
    , concatenateAtoms( [ Chs, MA, ':' ], Msg ) 
    , outputMessage( info, [ Msg ] )
    , printChanges( Changes ).

printChanges( [] ).
printChanges( [ addEdge( X, Y ) | Changes ] ) :-
      print( addedEdge, X, Y, Changes ).
printChanges( [ deleteEdge( X, Y ) | Changes ] ) :-
      print( deletedEdge, X , Y, Changes ).

print( MsgKey, X, Y, Changes ) :-
      messages( MsgKey, [ AE ] )
    , concatenateAtoms( [ AE, '( ', X, ', ', Y, ' )' ], Msg )
    , outputMessage( info, [ Msg ] )
    , printChanges( Changes ).


