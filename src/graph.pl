% DynamicGraph, graph module
%
% Description to be added.
%
% date: 2013-05-08
% responsible for exported functions:
%       none
%
:- module( graph, [graphName/1, edge/4, loadGraph/1, printGraph/0] ).

:- use_module( time, [timeConversion/2, timeToAtom/2] ).
:- use_module( utilities, [concatenateAtoms/2, numberToAtom/2] ).
:- use_module( messaging, [messages/2, outputMessage/2] ).

:- dynamic graphName/1, edgePrivate/4.

edge( Source, Destination, BeginTime, EndTime ) :-
        edgePrivate( Source, Destination, BeginTime, EndTime ).

loadGraph( File ) :-
        retractGraph,
        seeing( OldInputStream ),
        seen,
        ( see( File ) ; true), % TBD error message when file does not exist
        repeat,
                read( Term ),
                processTerm( Term, Status ),
                ( Status == error ; Status == success ),
        !,
        printResultMessage( Status ),
        seen,
        see( OldInputStream ).

processTerm( end_of_file, success ).
processTerm( name( GraphName ), Status ) :-
        graphName( OldName ),
        messages( dupliciteGraphName, [Info, OldPrefix, NewPrefix] ),
        concatenateAtoms( [OldPrefix,OldName], OldNameInfo ),
        concatenateAtoms( [NewPrefix,GraphName], NewNameInfo ),
        outputMessage( error, [Info, OldNameInfo, NewNameInfo] ),
        retractGraph,
        Status = error,
        !
        ;
        assertz( graphName( GraphName ) ),
        Status = continue,
        !.
processTerm( e( Source, Destination, Time, Duration ), Status ) :-
        timeConversion( BeginTime, Time ),
        EndTime is BeginTime + Duration,
        assertz( edgePrivate( Source, Destination, BeginTime, EndTime ) ),
        Status = continue,
        !
        ;
        Status = error,
        !.
processTerm( Term, error ) :-
        messages( invalidGraphTerm, [Message] ),
        functor(Term, Functor, Arity),
        numberToAtom( Arity, ArityAtom ),
        concatenateAtoms( [Message,Functor,'/',ArityAtom], MessageFinal),
        outputMessage( error, [MessageFinal] ),
        retractGraph.

retractGraph :-
        ( retract( graphName( _ ) ) ; true),
        ( retract( edgePrivate( _, _, _, _ ) ) ; true),
        !.

printGraph :-
        graphName( GraphName ),
        messages( graphInfo, [NamePrefix, EdgeInfix, BeginTimePrefix, EndTimePrefix] ),
        concatenateAtoms( [NamePrefix,GraphName], GraphNameLine ),
        outputMessage( info, [GraphNameLine] ),
        (       edge( Source, Destination, BeginTime, EndTime ),
                timeToAtom( BeginTime, BeginTimeA ),
                timeToAtom( EndTime, EndTimeA ),
                concatenateAtoms( [Source,EdgeInfix,Destination,BeginTimePrefix,BeginTimeA,EndTimePrefix,EndTimeA], EdgeInfo ),
                outputMessage( info, [EdgeInfo] ),
                fail
        ;
                true
        ),
        !.
printGraph :-
        messages( noGraph, messages ),
        outputMessage( info, messages ).
        
printResultMessage( success ) :-
        messages( graphLoadSuccess, [Message] ),
        graphName( GraphName ),
        !, % used to suppress warning, compiler cannot know, thath graphName is at most one
        concatenateAtoms( [Message,GraphName], MessageFinal ),
        outputMessage( info, [MessageFinal] ).
printResultMessage( error ) :-
        messages( graphLoadError, Messages ),
        outputMessage( error, Messages ).   