% DynamicGraph, graph module
%
% Module for loading graph from file, checking its consistency and printing it.
%
% date: 2013-05-14
% responsible for exported functions:
%       Martin Ukrop (graphName/1, edge/4, loadGraph/1, printGraph/0)
%
:- module( graph, [graphName/1, edge/4, loadGraph/1, printGraph/0] ).

:- use_module( time, [timeConversion/2, timeToAtom/2] ).
:- use_module( utilities, [concatenateAtoms/2, numberToAtom/2, openFileForReading/1] ).
:- use_module( messaging, [messages/2, outputMessage/2] ).


/* edgePrivate( +-Source, +-Destination, -EndTime, -EndTime)
private predicate for edge info (to prevent functions from altering the loaded graph)
data inside can be accessed via public predicate edge/4
see edge/4 for details
*/
:- dynamic edgePrivate/4.

/* graphName( -GraphName )
private predicate for graph name (to prevent functions from altering the loaded graph)
data inside can be accessed via public predicate graphName/1
see graphName/1 for details
*/
:- dynamic graphNamePrivate/1.

/* edge( +-Source, +-Destination, -BeginTime, -EndTime )
public predicate for edge information
@param +-Source         name of the source node
@param +-Destination    name of the destination node
@param -BeginTime       time when edge starts its existence (dynamicGraph time)
@param -EndTime         time when edge ends its existence (dynamicGraph time)
*/
edge( Source, Destination, BeginTime, EndTime ) :-
        edgePrivate( Source, Destination, BeginTime, EndTime ).

/* graphName( -GraphName )
public predicate for graph name, should succeed at most once
@param -GraphName       variable to unify graph name to
*/
graphName( GraphName ) :-
        graphNamePrivate( GraphName ).
        
/* loadGraph( +File )
loads graph from given file
if does not succeed (incorrect format of graph, not existent, ...) current graph is unloaded
non-existent files are handled gracefully
@param +File        name of the graph file, must be atomic
*/
loadGraph( File ) :-
        retractGraph,
        seeing( OldInputStream ),
        seen,
        (openFileForReading( File ) -> readTerms( Status ) ; Status = error),
        printResultMessage( Status ),
        seen,
        see( OldInputStream ).

/* readTerms( -Status )
reads term from input stream in a cycle
always succeeds exactly once
@param -Status error/success/continue (see processTerm for details)
*/
readTerms( Status ) :-
        repeat,
                read( Term ),
                processTerm( Term, Status ),
                ( Status == error ; Status == success ),
        !.

/* processTerm( +Term, -ProcessingStatus )
processed term falls into one of 4 categories
- name of the graph (check for old name, add)
- edge (add checking time format)
- end_of_file (end successfully)
- other term (end qith error)
@param +Term
@param -ProcessingStatus    continue (ok, we should continue with new term)
                            success (ok, this was the last term, stop processing)
                            error (error occured, stop processing)
*/
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
        atomic( GraphName ),
        assertz( graphNamePrivate( GraphName ) ),
        Status = continue,
        !
        ;
        messages( nonAtomicGraphName, [Message] ),
        outputMessage( error, [Message] ),
        Status = error,
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

/* retractGraph/0
retracts current graph from database
always succeeds exactly once
*/
retractGraph :-
        retractall( graphNamePrivate( _ ) ),
        retractall( edgePrivate( _, _, _, _ ) ).
        
/* printGraph/0
prints graph name and all its edges
if no graph is in the database, message saying so is printed
*/
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
        
/* printResultsMessage( +Status )
print final message about graph loading
@param +Status      success (graph was successfully loaded)
                    error (graph could not be loaded)
*/
printResultMessage( success ) :-
        messages( graphLoadSuccess, [Message] ),
        graphName( GraphName ),
        !, % used to suppress warning, compiler cannot know, thath graphName is at most one
        concatenateAtoms( [Message,GraphName], MessageFinal ),
        outputMessage( info, [MessageFinal] ).
printResultMessage( error ) :-
        messages( graphLoadError, Messages ),
        outputMessage( error, Messages ).   
