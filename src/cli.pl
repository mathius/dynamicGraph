% DynamicGraph, cli module
%
% This module contains predicates emulating the command line interface of DynamicGraph
% 
% date: 2013-05-14
% responsible for exported functions:
%       Martin Ukrop (cliMain/0)
%
:- module( cli, [cliMain/0] ).
:- use_module( messaging, [outputMessage/2, messages/2] ).
:- use_module( utilities, [concatenateAtoms/2, numberToAtom/2] ).
:- use_module( time, [timeConversion/2, timeInterval/2, timeToAtom/2] ).
:- use_module( stats, [statsNodes/0] ).
:- use_module( graph, [loadGraph/1, printGraph/0] ).
:- use_module( graphviz, [enableGraphviz/1, disableGraphviz/0] ).
:- use_module( stats, [statsNodes/0, statsEdges/0, statsAnalyseNode/1] ).
:- use_module( changeList, [ printChangeList/0 ] ).
:- use_module( generator, [ graphGenerate/1, graphGenerate/0 ] ).

/* validCommand( +Functor, +Arity, -Command )
list of valid user commands and their mapping to internal predicates
@param +Functor             functor of valid user command
@param +Arity               arity of this user command
@param -Command             name of the internal predicate to call
                            arity must be the same, arguments from the user are used
*/
validCommand( graphLoad, 1, loadGraph ).
validCommand( graphGenerate, 0, graphGenerate ).
validCommand( graphGenerate, 1, graphGenerate ).
validCommand( graph, 0, printGraph ).
validCommand( timeBegin, 0, printTimeInterval ).
validCommand( timeBegin, 1, setBeginTime ).
validCommand( timeEnd, 0, printTimeInterval ).
validCommand( timeEnd, 1, setEndTime ).
validCommand( timeInterval, 0, printTimeInterval ).
validCommand( timeInterval, 2, setTimeInterval ).
validCommand( time, 0, printTimeInterval ).
validCommand( graphviz, 1, enableGraphviz ).
validCommand( graphvizOff, 0, disableGraphviz ).
validCommand( statsNodes, 0, statsNodes ).
validCommand( statsEdges, 0, statsEdges ).
validCommand( statsComponents, 0, notImplemented ).
validCommand( statsProgress, 0, printChangeList ).
validCommand( statsAnalyseNode, 1, statsAnalyseNode ).
validCommand( statsMaxComponent, 0, notImplemented ).
validCommand( help, 0, printHelp ).
validCommand( quit, 0, quit ).
% shothand versions
validCommand( gl, Arity, Command ) :- validCommand( graphLoad, Arity, Command ).
validCommand( gg, Arity, Command ) :- validCommand( graphGenerate, Arity, Command ).
validCommand( g, Arity, Command ) :- validCommand( graph, Arity, Command ).
validCommand( tb, Arity, Command ) :- validCommand( timeBegin, Arity, Command ).
validCommand( te, Arity, Command ) :- validCommand( timeEnd, Arity, Command ).
validCommand( ti, Arity, Command ) :- validCommand( timeInterval, Arity, Command ).
validCommand( t, Arity, Command ) :- validCommand( time, Arity, Command ).
validCommand( gv, Arity, Command ) :- validCommand( graphviz, Arity, Command ).
validCommand( gvo, Arity, Command ) :- validCommand( graphvizOff, Arity, Command ).
validCommand( sn, Arity, Command ) :- validCommand( statsNodes, Arity, Command ).
validCommand( se, Arity, Command ) :- validCommand( statsEdges, Arity, Command ).
validCommand( sc, Arity, Command ) :- validCommand( statsComponents, Arity, Command ).
validCommand( sp, Arity, Command ) :- validCommand( statsProgress, Arity, Command ).
validCommand( san, Arity, Command ) :- validCommand( statsAnalyseNode, Arity, Command ).
validCommand( smc, Arity, Command ) :- validCommand( statsMaxComponent, Arity, Command ).
validCommand( h, Arity, Command ) :- validCommand( help, Arity, Command ).
validCommand( q, Arity, Command ) :- validCommand( quit, Arity, Command ).

/* validateUserInput( +UserTerm, -Command )
convert user command to the appropriate internal predicate
if UserTerm is not valid, write error message and fail 
@param +UserTerm            term to validate (read from user input)
@param -Command             corresponding internal predicate to call
*/
validateUserInput( UserTerm, Command ) :-
        functor( UserTerm, Functor, Arity ),
        validCommand( Functor, Arity, CommandName),
        !,
        UserTerm =.. [ _ | Arguments ],
        Command =.. [ CommandName | Arguments ].
validateUserInput( UserTerm, _ ) :-
        functor( UserTerm, Functor, Arity ),
        messages( invalidCommand, [Message] ),
        numberToAtom( Arity, ArityAtom ),
        concatenateAtoms( [Message,Functor,'/',ArityAtom], MessageFinal ), 
        outputMessage( error, [MessageFinal] ),
        fail.

/* cliMain/0
main cli loop
intro -> loop for command -> outro
*/
cliMain :-
        messages( intro, IntroString ),
        outputMessage( info, IntroString ),
        timeInterval( 0, 0),
        see( user ),
        repeat,
                read( UserTerm ),
                validateUserInput( UserTerm, Command ),
                call( Command ),
                Command == quit,
        !,
        seen,
        messages( outro, OutroString ),
        outputMessage( info, OutroString ).

% ======================================================================
% HELPER PREDICATES FOR USER ACTIONS
% ======================================================================

/* notImplemented/0, notImplemented/1, notImplemented/2
prints a message informing the user of functionality not yet implemented
always succeeds
*/
notImplemented :-
        messages( notImplemented, Message ),
        outputMessage( error, Message ).
notImplemented(_) :-
        messages( notImplemented, Message ),
        outputMessage( error, Message ).
notImplemented(_,_) :-
        messages( notImplemented, Message ),
        outputMessage( error, Message ).

% quit command, always succeeds
quit.
% prints command summary to user output
printHelp :-
        messages( help, Message ),
        outputMessage( info, Message ).
% prints current time interval
printTimeInterval :-
        messages( timeInterval, [Message] ),
        timeInterval( TimeBegin, TimeEnd ),
        timeToAtom( TimeBegin, TimeBeginA ),
        timeToAtom( TimeEnd, TimeEndA ),
        concatenateAtoms( [Message,TimeBeginA,' to ',TimeEndA], MessageFinal ),
        outputMessage( info, [MessageFinal] ).
% sets begin time of time interval
setBeginTime( TimeFunctor ) :-
        timeConversion( Time, TimeFunctor ),
        timeInterval( Time, _ ).
% sets end time of time interval
setEndTime( TimeFunctor ) :-
        timeConversion( Time, TimeFunctor ),
        timeInterval( _, Time ).
% sets both begin and end time of time interval
setEndTime( TimeBeginFunctor, TimeBeginFunctor ) :-
        timeConversion( TimeBegin, TimeBeginFunctor ),
        timeConversion( TimeEnd, TimeBeginFunctor ),
        timeInterval( TimeBegin, TimeEnd ).