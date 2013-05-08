% DynamicGraph, cli module
%
% This module contains predicates emulating the command line interface of DynamicGraph
% 
% date: 2013-05-08
% authors: Martin Ukrop (outputMessage/2, cliMain/0)
%
:- module( cli, [outputMessage/2, cliMain/0] ).
:- use_module( texts ).
:- use_module( utilities ).

/* messagePrefix( +Type, -Prefix )
get a prefix for cli messages
@param +Type                type of prefix (info, error, question)
@param -Prefix              prefix as an atom
*/
messagePrefix( info, '# ' ).
messagePrefix( error, '! ' ).
messagePrefix( question, '? ' ).

/* validCommand( +Functor, +Arity, -Command )
list of valid user commands and their mapping to internal predicates
@param +Functor             functor of valid user command
@param +Arity               arity of this user command
@param -Command             name of the internal predicate to call
                            arity must be the same, arguments from the user are used
*/
validCommand( graphLoad, 1, notImplemented ).
validCommand( graphGenerate, Arity, notImplemented ) :- member( Arity, [0,1] ).
validCommand( graph, 0, notImplemented ).
validCommand( timeBegin, Arity, notImplemented ) :- member( Arity, [0,1] ).
validCommand( timeEnd, Arity, notImplemented ) :- member( Arity, [0,1] ).
validCommand( timeInterval, Arity, notImplemented ) :- member( Arity, [0,1,2] ).
validCommand( time, 0, notImplemented ).
validCommand( graphviz, 1, notImplemented ).
validCommand( graphvizOff, 0, notImplemented ).
validCommand( statsNodes, 0, notImplemented ).
validCommand( statsEdges, 0, notImplemented ).
validCommand( statsComponents, 0, notImplemented ).
validCommand( statsProgress, 0, notImplemented ).
validCommand( statsAnalyseNode, 1, notImplemented ).
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
        number_chars( Arity, ArityChars ),
        atom_chars( ArityAtom, ArityChars ),
        concatenateAtoms( [Message,Functor,'/',ArityAtom], MessageFinal ), 
        outputMessage( error, [MessageFinal] ),
        fail.

/* outputMessage( +Type, +ListOfLines )
write a message to user cli with appropriate prefix
@param +Type                type of message, determines the prefix (info, error, question)
@param +ListOfLines         message formated as a list of atoms
*/
outputMessage( Type, ListOfLines) :-
        telling( OldOutputStream ),
        told,
        tell( user ),
        messagePrefix( Type, Prefix ),
        output( Prefix, ListOfLines ),
        told,
        tell( OldOutputStream ).

/* outputMessage( +Prefix, +ListOfLines )
write message to currently open output stream with each line prefixed by Prefix
@param +Prefix              atom, prefix for each message line
@param +ListOfLines         message formated as a list of atoms
*/
output( _, []).
output( Prefix, [Message|Rest] ) :-
        write( Prefix ),
        write( Message ),
        nl,
        output( Prefix, Rest ).

/* cliMain/0
main cli loop
intro -> loop for command -> outro
*/
cliMain :-
        messages( intro, IntroString ),
        outputMessage( info, IntroString ),
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

/* quit/0
quit command, always succeeds
*/
quit.

/* printHelp/0
prints command summary to user output
*/
printHelp :-
        messages( help, Message ),
        outputMessage( info, Message ).