% IB013 Logic Programming
% project 4 (Dynamic graph 2)
% Andrej Krejcir (xkrejcir), Martin Ukrop (xukrop), Vladimir Still (xstill)
% developed using SICStus Prolog 4.2.3
%
% utilities module
%
% This module contains general functions useful throughout the project.
%
% date: 2013-05-15
% responsible for exported functions:
%       Martin Ukrop (concatenateAtoms/2, numberToAtom/2)
%       Vladimír Štill ( makePath/3, prefixToLast/3 )
%
:- module( utilities, [concatenateAtoms/2, numberToAtom/2, openFileForReading/1
                      , makePath/3, prefixToLast/3 ] ).

:- use_module( messaging, [messages/2, outputMessage/2] ).
:- use_module( library( lists ), [ reverse/2 ] ).

/* numberToAtom( +Number, -Atom )
convert number to atom
@param +Number          number to convert, must be instantianed
@param -Atom            atom of the number
*/
numberToAtom( Number, Atom ) :-
        number_chars( Number, Chars ),
        atom_chars( Atom, Chars ).

/* concatenateAtoms( +ListOfAtoms, -Output )
concatenates atoms in the list to a single atom
only a wrapper function for concatenateAtoms0 (due to LCO)
@param +ListOfAtoms         atoms to concatenate, must be instantianed atoms
@param -Output              atom, concatenation in list order
*/
concatenateAtoms( [], '' ).
concatenateAtoms( [Atom|Rest], Output ) :-
        concatenateAtoms0(Rest, Atom, Output).

/* concatenateAtoms0( +ListOfAtoms, +AccumulatorPrefix, -Output )
accumulator version for atoms concatenation
@param +ListOfAtoms         atoms to concatenate, must be instantianed atoms
@param +AccumulatorPrefix   atom of already concatenated parts
@param -Output              atom, concatenation of AccumulatorPrefix and ListOfAtoms
*/
concatenateAtoms0( [], Output, Output ).
concatenateAtoms0( [Atom|Rest], AccumulatorPrefix, Output ) :-
        atom_concat( AccumulatorPrefix, Atom, AccumulatorPrefix2 ),
        concatenateAtoms0( Rest, AccumulatorPrefix2, Output ).

/* openFileForReading( +File )
opens specified file for reading
non-existent files are handled gracefully, originaly opened input stream is kept
@param +File    file to load
*/
openFileForReading( File ) :-
        seeing( OldInputStream ),
        ( atomic( File ) -> true 
                ;
                messages( filenameNotAtomic, [Message] ),
                outputMessage( error, [Message] ),
                fail
        ), 
        on_exception( error(existence_error(_,_),existence_error(_,_,_,_,_)),
                      see( File ), 
                      (         messages( fileNotOpen, [Message] ),
                                concatenateAtoms( [Message,'\'',File,'\''], MessageFinal ),
                                outputMessage( error, [MessageFinal] ),
                                see( OldInputStream ),
                                fail 
                      )
                    ).

/* makePath( +Directory, +File, -Path )
* composes path
* @Directory    directory of path
* @File         file name of path
* @path         resulting path
*/
makePath( Directory, File, Path ) :-
    concatenateAtoms( [ Directory, '/', File ], Path ).

/* prefixToLast( +Source, +Delimiter, -Prefix )
* gets londest prefix up to last occurence of given delimiter (excluding delimiter)
*
* @Source     atom to find delimiter in
* @Delimiter  single character atom -- the delimiter
* @Prefix     prefix of Source up to last occurence of Delimiter
*/
prefixToLast( Source, Delimiter, Prefix ) :-
      atom_chars( Source, SrcChr )
    , atom_chars( Delimiter, DelChr )
    , prefixToLastCh( SrcChr, DelChr, PrefChr )
    , atom_chars( Prefix, PrefChr ).

prefixToLastCh( S, [ D ], P ) :- prefixToLastCh( S, D, [], [], P ).

prefixToLastCh( [], _, _, LastPrefix, Prefix ) :- reverse( LastPrefix, Prefix ).
prefixToLastCh( [ D | XS ], D, Acc, _, Prefix ) :-
      !
    , prefixToLastCh( XS, D, [ D | Acc ], Acc, Prefix ).
prefixToLastCh( [ X | XS ], D, Acc, LastPrefix, Prefix ) :-
    prefixToLastCh( XS, D, [ X | Acc ], LastPrefix, Prefix ).
    
