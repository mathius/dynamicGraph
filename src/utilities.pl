% DynamicGraph, utilities module
%
% This module contains general functions useful throughout the project.
%
% date: 2013-05-08
% responsible for exported functions:
%       Martin Ukrop (concatenateAtoms/2, numberToAtom/2)
%
:- module( utilities, [concatenateAtoms/2, numberToAtom/2, openFileForReading/1] ).

:- use_module( messaging, [messages/2, outputMessage/2] ).

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

openFileForReading( File ) :-
        on_exception( error(existence_error(_,_),existence_error(_,_,_,_,_)),
                      see( File ), 
                      ( fileNotOpen( File ),
                        fail )
                    ).

fileNotOpen( File ) :-
        messages( fileNotOpen, [Message] ),
        concatenateAtoms( [Message,'\'',File,'\''], MessageFinal ),
        outputMessage( error, [MessageFinal] ).