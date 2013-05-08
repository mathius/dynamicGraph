% DynamicGraph, utilities module
%
% This module contains general functions useful throughout the project.
%
% date: 2013-05-08
% authors: Martin Ukrop (concatenateAtoms/2)
%
:- module( utilities, [concatenateAtoms/2] ).

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