% DynamicGraph, chageList module
%
% Exports single predicate getChangeList which generates list of changes
% (of graph in database)
%
% date: 2013-05-14
% responsible for exported functions:
%       Vladimir Still ( getChangeList/1, printChangeList/0 )
%
:- module( changeList, [ getChangeList/1, printChangeList/0 ] ).

:- use_module( library( lists ) ).
:- use_module( graph, [ edge/4 ] ).

% TBD
printChangeList.

/* getChangeList( -ChangeList )
 generates list of changes of graph in database
 @param -ChangeList    list of changes in following format:
                 changeList( StartOfTime, EndOfTime, Changes ) where
                  - StartOfTime is first occurence of first edge
                  - EndOfTime is last occurence of last edge
                  - Changes is list of terms in format
                      minute( Minute, ChangesInMimute ) where
                      - Minute is imute of changes ocuring
                      - ChangesInMimute is list of either
                        addEdge( X, Y ) or deleteEdge( X, Y ).
*/
getChangeList( changeList( StartOfTime, EndOfTime, Changes ) ) :-
      findEdges( List )
    , withStart( List, WSList )
    , keysort( WSList, WSListS )
    , seconds( WSListS, Starts )
    , withEnd( List, WEList )
    , keysort( WEList, WEListS )
    , seconds( WEListS, Ends )
    , geneateChanges( Starts, Ends, StartOfTime, EndOfTime, Changes ).

seconds( [], [] ).
seconds( [ _ - X | XS ], [ X | SXS ] ) :- seconds( XS, SXS ).

withStart( [], [] ).
withStart( [ e( X, Y, S, E ) | SS ], [ S - e( X, Y, S, E ) | WSS ] ) :-
    withStart( SS, WSS ).

withEnd( [], [] ).
withEnd( [ e( X, Y, S, E ) | ES ], [ E - e( X, Y, S, E ) | WES ] ) :-
    withEnd( ES, WES ).

% based on http://ai.ia.agh.edu.pl/wiki/pl:prolog:pllib:findall
findEdges( List)  :-
      graph:edge( X, Y, Z, W )                         % Find a solution
      , assertz( queue( e( X, Y, Z, W ) ) )                 % Assert it
      , fail                                  % Try to find more solutions
    ; assertz( queue( bottom ) )            % Mark end of solutions
      , collect( List ).                      % Collect the solutions 

collect( L )  :-
      retract( queue( X ) )     % Retract next solution
    , !
    ,
    ( X == bottom, !, L = []               % End of solutions?
    ; L = [ X | Rest ], collect( Rest )
    ).  % Otherwise collect the rest

% no empty case -> that would be empty graph
geneateChanges( Starts, Ends, StartOfTime, EndOfTime, Changes ) :-
    (   Starts = [ e( _, _, S, _ ) | _ ]
      , Ends = [ e( _, _, _, E ) | _ ]
      , min( S, E, StartOfTime )
      , !
    ;   Starts = [ e( _, _, StartOfTime, _ ) | _ ]
      , Ends = []
      , !
    ;   Starts = []
      , Ends = [ e( _, _, StartOfTime, _ ) | _ ]
    )
    , geneateChanges2( StartOfTime, Starts, Ends, Changes, EndOfTime ).

geneateChanges2( EndOfTime, [], [], [], EndOfTime ).
geneateChanges2( Minute, Starts, Ends, [ minute( Minute, Changes ) | MS ], EndOfTime ) :-
      changesInMinute( Minute, Starts, Ends, Minute1, Starts1, Ends1, Changes )
    ,
    (   Minute == Minute1, EndOfTime = Minute, MS = []
      , ! % we reached end of graph
    ;   geneateChanges2( Minute1, Starts1, Ends1, MS, EndOfTime )
    ).

changesInMinute( Minute, Starts, Ends, Minute1, Starts1, Ends1, Changes ) :-
      starts( Minute, Starts, M1S, Starts1, Chan1 )
    , ends( Minute, Ends, Chan1, M1E, Ends1, Changes )
    ,
    (   min( M1S, M1E, Minute1 )
      , Minute1 > Minute
      , !
    ;   max( M1S, M1E, Minute1 )
    ).

starts( Minute, [], Minute, [], [] ) :- !.
starts( Minute, SS, NextMinute, SS, [] ) :-
      SS = [ e( _, _, NextMinute, _ ) | _ ]
    , Minute < NextMinute
    , !.
starts( Minute, [ e( X, Y, _, _ ) | SS ], NextMinute, NextStarts, [ addEdge( X, Y ) | CS ] ) :-
    starts( Minute, SS, NextMinute, NextStarts, CS ).

ends( Minute, [], Starts, Minute, [], Starts ) :- !.
ends( Minute, ES, Starts, NextMinute, ES, Starts ) :- 
      ES = [ e( _, _, _, NextMinute ) | _ ]
    , Minute < NextMinute
    , !.
ends( Minute, [ e( X, Y, _, _ ) | ES ], Starts, NextMinute, NextEnds, [ deleteEdge( X, Y ) | CS ] ) :-
    ends( Minute, ES, Starts, NextMinute, NextEnds, CS ).

min( X, Y, Z ) :- X > Y, !, Z = Y.
min( X, _, X ).

max( X, Y, Z ) :- X < Y, !, Z = Y.
max( X, _, X ).
