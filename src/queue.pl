% IB013 Logic Programming
% project 4 (Dynamic graph 2)
% Andrej Krejcir (xkrejcir), Martin Ukrop (xukrop), Vladimir Still (xstill)
% developed using SICStus Prolog 4.2.3
%
% queue module
%
% Description to be added.
%
% date: 2013-05-08
% responsible for exported functions:
%       Vladimir Still (isQueue/1, emptyQueue/1, queueFromList/2, enqueue/3, dequeue/3, mergeQueue/3)
%
:- module( queue, [ isQueue/1, emptyQueue/1, queueFromList/2
                  , enqueue/3, dequeue/3, mergeQueue/3 ] ).

:- use_module( library( lists ) ).
:- use_module( time ).

% isQueue( +Queue )
isQueue( queue( _, _ ) ).

% emptyQueue( ?Queue )
emptyQueue( queue( [], [] ) ).

% queueFromList( +List, -Queue )
queueFromList( XS, queue( XS, [] ) ).

% enqueue( +Value, +Queue, -QueueWithValue )
enqueue( X, Queue, QueueWithValue ) :-
    nonvar( X ), isQueue( Queue ), intEnqueue( X, Queue, QueueWithValue ).

intEnqueue( X, queue( Front, Back ), queue( Front, [ X | Back ] ) ).

% dequeue( +Queue, -Value, -QueueWithoutHead )
dequeue( Queue, X, QueueWithoutHead ) :-
    isQueue( Queue ), intDequeue( Queue, X, QueueWithoutHead ).

intDequeue( queue( [ X | Front ], Back ), X, queue( Front, Back ) ) :- !.
intDequeue( queue( [], Back ), X, queue( RBack, [] ) ) :-
    reverse( Back, [ X | RBack ] ).

% generally O( n ), in best case O( 1 )
mergeQueue( Q1, Q2, QOut ) :- isQueue( Q1 ), isQueue( Q2 ), intMerge( Q1, Q2, QOut ).

intMerge( queue( [], [] ), Q2, Q2 ) :- !.
intMerge( Q1, queue( [], [] ), Q1 ) :- !.
intMerge( queue( Front, [] ), queue( [], Back ), queue( Front, Back ) ) :- !.
% in this case we make sure all items currently in queue will have
% O( 1 ) dequeue
intMerge( queue( F1, B1 ), queue( F2, B2 ), queue( Front, [] ) ) :-
      reverse( B1, B1R )
    , reverse( B2, B2R ) 
    , append( F2, B2R, Q2 ) % appends must run from behind as appendig short ++ long
    , append( B1R, Q2, B1Q2 ) % is more efficient than long ++ short
    , append( F1, B1Q2, Front ).

