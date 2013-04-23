% SICStus Prolog
%
% part of DynamicGraph
% 

:- module( queue, [ isQueue/1, emptyQueue/1, queueFromList/2
                  , enqueue/3, dequeue/3 ] ).

:- use_module( library( lists ) ).
:- use_module( utility ).

% isQueue( +Queue )
isQueue( queue( Front, Back ) ) :- is_list( Front ), is_list( Back ).

% emptyQueue( ?Queue )
emptyQueue( queue( [], [] ) ).

% queueFromList( +List, -Queue )
queueFromList( XS, queue( XS, [] ) ) :- is_list( XS ).

% enqueue( +Value, +Queue, -QueueWithValue )
enqueue( X, Queue, QueueWithValue ) :-
    nonvar( X ), isQueue( Queue ), intEnqueue( X, Queue, QueueWithValue ).

intEnqueue( X, queue( Front, Back ), queue( Front, [ X | Back ] ) ).

% dequeue( +Queue, -Value, -QueueWithoutHead )
dequeue( Queue, X, QueueWithoutHead ) :-
    isQueue( Queue ), intDequeue( Queue, X, QueueWithoutHead ).

intDequeue( queue( [ X | Front ], Back ), X, queue( Front, Back ) ).
intDequeue( queue( [], Back ), X, queue( RBack, [] ) ) :-
    reverse( Back, [ X | RBack ] ).
