% IB013 Logic Programming
% project 4 (Dynamic graph 2)
% Andrej Krejcir (xkrejcir), Martin Ukrop (xukrop), Vladimir Still (xstill)
% developed using SICStus Prolog 4.2.3
%
% time module
%
% Time manipulating and converting functions
%
% date: 2013-05-08
% responsible for exported functions: 
%       Andrej Krejcir (timeConversion/2)
%       Martin Ukrop (timeInterval/2, timeToAtom/2)
%
:- module( time, [timeConversion/2, timeInterval/2, timeToAtom/2] ).

:- use_module( library( system ) ).
:- use_module( utilities, [concatenateAtoms/2, numberToAtom/2] ).
:- use_module( messaging, [outputMessage/2, messages/2] ).

/* timeToInt(+TimeFunctor, -TimeNumber)
conversion from time functor to time number
*/
% minutes form 2013-3-13+00:00
timeToInt(Year-Month-Day+Hour:Min,OutTime):-
    datime(UnixTime, datime(Year,Month,Day,Hour,Min,0)),
    datime(StartTime, datime(2013,3,13,0,0,0)),
    UnixMin is floor(UnixTime / 60),
    StartMin is floor(StartTime / 60),
    OutTime is UnixMin - StartMin.

/* intToTime(+TimeNumber, - TimeFunctor)
conversion form time number to time functor
*/
intToTime(Time, Year-Month-Day+Hour:Min):-
    datime(StartTime, datime(2013,3,13,0,0,0)),
    UnixTime is StartTime + Time * 60,
    datime(UnixTime, datime(Year,Month,Day,Hour,Min,_)).

/* timeConversion/2
timeConversion( +TimeNumber, -TimeFunctor ) conversion number->functor
timeConversion( -TimeNumber, +TimeFunctor ) conversion number<-functor
if conversion functor->number is unsiccessful, error message is issued
@param TimeNumber               time in DynamicGraph format (integer from 13.3.2013 19:00)
@param TimeFunctor              time in format Year-Month-Day+Hour:Min
*/
timeConversion( TimeNumber, TimeFunctor ):- 
        var( TimeNumber ),
        !,
        (       ground( TimeFunctor ), 
                timeToInt( TimeFunctor, TimeNumber ),
                !
                ;
                messages( invalidTimestamp, [Message] ),
                outputMessage( error, [Message] ),
                fail
        ).
timeConversion( TimeNumber, TimeFunctor ):- 
        integer( TimeNumber ), 
        intToTime( TimeNumber, TimeFunctor ).

/* timeToAtom( +Time, -Atom )
format time into atom for printing
@param +Time            time to convert (must be valid DynamicGraphTime)
@param -Atom            time formated into atom                                      
*/
timeToAtom( Time, Atom ) :-
        timeConversion( Time, Year-Month-Day+Hour:Minute),
        numberToAtom( Year, YearA ),
        numberToAtom( Month, MonthA ),
        numberToAtom( Day, DayA ),
        numberToAtom( Hour, HourA ),
        numberToAtom( Minute, MinuteA ),
        concatenateAtoms( [YearA,'-',MonthA,'-',DayA,' ',HourA,':',MinuteA], Atom ).

/* timeIntervalBegin/1, timeIntervalEnd/1
dynamic predicates denoting start and end of the chosen time interval
each must be in the database exactly one in every moment of the execution
do not add or retract manually, only using timeInterval/2 !!!
*/
:- dynamic timeIntervalBegin/1, timeIntervalEnd/1.
%:- volatile timeIntervalBegin/1, timeIntervalEnd/1. % to suppress warning ( TBD !!!)

/* setTimeIntervalBegin( +-Time )
get/set timeIntervalBegin
@param +Time                   set time interval begin time (must be a valid DynamicGraph time value)
@param -Time                   get time interval begin time (DynamicGraph time format)
*/
setTimeIntervalBegin( Time ) :- % get time
        var( Time ),
        !,
        timeIntervalBegin( Time ).
setTimeIntervalBegin( Time ) :- % set time
        retractall( timeIntervalBegin( _ ) ), % retract old time if applicable, succeed if not
        assertz( timeIntervalBegin( Time ) ),
        messages( timeBeginChange, [Message] ),
        timeToAtom( Time, TimeHumanReadable ),
        concatenateAtoms( [Message,TimeHumanReadable], MessageFinal),
        outputMessage( info, [MessageFinal] ).

/* setTimeIntervalEnd( +-Time )
get/set timeIntervalEnd
@param +Time                   set time interval end time (must be a valid DynamicGraph time value)
@param -Time                   get time interval end time (DynamicGraph time format)
*/
setTimeIntervalEnd( Time ) :- % get time
        var( Time ),
        !,
        timeIntervalEnd( Time ).
setTimeIntervalEnd( Time ) :- % set time
        retractall( timeIntervalEnd( _ ) ), % retract old time if applicable, succeed if not
        assertz( timeIntervalEnd( Time ) ),
        messages( timeEndChange, [Message] ),
        timeToAtom( Time, TimeHumanReadable ),
        concatenateAtoms( [Message,TimeHumanReadable], MessageFinal),
        outputMessage( info, [MessageFinal] ).

/* timeInterval/2
timeInterval( +IntervalBegin, +IntervalEnd ) set interval begin+end
timeInterval( +IntervalBegin, -IntervalEnd ) set interval begin, get interval end
timeInterval( -IntervalBegin, +IntervalEnd ) set interval end, get interval begin
timeInterval( -IntervalBegin, -IntervalEnd ) get interval begin+end
decision of the variant is made based on var(argument)
in case of correct inputs always succeeds just once
@param IntervalBegin            var for getting, valid DynamicGraph time for set
@param IntervalEnd              var for getting, valid DynamicGraph time for set
*/
timeInterval( IntervalBegin, IntervalEnd ) :-
        setTimeIntervalBegin( IntervalBegin ),
        setTimeIntervalEnd( IntervalEnd ).