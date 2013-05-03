% SICStus Prolog
% 
% part of DynamicGraph
% 

:- module( utility, [timeToInt/2, intToTime/2, time/2] ).
    
:- use_module(library(system)).
          
% minutes form 2013-3-13+00:00
timeToInt(Year-Month-Day+Hour:Min,OutTime):-
    datime(UnixTime, datime(Year,Month,Day,Hour,Min,0)),
    datime(StartTime, datime(2013,3,13,0,0,0)),
    UnixMin is floor(UnixTime / 60),
    StartMin is floor(StartTime / 60),
    OutTime is UnixMin - StartMin.
    
intToTime(Time, Year-Month-Day+Hour:Min):-
    datime(StartTime, datime(2013,3,13,0,0,0)),
    UnixTime is StartTime + Time * 60,
    datime(UnixTime, datime(Year,Month,Day,Hour,Min,_)).

time(N,T):- var(N), ground(T), !, timeToInt(T,N).
time(N,T):- integer(N), intToTime(N,T).     
