% IB013 Logic Programming
% project 4 (Dynamic graph 2)
% Andrej Krejcir (xkrejcir), Martin Ukrop (xukrop), Vladimir Still (xstill)
% developed using SICStus Prolog 4.2.3
%
% graphviz module
%
% everithing necessary for graphviz export
% - enabling and disabling export
% - plot graphs in moment
% - plot graph hinghlighting ceratin vertices
%
% date: 2013-05-16
% authors: Vladimir Still ( enableGraphviz/1, disableGraphviz/0, plotGraph/1
%                         , plotGraph/3, graphvizEnabled/0 )
%          Andrej Krejcir ( graphvizFilename/2 )
%
:- module( graphviz, [ enableGraphviz/1, disableGraphviz/0, plotGraph/1
                     , plotGraph/3, graphvizEnabled/0, graphvizFilename/2 ] ).

:- use_module( library( lists ) ).
:- use_module( graphManipulation, [edge/2] ).
:- use_module( utilities, [ makePath/3, concatenateAtoms/2, numberToAtom/2 ] ).
:- use_module( messaging, [ outputMessage/2, messages/2 ] ).
:- use_module( graph, [ graphName/1 ] ).
:- use_module( time, [ timeConversion/2 ] ).

:- dynamic graphvizDirectory/1.

/* graphvizEnabled
* succeeds iff graphviz is enabled
*/
graphvizEnabled :- graphvizDirectory( _ ).

/* enableGraphviz( +Dir )
* enabled graphviz output to given directory
* @param Dir      directory to be used as base path for saving
*/
enableGraphviz( Dir ) :- 
      disableGraphvizInt
    , assertz( graphvizDirectory( Dir ) )
    , messages( graphvizEnabled, [ GE ] )
    , concatenateAtoms( [ GE, '\'', Dir, '\'', '.' ], Msg )
    , outputMessage( info, [ Msg ] ).

/* disableGraphviz
* disabled graphviz output.
*/
disableGraphviz :-
      disableGraphvizInt
    , messages( graphvizDisabled, M )
    , outputMessage( info, M ).

disableGraphvizInt :- retractall( graphvizDirectory( _ ) ).

/* plotGraph( +OutputFile, +Edges )
* if graphviz is enabled, saves plot of graph given by edges currently in database
* to graphviz directory
@param +OutputFile     file to save plot to, must be ground
*/
plotGraph( OutputFile ) :- plotGraph( OutputFile, [], 'none' ).

/* plotGraph( +OutputFile, +Edges, +Highlight, +Color )
saves plot of graph given by edges currently in database, highighting given vertices
@param +OutputFile     file to save plot to
@param +Edges          list of edges in format e( x, y )
@param +Highlight      list of vertices to highlight
@param +Color          color of highlighted vertices
*/
plotGraph( OutputFile, Highlight, Color ) :-
      plotGraphIfEnabled( OutputFile, Highlight, Color ).

plotGraphIfEnabled( OutputFile, Highlight, Color ) :-
      graphvizDirectory( Dir )
    , !
    , telling( OldFile )
    , told
    , makePath( Dir, OutputFile, OutputPath )
    , tell( OutputPath )
    , write( 'graph G {' )
    , nl
    , writeEdges
    , nl
    , writeHighlight( Color, Highlight )
    , write( '}' )
    , nl
    , told
    , tell( OldFile )
    , messages( graphvizWritten, [ GW ] )
    , concatenateAtoms( [ GW, '\'', OutputPath, '\'.' ], Mesg )
    , outputMessage( info, [ Mesg ] ).
% case if disabled
plotGraphIfEnabled( _, _, _ ).

writeEdges :-
    (   edge( X, Y )
      , write( X )
      , write( ' -- ' )
      , write( Y )
      , write( ';' )
      , nl
      , fail
    ; true
    ).

% writeHighlight( +Color, +Highlighted )
writeHighlight( _, [] ).
writeHighlight( Color, [ H | HS ] ) :-
      write( H )
    , write( ' [ color=' )
    , write( Color )
    , write( '];' )
    , nl
    , writeHighlight( Color, HS ).
 
/* graphvizFilename( +Time, -Filename )
* composes name for graphviz output file
* @Time         time of graph
* @Filename     name to be used for export
*/
graphvizFilename(Time, Filename):-
    graphName(GName),
    timeConversion( Time, Year-Month-Day+Hour:Minute),
    numberToAtom( Year, YearA ),
    numberToAtom( Month, MonthA ),
    numberToAtom( Day, DayA ),
    numberToAtom( Hour, HourA ),
    numberToAtom( Minute, MinuteA ),
    concatenateAtoms( [YearA,'-',MonthA,'-',DayA,'_',HourA,'-',MinuteA], TimeA ),
    concatenateAtoms([GName,'_', TimeA,'.dot'], Filename).
    
    
        
        
