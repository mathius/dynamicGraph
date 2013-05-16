% DynamicGraph, graphviz module
%
% Description to be added.
%
% date: 2013-05-14
% authors: Vladimir Still ( enableGraphviz/1, disableGraphviz/0, plotGraph/1, plotGraph/3 )
%
:- module( graphviz, [ enableGraphviz/1, disableGraphviz/0, plotGraph/1, plotGraph/3 ] ).

:- use_module( library( lists ) ).
:- use_module( graphManipulation, [edge/2] ).
:- use_module( utilities, [ makePath/3, concatenateAtoms/2 ] ).
:- use_module( messaging, [ outputMessage/2, messages/2 ] ).

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
saves plot of graph given by edges currently in database
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

writeHighlight( _, [] ).
writeHighlight( Color, [ H | HS ] ) :-
      write( H )
    , write( ' [ color=' )
    , write( Color )
    , write( '];' )
    , nl
    , writeHighlight( Color, HS ).
