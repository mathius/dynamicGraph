% DynamicGraph, graphviz module
%
% Description to be added.
%
% date: 2013-05-08
% authors: Vladimir Still ( plotGraph/2, plotGraph/4 )
%
:- module( graphviz, [ plotGraph/2, plotGraph/4 ] ).

:- use_module( library( lists ) ).

/* plotGraph( +OutputFile, +Edges )
saves plot of graph given by edges to output file
@param +OutputFile     file to save plot to, must be ground
@param +Edges          list of edges in format e( x, y ), must be ground
*/
plotGraph( OldFile, Edges ) :- plotGraph( OldFile, Edges, [] ).

/* plotGraph( +OutputFile, +Edges, +Highlight )
saves plot of graph given by edges to output file, highighting given vertices
@param +OutputFile     file to save plot to
@param +Edges          list of edges in format e( x, y )
@param +Highlight      list of vertices to highlight
@param +Color          color of highlighted vertices
*/
plotGraph( OutputFile, Edges, Highlight, Color ) :-
      telling( OldFile )
    , told
    , tell( OutputFile )
    , write( 'graph G {' )
    , nl
    , writeEdges( Edges )
    , nl
    , writeHighlight( Color, Highlight )
    , write( '}' )
    , nl
    , told
    , tell( OldFile ).

writeEdges( [] ).
writeEdges( [ e( X, Y ) | ES ] ) :-
      write( X )
    , write( ' -- ' )
    , write( Y )
    , write( ';' )
    , nl
    , writeEdges( ES ).

writeHighlight( _, [] ).
writeHighlight( Color, [ H | HS ] ) :-
      write( H )
    , write( ' [ color=' )
    , write( Color )
    , write( '];' )
    , nl
    , writeHighlight( Color, HS ).
