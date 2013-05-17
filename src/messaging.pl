% IB013 Logic Programming
% project 4 (Dynamic graph 2)
% Andrej Krejcir (xkrejcir), Martin Ukrop (xukrop), Vladimir Still (xstill)
% developed using SICStus Prolog 4.2.3
%
% texts module
%
% This module groups all user texts used in DynamicGraph.
% 
% date: 2013-05-08
% responsible for exported functions:
%       Martin Ukrop (outputMessage/2, messages/2)
%
:- module( messsaging, [outputMessage/2, messages/2] ).

/* outputMessage( +Type, +ListOfLines )
write a message to user cli with appropriate prefix
@param +Type                type of message, determines the prefix (info, error, question)
@param +ListOfLines         message formated as a list of atoms
*/
outputMessage( Type, ListOfLines) :-
        telling( OldOutputStream ),
        told,
        tell( user ),
        messagePrefix( Type, Prefix ),
        output( ListOfLines, Prefix ),
        told,
        tell( OldOutputStream ).

/* output( +ListOfLines, +Prefix )
write message to currently open output stream with each line prefixed by Prefix
@param +ListOfLines         message formated as a list of atoms
@param +Prefix              atom, prefix for each message line
*/
output( [], _ ).
output( [Message|Rest], Prefix ) :-
        write( Prefix ),
        write( Message ),
        nl,
        output( Rest, Prefix ).

/* messagePrefix( +Type, -Prefix )
get a prefix for cli messages
@param +Type                type of prefix (info, error, question)
@param -Prefix              prefix as an atom
*/
messagePrefix( info, '# ' ).
messagePrefix( error, '! ' ).
messagePrefix( question, '? ' ).

/* messages( +MessageIdentifier, -Message )
get a user message
@param +MessageIdentifier   atom, uniquely identifies the message
@param -Message             message formated as a list of atoms
*/
messages( intro, % displayed at the start of dynamicGraph 
          ['=================== DynamicGraph ====================',
           '=========== time-dependent graph analyser ===========',
           'Authors: Andrej Krejcir, Martin Ukrop, Vladimir Still',
           '','Write \'help.\' to display help summary.']
        ).
messages( help, % user help summary
          ['=== List of commands',
           'All commands are prolog predicates and must end with a dot.',
           'Shorter command equivalents are written in parentheses.',
           '',
           '== Graph manipulating commands',
           'graphLoad/1           (gl/1)  load graph from specified file',
           'graphGenerate/1       (gg/1)  generate new graph (settings from file)',
           'graphGenerate/0       (gg/0)  generate new graph (interactive)',
           'graph/0               (g/0)   display info about currently loaded graph',
           '',
           '== Time commands',
           'timeBegin/1           (tb/1)  set beginning of time interval',
           'timeBegin/0           (tb/0)  set first edge occurence as beginning',
           'timeEnd/1             (te/1)  set end of time interval',
           'timeEnd/0             (te/0)  set last edge occurence as end',
           'timeInterval/2        (ti/2)  set beginning/end of time interval',
           'timeInterval/0        (ti/0)  set time from first to last edge occurence',
           'timeMoment/1          (tm/1)  set both beginning/end of time interval',
           'time/0                (t/0)   display currently set time',
           '',
           '== Graphviz output commands',
           'graphviz/1            (gv/1)  enable Graphviz outputs to specified folder',
           'graphvizOff/0         (gvo/0) disable Graphviz outputs',
           '',
           '== Graph statistics commands',
           'statsNodes/0          (sn/0)  display statistics for nodes',
           'statsEdges/0          (se/0)  display statistics for edges',
           'statsComponents/0     (sc/0)  display stats for components at the beginning',
           'statsProgress/0       (sp/0)  display times when graph is changing',
           'statsAnalyseNode/1    (san/1) analyse given node',
           'statsMaxComponent/0   (smc/0) find maximal component',
           '',
           '== Other',
           'help/0                (h/0)   display the list of commands',
           'quit/0                (q/0)   quit DynamicGraph session']
        ).
messages( outro, % displayed when exiting dynamicGraph 
          ['Exiting DynamicGraph.','Bye.']
        ).
messages( invalidCommand, % invalid command read from user
          ['These is no such command: ']
        ).
messages( notImplemented, % functions not yet implemented in dynamicGraph
          ['This function is not yet impleneted.','Stay tuned for updates :-}.']
        ).
messages( timeBeginChange, % beginning of the time interval was changed
          ['Time interval beginning set: ']
        ).
messages( timeEndChange, % end of the time interval was changed
          ['Time interval end set: ']
        ).
messages( timeInterval, % printing time interval
          ['Current time interval: '] 
        ).
messages( invalidTimestamp, % timestamp provided is invalid
          ['Invalid timestamp.']
        ).
messages( numberOfNodes, % used for printing node statistics
          ['Number of nodes in graph: ']
        ).
messages( nodeName, % used for printing node statistics
          ['Name: ']
        ).
messages( nodeDegree, % used for printing node statistics
          ['Degree: ']
        ).
messages( nodeMax, % used for printing node statistics
          ['Node with maximum degree is: ']
        ).
messages( nodeMin, % used for printing node statistics
          ['Node with minimum degree is: ']
        ).                                             
messages( edgeNum, % used for printing edge statistics
          ['Number of edges in graph: ']
        ).
messages( edgeInfo1, % used for printing edge statistics
          ['Edge from: ', ' to ']
        ).
messages( edgeInfo2, % used for printing edge statistics
          ['Start time: ', 'End time:   ', ' -- ', 'Interval: ' , ' min']
        ).
messages( edgeMax, % used for printing edge statistics
          ['Edge with longest time interval:']
        ).
messages( edgeMin, % used for printing edge statistics
          ['Edge with shortest time interval:']
        ).
messages( noNode, % used for printing node statistics
          ['Node doesn\'t exist.']
        ).    
messages( nodeNeighbours, % used for printing node statistics
          ['Connected edges:']
        ).        
messages( neighbourMsg, % used for printing node statistics
          [' --> From ', ' to ', ' -- ', ' min']
        ).

messages( compLabel, % used for printing component statistics
          ['Label: ']
        ).                
messages( compSize, % used for printing component statistics
          ['Size: ']
        ). 
messages( compNodes, % used for printing component statistics
          ['Nodes: ']
        ). 
messages( compTime, % used for printing component statistics
          ['Time: ']
        ).
messages( graphLoadSuccess, % graph was successfully loaded
          ['Successfully loaded: ']
        ).
messages( graphLoadError, % graph was not successfully loaded, no graph loaded
          ['Could not load graph.','No graph is loaded.']
        ).
messages( graphInfo,
          ['Graph loaded: ',' -> ',', from ',' to ']
        ).
messages( noGraph,
          ['No graph is loaded.','Load graph using \'graphLoad/1\' command.']
        ).
messages( dupliciteGraphName,
          ['Graph name is defined multiple times.','Old graph name: ','Colliding name: ']
        ).
messages( graphNoName,
          ['No name was specified for the graph.']
        ).
messages( invalidGraphTerm,
          ['Invalid term in graph file: ']
        ).
messages( fileNotOpen,
          ['Could not open file ']
        ).
messages( filenameNotAtomic,
          ['Provided filename is not atomic.']
        ).
messages( nonAtomicGraphName,
          ['The graph name is not atomic.']
        ).
messages( duplicatedEdge,
          [ 'Edge is duplicated in graph: ' ]
        ).
messages( deletingNonexistenEdge,
          [ 'Attemt to delete nonexisten edge: ' ]
        ).
messages( finished, [ 'Finished.' ] ).
messages( writtingFile, [ 'Writing to file: ' ] ).
messages( missingPredicate, [ 'Missing predicate: ' ] ).
messages( nothingGenerated, [ 'Nothing generated.' ] ).
messages( generating, [ 'Generating graph...' ] ).

messages( askName, [ 'Enter name of graph: ' ] ).
messages( askNodes, [ 'Enter number of nodes: ' ] ).
messages( askEdges, [ 'Enter number of edges (min, max separated by dot): ' ] ).
messages( askNewEdge, [ 'Enter probability of emergence of new edge: ' ] ).
messages( askRemoveEdge, [ 'Enter probability of removal of edge: ' ] ).
messages( askDuration, [ 'Enter duration of graph: (dot separated, YYYY-MM-DD+HH:MM): ' ] ).

messages( invalidInput, [ 'Invalid input' ] ).
messages( nameMustBeAtom, [ 'Name must be atom.' ] ).
messages( expectedNumber, [ 'Number was expected.' ] ).
messages( expectedProbability,
          [ 'Probability -- number in range [ 0, 1 ) -- was expected. ' ]
        ).
messages( expected2Numbers, [ 'Two numbers separated by dot were expected. ' ] ).
messages( expectedDuration,
          [ 'Duration -- two dot separated dates (second bigger) -- were expected. ' ]
        ).
messages( graphvizEnabled, [ 'Graphviz output enabled to directory ' ] ).
messages( graphvizDisabled, [ 'Graphviz output disabled.' ] ).
messages( graphvizWritten, [ 'Graphviz file writtern to ' ] ).

messages( warnBigOutput, [ 'This command will generate more than 10 files'
                         , 'Do you want to write those files (y) or disable file output temporarily?'
                         ] ).
messages( addedEdge, [ 'Edge added: ' ] ).
messages( deletedEdge, [ 'Edge deleted: ' ] ).
messages( changesMinute, [ 'Changes in minute: ' ] ).
