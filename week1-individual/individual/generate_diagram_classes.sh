#!/bin/bash

FILENAME="generate_classes.dot"

generateDotFile(){
	echo "digraph D {"
	for f in $( find .. -name '*.java')
	do    
		cat $f | ./remove_comments.sh | ./get_classes.sh | sed 's/$/ [shape=box]/g'
	done
	echo "}"
}

generatePdf()
{
	dot -T pdf $FILENAME -O 
}

generateDotFile >> $FILENAME
generatePdf
