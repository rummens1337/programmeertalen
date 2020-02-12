#!/bin/bash

DIRNAME="/home/rummens/uva/programmeertalen/individual/week1/knapsack/individual/generated_files/"
FILENAME="generate_imports.dot"

generateDotFile(){
	echo "digraph D {"
	for f in $( find .. -name '*.java')
	do    
		cat $f | ./remove_comments.sh | ./get_imports.sh | sed 's/$f/ [shape=box]/g'
	done
	echo "}"
}

generatePdf()
{
	dot -T pdf "$DIRNAME$FILENAME" -O 
}

generateDotFile >> "$DIRNAME$FILENAME"
generatePdf
