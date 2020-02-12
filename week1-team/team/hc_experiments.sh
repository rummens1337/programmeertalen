#!/bin/bash

# Declare global variables in UPPER_SNAKE_CASE.
VERBOSE="-verbose 1"
NR_ITEMS="-nr_items 100"
DIMENSIONS="-dimensions 2"
SOLVER_NAME="-solver hill_climbing"
NR_ITERATIONS="-nr_iterations 10000"
OUTPUT_DIRECTORY="team/hc-logs"
NUMBER_OF_UNPACKS=0
NUMBER_OF_RUNS=0

changeDirectory(){
  # Change working directory and compile Main.java.
  cd ../ && javac Main.java
}

# We are aware that two loops are a bit redundant, but this is the somewhat more readable variant.
readUnpacks(){
  while true
  do
    # Prompt user for input
    read -ep "Enter the amount of unpacks (integer): " NUMBER_OF_UNPACKS

    # Check if input is indeed an integer
    if [ "$NUMBER_OF_UNPACKS" ] && [ "$NUMBER_OF_UNPACKS" -eq "$NUMBER_OF_UNPACKS" ] 2>/dev/null; then
        echo "Value entered: $NUMBER_OF_UNPACKS"
        break;
      else
        echo "Please enter an integer, hint: [0-9]+."
    fi
  done
}

readRuns(){
  while true
  do
    # Prompt user for input
    read -ep "Enter the amount of runs per unpack (integer): " NUMBER_OF_RUNS

    # Check if input is indeed an integer
    if [ "$NUMBER_OF_RUNS" ] && [ "$NUMBER_OF_RUNS" -eq "$NUMBER_OF_RUNS" ] 2>/dev/null; then
        echo "Value entered: $NUMBER_OF_RUNS"
        break;
      else
        echo "Please enter an integer, hint: [0-9]+."
    fi
  done
}

executeHillClimb(){
  echo "__________________________________________"
  echo "Executing the hill_climbing algorithm. "
  echo ""

  start=$(date +%s)
  # Iterate for the amount of different unpack numbers.
  for (( i=1; i < NUMBER_OF_UNPACKS+1; i++ ))
  do
    NR_TO_UNPACK="-nr_to_unpack $i"

    # Run each unpack number five times.
    for (( j=1; j < NUMBER_OF_RUNS+1; j++ ))
    do
      java Main $VERBOSE $NR_ITEMS $DIMENSIONS $SOLVER_NAME $NR_ITERATIONS $NR_TO_UNPACK > "$OUTPUT_DIRECTORY/unpackfile$i-run$j.txt"
      echo "Created file: $OUTPUT_DIRECTORY/unpackfile$i-run$j"
    done
  done
  end=$(date +%s)

  echo "__________________________________________"
  echo "Finished execution in approximately $((end-start)) seconds "
  echo ""
}


# Execute the functions defined above.
# In large files, doing such a thing gives you oversight in the line of execution in a simple yet powerfull manner.
changeDirectory
readUnpacks
readRuns
executeHillClimb