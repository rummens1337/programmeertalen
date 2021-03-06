#!/bin/bash

# Declare global variables in UPPER_SNAKE_CASE.
VERBOSE="-verbose 1"
NR_ITEMS="-nr_items 100"
DIMENSIONS="-dimensions 2"
SOLVER_NAME="-solver hill_climbing"
NR_ITERATIONS="-nr_iterations 10000"
OUTPUT_DIRECTORY="./team/hc-logs"
NUMBER_OF_UNPACKS=6
NUMBER_OF_RUNS=5

# Was used for local testing purposes.
changeDirectory() {
  cd ../ && javac Main.java
}

# If enabled, reads input from user for the value NUMBER_OF_UNPACKS.
# User input is confirmed before continuing.
readUnpacks() {
  while true; do
    # Prompt user for input
    read -ep "Enter the amount of unpacks (integer): " NUMBER_OF_UNPACKS

    # Check if input is indeed an integer
    if [ "$NUMBER_OF_UNPACKS" ] && [ "$NUMBER_OF_UNPACKS" -eq "$NUMBER_OF_UNPACKS" ] 2>/dev/null; then
      echo "Value entered: $NUMBER_OF_UNPACKS"
      break
    else
      echo "Please enter an integer, hint: [0-9]+."
    fi
  done
}

# If enabled, reads input from user for the value NUMBER_OF_RUNS.
# User input is confirmed before continuing.
readRuns() {
  while true; do
    # Prompt user for input
    read -ep "Enter the amount of runs per unpack (integer): " NUMBER_OF_RUNS

    # Check if input is indeed an integer
    if [ "$NUMBER_OF_RUNS" ] && [ "$NUMBER_OF_RUNS" -eq "$NUMBER_OF_RUNS" ] 2>/dev/null; then
      echo "Value entered: $NUMBER_OF_RUNS"
      break
    else
      echo "Please enter an integer, hint: [0-9]+."
    fi
  done
}

# Executes the hill_climbing algorithm and saves it's output to the OUTPUT_DIRECTORY.
# Also prints some debugging info.
executeHillClimb() {
  echo "__________________________________________"
  echo "Executing the hill_climbing algorithm. "
  echo ""

  start=$(date +%s)
  # Iterate for the amount of different unpack numbers.
  for i in $(eval echo "{1..$NUMBER_OF_UNPACKS}"); do
    NR_TO_UNPACK="-nr_to_unpack $i"

    # Iterate for the amount of number of runs.
    for j in $(eval echo "{1..$NUMBER_OF_RUNS}"); do

      java Main $VERBOSE $NR_ITEMS $DIMENSIONS $SOLVER_NAME $NR_ITERATIONS $NR_TO_UNPACK > \
        "$OUTPUT_DIRECTORY/unpackfile$i-run$j.txt"

      echo "Created file: $OUTPUT_DIRECTORY/unpackfile$i-run$j"
    done
  done
  end=$(date +%s)

  echo "__________________________________________"
  echo "Finished execution in approximately $((end - start)) seconds "
  echo ""
}

# Execute the functions defined above.
# In large files, doing such a thing gives you oversight in the line of execution in a simple yet powerfull manner.
# changeDirectory
# readUnpacks
# readRuns

# The above steps are commented out, because it was outside the scope of the project.
# They were however fun to experiment with and gave us a better understanding of interactive bash scripts.
executeHillClimb
