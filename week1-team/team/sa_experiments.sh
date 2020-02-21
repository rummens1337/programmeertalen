#!/bin/bash

# Declare global variables in UPPER_SNAKE_CASE.
VERBOSE="-verbose 1"
NR_ITEMS="-nr_items 100"
DIMENSIONS="-dimensions 2"
SOLVER_NAME="-solver simulated_annealing"
NR_ITERATIONS="-nr_iterations 10000"
NUMBER_OF_UNPACKS=2 # Best value found in hc_statistics.sh
NUMBER_OF_RUNS=5
MAX_TEMPERATURE=(0 1 10 100 1000 10000)

OUTPUT_DIRECTORY="./team/sa-logs"

# Executes the simulated_annealing algorithm and saves its output to the OUTPUT_DIRECTORY.
# Also prints some debugging info.
executeSimulatedAnnealing() {
  echo "__________________________________________"
  echo "Executing the simulated annealing algorithm. "
  echo ""

  start=$(date +%s)
  iterator=1

  # Iterate for the amount of elements in MAX_TEMPERATURE.
  for i in "${MAX_TEMPERATURE[@]}"; do
    echo "Max temperature: " $i

    # Iterate for the NUMBER_OF_RUNS.
    for j in $(eval echo "{1..$NUMBER_OF_RUNS}"); do

      java Main $VERBOSE $NR_ITEMS $DIMENSIONS $SOLVER_NAME $NR_ITERATIONS $NR_TO_UNPACK > \
        "$OUTPUT_DIRECTORY/unpackfile$iterator-run$j.txt"

      echo "Created file: $OUTPUT_DIRECTORY/unpackfile$iterator-run$j"
    done
    iterator=$((iterator + 1))
  done
  end=$(date +%s)

  echo "__________________________________________"
  echo "Finished execution in approximately $((end - start)) seconds "
  echo ""
}

# Execute the simulated annealing algorithm and generate output files.
executeSimulatedAnnealing
