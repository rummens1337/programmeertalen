#!/bin/bash

# Declare global variables in UPPER_SNAKE_CASE.
VERBOSE="-verbose 1"
NR_ITEMS="-nr_items 100"
DIMENSIONS="-dimensions 2"
SOLVER_NAME="-solver simulated_annealing"
NR_ITERATIONS="-nr_iterations 10000"
OUTPUT_DIRECTORY="team/sa-logs"
NUMBER_OF_UNPACKS=2
NUMBER_OF_RUNS=0
NUMBER_MAX_TEMPERATURES=0
ALL_MAX_TEMPERATURES=(filler 0 1 10 100 1000 1000)

changeDirectory(){
  # Change working directory and compile Main.java.
  cd ../ && javac Main.java
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

readMaxTemperatures(){
  while true
  do
    # Prompt user for input
    read -ep "Enter the amount of maximum temperatures (integer, f.e. goes from 0 to 1 to 10 to 100 etc, 0-6): " NUMBER_MAX_TEMPERATURES

    # Check if input is indeed an integer between 0 and 6
    if [ "$NUMBER_MAX_TEMPERATURES" ] && [ "$NUMBER_MAX_TEMPERATURES" -eq "$NUMBER_MAX_TEMPERATURES" ] 2>/dev/null; then
        echo "Value entered: $NUMBER_MAX_TEMPERATURES"
        break;

      elif [ "$NUMBER_MAX_TEMPERATURES" -gt 6] || [ "$NUMBER_MAX_TEMPERATURES" -lt 0]; then
        echo "Please enter an integer in the given range, hint: 0-6."

      else
        echo "Please enter an integer in the given range, hint: 0-6."
    fi
  done
}

executeSimulatedAnnealing(){
  echo "__________________________________________"
  echo "Executing the simulated_annealing algorithm. "
  echo ""

  start=$(date +%s)
  # Iterate for the amount of different max temperatures, and grabs the actual max temperatures out of an array.
  for (( i=1; i < NUMBER_MAX_TEMPERATURES+1; i++ ))
  do
    NR_MAX_TEMPERATURES="-max_temperature $ALL_MAX_TEMPERATURES[$i]"

    # Run each unpack number five times.
    for (( j=1; j < NUMBER_OF_RUNS+1; j++ ))
    do
      java Main $VERBOSE $NR_ITEMS $DIMENSIONS $SOLVER_NAME $NR_ITERATIONS $NUMBER_OF_UNPACKS $NR_MAX_TEMPERATURES> "$OUTPUT_DIRECTORY/unpackfile$i-run$j.txt"
      echo "Created file: $OUTPUT_DIRECTORY/unpackfile$i-run$j"
    done
  done
  end=$(date +%s)

  echo "__________________________________________"
  echo "Finished execution in approximately $((end-start)) seconds "
  echo ""
}

# Execute the functions defined above.
changeDirectory
readRuns
readMaxTemperatures
executeSimulatedAnnealing
