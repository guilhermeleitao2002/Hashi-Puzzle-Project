# Hashi Puzzle Solver

## Project Overview

This project involves implementing a Prolog program to solve Hashi (Bridges) puzzles. A Hashi puzzle is played on an n × m grid where some cells contain islands with a specified number of bridges that must be connected to other islands.

## Puzzle Rules

- Each island has a number indicating how many bridges must connect to it
- Bridges can only be horizontal or vertical
- Maximum of two bridges between any two islands
- Bridges cannot cross other islands or other bridges
- All islands must be connected by bridges in the final solution

## Key Implementation Tasks

The project requires implementing several Prolog predicates to solve the puzzle, including:

1. Extracting islands from puzzle grids
2. Finding neighboring islands
3. Representing puzzle state
4. Creating and validating bridges
5. Tracking island connections
6. Managing completed islands

## Core Functionality

The solver will progressively:
- Identify islands on the grid
- Determine possible bridge connections
- Build bridges between islands
- Verify bridge placement rules
- Track island connection status

## Technical Components

- Input: A 2D grid representing the initial puzzle state
- Each island represented by its position and required bridge count
- State tracking of island connections and remaining bridge requirements
- Bridge creation and validation mechanisms

## Development Focus

- Logical constraint satisfaction
- Grid traversal and island connection algorithms
- State management in Prolog
- Systematic puzzle solving approach

## Project Goals

Create a flexible and efficient Prolog implementation that can solve Hashi puzzles by methodically applying logical rules and constraints.
