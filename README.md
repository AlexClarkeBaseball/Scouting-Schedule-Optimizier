# Scout Schedule Optimizer

## Overview

The Scout Schedule Optimizer is an R Shiny application designed to optimize baseball scouting schedules. This application uses a practical, yet flawed two-stage optimization approach to generate the most effective scouting schedule based on player roles, game values, and various constraints.

## Features

- **Two-Stage Optimization Process**:
  - Stage 1: Linear programming optimization to create an initial optimal schedule
  - Stage 2: Applies the Repeat Evaluation Decay to all players and adjusts Game Scores after the schedule has been finalized.

- **Game Value Calculation**:
  - Accounts for player roles, playing probabilities and injury risks
  - Implements "times seen decay" to prioritize new player viewings
  - Handles doubleheader games appropriately

## Project Structure

- **app.R**: Main Shiny application containing the UI, server logic, and Stage 1 optimization
- **stage_two.R**: Helper functions for Stage 2 optimization and repeat evaluation decay

## Key Functions

### stage_two.R

- **processFinalSchedule()**: Processes the optimal schedule from Stage 1, marks doubleheader games, and recalculates game values without recalculating the optimal schedule.
- **updatePlayerAppearances()**: Tracks player appearances throughout the season for accurate "times seen" calculations
- **calculateAccurateGameScore()**: Calculates the precise game value including player bonuses and times seen decay

## Usage

1. Download the two data files
2. Run stage_two.R then run app.R
3. Execute the optimization to generate an optimal scouting schedule for the players you selected.
4. View and export the results

## Requirements

- R with Shiny package installed
- Other R packages:
  - Shinydashboard
  - lpSolve
  - dplyr 
  - DT
  - Readxl

## Notes

- Doubleheader games have special handling to prevent double-counting appearances for pitchers 


