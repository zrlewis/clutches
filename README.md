# clutches


## Analysis of Plethodon clutch and developmental data

### Authors: Zachary R. Lewis, Brooks G. Mathewson, James Hanken


## Contents

- `analysis.R` The `R` script for generating figures and tables. For Figure 1, to output the inset ("blowup")
 of early stages, un-comment the viewport code and run the relevant code in console directly.
- `clutchData.csv` Dataset on clutch encounters in Massachusetts
- `clutchDataClimate.csv` Climate data
- `staging_at_15C.csv` Staging data for lab-reared clutches

## Dependencies

- `R` packages:
	+ `tidyverse`
	+ `lubridate` 
	+ `ggplot2`
	+ `grid`
	+ `gridExtra`
	+ `broom`

## Running the code

Each time you want to execute an analysis:

- Navigate to the root of the repository, e.g., `clutches`.
- Run the code, e.g., launch `R` and run `source("analysis.R")`.




