# clutches


## Analysis of _Plethodon cinereus_ clutch and developmental data

### Authors: Zachary R. Lewis, Brooks G. Mathewson, James Hanken


## Contents

- [`analysis.R`](./analysis.R) The `R` script for generating figures and tables. For Figure 1, to output the inset ("blowup")
 of early stages, un-comment the viewport code and run the relevant code in console directly.
- [`clutchData.csv`](./clutchData.csv) Dataset on _Plethodon cinereus_ clutch encounters in Massachusetts
- [`climateData.csv`](./climateData.csv) Climate data
- [`environmentalAnalysis.R`](./environmentalAnalysis.R) The `R` script for assessing the effects of precipitation and relative humidity on clutch size.
- [`staging_at_15C.csv`](./staging_at_15C.csv) Staging data for lab-reared clutches of _P. cinereus_

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

- Clone the repository, e.g., `git clone https://github.com/zrlewis/clutches.git`
- Navigate to the root of the repository, e.g., `cd clutches`.
- Run the code, e.g., launch `R` and run `source("analysis.R")`.
- The figures and tables will be generated in your repository.
- The precipitation and relative humidity analyses can be replicated by running `source("environmentalAnalysis.R")` in `R`.
