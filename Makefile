school = data/school_data.csv
funcs = code/functions/summary_functions.R
eda = code/scripts/eda.R

.PHONY: all data eda premodel ols

all: eda

data:
	curl -o $(school) https://ed-public-download.apps.cloud.gov/downloads/Most-Recent-Cohorts-All-Data-Elements.csv

eda: $(school) $(funcs)
	Rscript $(eda)

premodel: data/combined_data.csv
	Rscript code/scripts/premodeling.R

ols: code/scripts/ols-second-model.R data/Completion_W_A.csv data/Completion_W_B.csv data/Completion_W_H.csv data/Income.csv
				cd code/scripts; Rscript ols-second-model.R



