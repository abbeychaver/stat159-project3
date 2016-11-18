school = data/school_data.csv
funcs = code/functions/summary_functions.R
eda = code/scripts/eda.R

.PHONY: all data eda

all: eda

data:
	curl -o $(school) https://ed-public-download.apps.cloud.gov/downloads/Most-Recent-Cohorts-All-Data-Elements.csv

eda: $(school) $(funcs)
	Rscript $(eda)



