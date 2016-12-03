# Variables 
school = data/school_data.csv
funcs = code/functions/summary_functions.R
eda = code/scripts/eda.R
income = data/Income.csv

.PHONY: all data eda premodel anova ols session clean

all: eda premodel

data:
	curl -o $(school) https://ed-public-download.apps.cloud.gov/downloads/Most-Recent-Cohorts-All-Data-Elements.csv

eda: $(school) $(funcs)
	Rscript $(eda)

premodel: data/combined_data.csv
	Rscript code/scripts/premodeling.R

anova: code/scripts/anova.R data/Completion_W_A.csv data/Completion_W_B.csv data/Completion_W_H.csv data/Income.csv
	Rscript code/scripts/anova.R

ols: code/scripts/ols-second-model.R data/Completion_W_A.csv data/Completion_W_B.csv data/Completion_W_H.csv data/Income.csv
				cd code/scripts; Rscript ols-second-model.R

shiny-earnings: 
	Rscript -e "library(shiny); runApp('shiny/funding_type_earning_app.R', launch.browser = TRUE)"

shiny-completion:
	Rscript -e "library(shiny); runApp('shiny/funding_type_completion_app.R', launch.browser = TRUE)"

session:
	bash session.sh

clean:
	rm -f report/report.pdf report/report.Rnw



