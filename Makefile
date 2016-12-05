# Variables 
school = data/school_data.csv
funcs = code/functions/summary_functions.R
eda = code/scripts/eda.R
income = data/Income.csv

.PHONY: all data eda premodel anova ols rf session clean

all: premodel eda anova ols rf

data:
	curl -o $(school) https://ed-public-download.apps.cloud.gov/downloads/Most-Recent-Cohorts-All-Data-Elements.csv

premodel: data/school_data.csv
	Rscript code/scripts/premodeling.R

eda: $(school) $(funcs)
	Rscript $(eda)

anova:
	Rscript code/scripts/anova.R

ols: code/scripts/ols-second-model.R data/Completion_W_A.csv data/Completion_W_B.csv data/Completion_W_H.csv data/Income.csv
				Rscript code/scripts/ols-second-model.R

rf: data/Completion_W_A.csv data/Completion_W_B.csv data/Completion_W_H.csv data/Income.csv
	Rscript code/scripts/random_forest.R

shiny-earnings: 
	Rscript -e "library(shiny); runApp('shiny/funding_type_earning_app.R', launch.browser = TRUE)"

shiny-completion:
	Rscript -e "library(shiny); runApp('shiny/funding_type_completion_app.R', launch.browser = TRUE)"

session:
	bash session.sh
report: 
	cd reports; R CMD Sweave --pdf report.Rnw; rm report-concordance.tex; rm report.aux; rm report.log; rm report.out; rm report.tex; cd ..
slides:
	Rscript -e "rmarkdown::render('slides/slides.Rmd')"

clean:
	rm -f report/report.pdf 



