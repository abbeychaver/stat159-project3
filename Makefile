# Variables 
school = data/school_data.csv
funcs = code/functions/summary_functions.R
eda = code/scripts/eda.R
income = data/Income.csv

.PHONY: all data eda premodel anova ols rf shiny-earnings shiny-completion session report slides clean

all: premodel eda anova ols rf shiny-earnings shiny-completion report slides

# Downloads the most recent College Score Card data to data/school_data.csv
data:
	curl -o $(school) https://ed-public-download.apps.cloud.gov/downloads/Most-Recent-Cohorts-All-Data-Elements.csv

# Cleans up raw dataset and divides it into separate csvs
premodel: data/school_data.csv
	Rscript code/scripts/premodeling.R

# Creates summary statistics for some of the relevant variables
eda: $(school) $(funcs)
	Rscript $(eda)

# Runs anova analysis
anova: data/Completion_W_A.csv data/Completion_W_B.csv data/Completion_W_H.csv data/Income.csv
	Rscript code/scripts/anova.R

# Fits ols regression model to the dataset
ols: data/Completion_W_A.csv data/Completion_W_B.csv data/Completion_W_H.csv data/Income.csv
	Rscript code/scripts/ols-second-model.R

# Fits random forest model to other relevant variables in the dataset
rf: data/Completion_W_A.csv data/Completion_W_B.csv data/Completion_W_H.csv data/Income.csv
	Rscript code/scripts/random_forest.R

# Launches shiny app in browser for earnings gap
shiny-earnings: shiny/funding_type_earning_app.R
	Rscript -e "library(shiny); runApp('$<', launch.browser = TRUE)"

# Launches shiny app in browser for completion gap
shiny-completion: shiny/funding_type_completion_app.R
	Rscript -e "library(shiny); runApp('$<', launch.browser = TRUE)"

# Updates information about session in session-info.txt
session: code/scripts/session_info.R
	bash session.sh

# Creates the final report/report.pdf file
report: 
	cd reports; R CMD Sweave --pdf report.Rnw; rm report-concordance.tex; rm report.aux; rm report.log; rm report.out; rm report.tex; cd ..

# Creates the final slides/slides.html file
slides: slides/slides.Rmd
	Rscript -e "rmarkdown::render('slides/slides.Rmd')"

# Deletes the generated report/report.pdf file
clean:
	rm -f report/report.pdf 



