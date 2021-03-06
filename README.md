# stat159-project3

## Outcome Gaps in Higher Education

#### Authors: Abbey Chaver, Tina Huang, Shirley Jin, and Xiaoqian Zhu

This project researches outcomes, including rates of completion and mean earnings after graduating, for underrepresented students based on school type and instructional expenditures per student. 


The main file directory structure of the project is as follows:

```
stat159-fall2016-project3/
  README.md
  Makefile
  LICENSE
  session-info.txt
  .gitignore
  data/
    summaries/
      ...
    school_data.csv
    ...
  code/
    functions/
      ...
    scripts/
      ...
    tests/
      ...
  images/ 
    ...
  shiny/
    funding_type_earning_app.R
    funding_type_completion_app.R
  report/
    report.Rnw
    report.pdf
    sections/
      ...
  slides/
    slides.Rmd
    slides.pdf
```


The main dataset for this project is `data/school_data.csv`. 

The `code/functions` folder contains functions used to create summaries of the data variables, as well as calculate regression statistics.

The `code/scripts` folder contains the R script files that clean and premodel the data, as well as perform ANOVA, ols regression, and random forest regression upon the data. 

The `code/tests` folder contains test files that check the outputs of the regression statistic functions. 

The `images` folder contains the plots generated by the R files in the code folder.

The `shiny` folder contains two shiny applications with interactive plots. 

The report folder contains the individual sections of the report in the sections folder, as well as the intermediate file `report.Rnw` and the final `report.pdf`.

Finally, the slides folder contains the slides for the presentation of this project.

______

To obtain the files contained in this project on your local machine, type `git clone https://github.com/abbeychaver/stat159-project3.git` into your terminal.

Once you have this directory locally, `cd` into the root directory. Then you can run 	`make` commands to compile the code and generate outputs.


- Run `make data` to obtain the College Score Card data which will be downloaded to `data/school_data.csv`. Note that this file is quite large and cannot be saved onto Github. 
- Run `make` or `make all` to run all the R files to generate the text summaries of all the variables, all the plots, and the final report file.
- Run `make premodel` to clean and separate the data into its relevant .csv files. 
- Run `make eda` to create summaries and plots for the variables in the `school_data.csv` dataset
- Run `make anova` to conduct analysis of variance
- Run `make ols` to fit the ols regression model
- Run `make rf` to generate the random forest models
- Run `make shiny-earnings` to launch the shiny application in the browser that plots the mean earnings gap against school student funding
- Run `make shiny-completion` to launch the shiny application in the browser that plots the completion rate gap against school student funding
- Run `make report` to generate the final `report/report.pdf`. 
- Run `make session` to get information about your current session in `session-info.txt`. 
- Run `make clean` to delete `report/report.pdf`. 
- Run `make slides` to generate the final slide presentation. 

The code in this project is licensed under the Open Source MIT license. The details of this license can be found in the LICENSE file. 

![](https://i.creativecommons.org/l/by/4.0/88x31.png)

This work is licensed under a [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/).

