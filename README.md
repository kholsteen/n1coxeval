**n1coxeval** is a package of functions for use and evaluation of Cox
regression for n-of-1 observational studies of recurrent event triggers.

The script **main.R** uses this package to reproduce the analyses in the
paper.

The package may be installed from Github as follows:

``` r
## Install the package
if(!require("remotes")){
    install.packages("remotes")
}
remotes::install_github("kholsteen/n1coxeval")
```

To reproduce the analyses, first update the following parameters in
**main.R** to fit your computing environment:

``` r
parent_output_dir <- "path/to/your/dir" ## Parent directory for simulation output
run_date <- "todaysdate" ## string subfolder name to create for storing this run of the analysis
num_parallel_workers <- 5 ## number of cores to use for parallel processing
```

Then run the script **main.R.** Expect the full analysis to take up to
12 hours with 5 parallel workers.
