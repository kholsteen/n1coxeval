###################################
# n-of-1 cox regression evaluation
# Katherine Holsteen
# January 2020
###################################
##### SETUP ####
# Modify these parameters
## Parent directory for simulation output
parent_output_dir <- "C:/Users/Katherine Holsteen/Documents/My Documents/Stanford Medicine Box/Code/n1-power/data" #"path/to/your/dir"
## string subfolder name to create for storing this run of the analysis
run_date = "2020-01-14"
## number of cores to use for parallel processing
num_parallel_workers = 5
## number of participants to simulate; decrease for testing
n_subj = 1000

# Switch statements for analysis steps
generate_data <- 1
estimate_models <- 1
summarize_results <- 1

# Create directories
run_dir = file.path(parent_output_dir, run_date)
fig_dir <- file.path(run_dir, "figures")
dir.create(run_dir, showWarnings = FALSE)
dir.create(fig_dir, showWarnings = FALSE)

# Install & library necessary packages
installed_packages <- utils::installed.packages()
for (pkg in c("doParallel", "n1coxeval", "future")) {
  if (!(pkg) %in% installed_packages) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
}

# Set up parallel computing
cl <- parallel::makeCluster(num_parallel_workers)
doParallel::registerDoParallel(cl)
foreach::getDoParWorkers()

# Set random seed for exact reproducibility
set.seed(1517)

###### ITERATE OVER THE MAJOR COMPONENTS ####

for (simmv in list(
  c(TRUE, FALSE),
  c(TRUE, TRUE),
  c(FALSE, FALSE))) {

  sim = simmv[1]
  mv = simmv[2]

  results_dir <- file.path(run_dir, paste0("sim=", sim, "&mv=", mv))
  dir.create(results_dir, showWarnings = FALSE)

  ##### CREATE PARAMS ######
  tictoc::tic("Create parameters")
  if (sim) {
    if (!mv) {
    n1coxeval::create_params(results_dir,
                  n_subj = n_subj,
                  n_vars = 1,
                  freq = 0.2,
                  var_types = c("b", "c"),
                  wb_shape = 2,
                  n_imp = 1,
                  beta_imp = log(c(1,2,3,4,8)))
    } else {
      n1coxeval::create_params(results_dir,
                             n_subj = n_subj,
                             n_vars = 25,
                             freq = NA,
                             var_types = "c",
                             wb_shape = 2,
                             n_imp = 4,
                             beta_imp = log(c(2,3,4,8)))
    }
  }
  tictoc::toc()

  ##### ITERATE OVER PARAMETERS ######
  tictoc::tic("Iterate over parameters")
  if (sim) {
    params <- readRDS(file.path(results_dir, "params.rds"))
    p_list <- c(1:nrow(params))
  } else { p_list <- 1 }

  for (p in p_list) {

    cat(paste0("\n\nparam row #", p, " ----- Starting @ ", lubridate::now(), " ------\n"))
    if (sim) { param_row = params[p, ] } else {   param_row = NA }
    beta <- n1coxeval::create_named_beta(sim = sim, param_row = param_row)
    xvars <- names(beta)

    if (generate_data == 1) {
      data_list <- n1coxeval::generate_data_wrapper(
                              sim = sim,
                              param = param_row,
                              x_vars = xvars,
                              p = p)

      saveRDS(data_list, file.path(results_dir, paste0("data_list", p, ".rds")))

    }

    if (estimate_models == 1) {

      data_list <- readRDS(file.path(results_dir, paste0("data_list", p, ".rds")))
      #### SET THE PARAMETERS FOR ESTIMATION ####
      e_params <- n1coxeval::estimation_params(sim = sim, mv = mv)

      ## Dummy version of params for the case study.
      if (!sim) {
        n1coxeval::create_params(results_dir,
                               n_subj = length(data_list),
                               wb_shape = max(e_params$n.events),
                               n_vars = length(beta),
                               var_types = NA,
                               beta_imp = NA,
                               sim = FALSE)
        params <- readRDS(file.path(results_dir, "params.rds"))
      }

      cat(paste0("length of data_list = ", length(data_list), "; length of e_params = ", nrow(e_params), "\n"))

      ## Mapping over vars, ids, n_events within this parameter specification dataset
      ## Result will be a tbl with nrows = n.subjects*nrow(e_params)

      #### MAP OVER DATA LIST ####
      tictoc::tic(paste0("map over data list for param row #", p))
      result <- foreach::foreach(data = iterators::iter(data_list),
        .combine = rbind,

        .packages = c("n1coxeval"),
        .verbose = FALSE) %dopar% {
          i = nrow(e_params)

          toreturn <- purrr::map_dfr(names(beta), function(var) {

              purrr::pmap_dfr(
                list(n = e_params[1:i,]$n.events,
                   u = e_params[1:i,]$prior_hr_ub,
                   r = e_params[1:i,]$prior_normal),

                n1coxeval::cox_model_wrapper,
                data = data,
                var = var,
                error_dir = results_dir)

            })

          toreturn
        }
      tictoc::toc()
      #### Save Results ####
      cat(paste0("length of result = ", nrow(result), "\n"))
      saveRDS(result, file.path(results_dir, paste0("result", p, ".rds")))
      cat(paste0("result", p, ".rds saved @ ", results_dir, "\n"))

    }


  }

  tictoc::toc()

  if (summarize_results == 1) {
    ##### SUMMARIZE RESULTS ####
    future::plan(multisession)
    tictoc::tic("Summarize results")
    res <- n1coxeval::summarise_results_wrapper(results_dir = results_dir,
                                              rowwise = FALSE,
                                              mv = mv,
                                              sim = sim,
                                              row_errors = FALSE)
    saveRDS(res, file.path(results_dir, "result-summary.rds"))
    cat(paste0("result-summary.rds saved @ ", results_dir, "\n"))
    tictoc::toc()
  }
  cat(paste0("----- Finishing @ ", lubridate::now(), " ------\n"))
}

#### PLOT RESULTS ####
future::plan(multisession)
n1coxeval::figures_univ(run_dir)
n1coxeval::figures_mv(run_dir)
n1coxeval::figures_cs(run_dir)
tictoc::tic()
n1coxeval::figures_supp(run_dir)
tictoc::toc()










