main <- function(start = 1, end = 4) {
  #source("misc/conditions.r") # conditions.r specify simulation conditions
  n <- c(70, 600)
  fcor <- c(.7, 1 - 1e-10) # focal trait correlation
  conditions <- tidyr::crossing(n, fcor)
  colnames(conditions) <- c("n", "fcor")
  REP_PER_CONDITION <- 1000
  SET_PER_CONDITION <- 10
  rep_sets <- 1:SET_PER_CONDITION
  reps_per_set <- 1:(REP_PER_CONDITION/SET_PER_CONDITION)
  
  for (condition_number in start:end) {
    for (rep_set in rep_sets) {
      tictoc::tic()
      print(paste("Starting condition number", condition_number, "rep", rep_set))
      print(conditions[condition_number, ])
      filename <- paste0("piolotsim", condition_number, "-", rep_set, ".csv")
      if (!file.exists(filename)) {
        for (rep in reps_per_set) {
          cat("piolotsim: ", condition_number, "rep set: ", rep_set, "rep: ", rep)
          data <- generate_dat (conditions, condition_number, rep_set, rep)
          temp <- analyze_dat (conditions, condition_number, rep_set, rep, data)
          if (rep == 1) {
            out <- temp
          } else {
            out <- rbind(out, temp)
          }
        } # end of for (rep in reps_per_set)

        readr::write_csv(out, file = filename)
        print(out)
        tictoc::toc()
      } # end of if (!file.exists(filename))
    } # end of  for (rep_set in rep_sets)
  } # end of for (condition_number in condition_numbers)
} # end of function