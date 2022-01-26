main <- function(start = 1, end = 4) {
  source("misc/conditions.r") # conditions.r specify simulation conditions
  REP_PER_CONDITION <- 1000
  SET_PER_CONDITION <- 10
  rep_sets <- 1:SET_PER_CONDITION
  rep_per_set <- 1:(REP_PER_CONDITION/SET_PER_CONDITION)
  
  for (condition_number in start:end) {
    for (rep_set in rep_sets) {
      tictoc::tic()
      print(paste("Starting condition number", condition_number, "rep", rep_set))
      print(conditions[condition_number, ])
      filename <- paste0("piolotsim", condition_number, "-", rep_set, ".csv")
      if (!file.exists(filename)) {
        for (rep in rep_per_set) {
          cat("piolotsim: ", condition_number, "rep set: ", rep_set, "rep: ", rep)
          data <- generate_data(condition_number, rep_set, rep)
          temp <- analyze_data(condition_number, rep_set, rep, data)
          if (rep == 1) {
            out <- temp
          } else {
            out <- rbind(out, temp)
          }
        } # end of for (rep in rep_per_set)

        readr::write_csv(data.frame(out), file = filename)
        print(out)
        tictoc::toc()
      } # end of if (!file.exists(filename))
    } # end of  for (rep_set in rep_sets)
  } # end of for (condition_number in condition_numbers)
} # end of function