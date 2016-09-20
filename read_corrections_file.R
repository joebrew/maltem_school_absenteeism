library(readxl)
results_list <- list()
for (i in 1:24){ # not doing row 25, since we don't have corrections for that
  message(paste0('Working on school ', i))
  # Read in the data
  this_school <- suppressMessages(read_excel('corrections/Duplicados_Aug 2016.xls',
                            sheet = paste0('school_', i)))
  this_school <- data.frame(this_school)
  message(paste0('Rows :', nrow(this_school)))
  
  # Identify which row has the correct stuff
  start_here <- which(this_school$NAME == 'NAME') + 1
  # Subset
  this_school <- this_school[start_here:nrow(this_school),]
  # Add to results
  results_list[[i]] <- this_school
}
the_binder <- plyr::rbind.fill
corrections <- do.call('the_binder', results_list)
rm(results_list, the_binder, this_school, i, start_here)
