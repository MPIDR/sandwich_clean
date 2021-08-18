library(rstudioapi)

# Sandwich -------------

(jobs <- list.files(pattern = "job_", full.names = T))

jobRunScript(jobs[1])