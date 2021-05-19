library(rstudioapi)

# Sandwich -------------

(jobs <- list.files(pattern = "job_", full.names = T))

jobRunScript(jobs[1])
# jobRunScript(jobs[2])
# jobRunScript(jobs[3])