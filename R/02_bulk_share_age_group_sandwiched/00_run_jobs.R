library(rstudioapi)


# Sandwich -------------

(sand_jobs <- list.files(pattern = "job_sand", full.names = T))

jobRunScript(sand_jobs[1])
jobRunScript(sand_jobs[2])
jobRunScript(sand_jobs[3])
jobRunScript(sand_jobs[4])

# Grandsandwich -------------

gsand_jobs <- list.files(pattern = "job_grandsand", full.names = T)

jobRunScript(gsand_jobs[1])
jobRunScript(gsand_jobs[2])
jobRunScript(gsand_jobs[3])
jobRunScript(gsand_jobs[4])
