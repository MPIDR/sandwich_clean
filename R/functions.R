
# *~^**~^**~^**~^**~^**~^**~^*#
# Code by                     #
# Diego Alburez-Gutierrez     #
# gatemonte@gmail.com         #
# @d_alburez                  #
# unless stated otherwise.    #
# Last edited 20200312        #
# GNU GENERAL PUBLIC LICENSE  #
# Version 3, 29 June 2007     #
# *~^**~^**~^**~^**~^**~^**~^*#
#       \   ^__^ 
#        \  (oo)\ ________ 
#           (__)\         )\ /\ 
#                ||------w|
#                ||      ||


age_at_death_all <- function(
  death_l
  , p
  , parallel = T
  , numCores = 8
  , worker_function = worker_get_ages
  , adjust_own_mortality = F
) {
  
  if(!is.data.table(p)) p <- as.data.table(p)
  p <- p[, c("profileid", "birth_year", "death_year")]
  
  if(!parallel) {
    # Option A. Non parallelised
    print("Finding data (non-parallelised).")
    out_l <- lapply(death_l, worker_function, p, adjust_own_mortality)
  } else {
    # Option B. Parallelise using sockets for Windows
    # http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
    # browser()
    
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    clusterExport(
      cl
      , varlist = c("death_l", "p")
      , envir = environment()
    )
    
    # Load packages
    clusterEvalQ(cl, library(data.table))
    
    print('Start computations...')
    
    out_l <- parLapply(cl, death_l, worker_function , p, adjust_own_mortality) 
    
    stopCluster(cl)
  }
  
  return(out_l)
  
}

aggregate_matrix<- function(mat, by = "year", export = F, name_out = "women"){
  # browser()
  # Aggregate death loss by mother's cohort
  
  # I now have a df with the cummulative number of deaths for each mother
  # With a column indicating mother's birth cohort
  # Now, it is possible to get the average by mothers birth cohort
  
  # There are two ways of doing this, either
  # For 1-year long birth cohorts, but there can be few observations for earlier years
  # Another option is to group observations in 10-year long cohorts
  
  if(by == "year"){
    deaths_l <- split(
      select(mat, -ego, - birth, - cohort)
      , mat$birth
    ) 
  } else {
    deaths_l <- split(
      select(mat, -ego, - birth, - cohort)
      , mat$cohort
    )
  }
  
  # To see how many observations there are per cohort
  # unlist(lapply(deaths_l, nrow))
  
  # Get average over cohorts
  # This gets the mean for every column-age of the mother. 
  # In principle, it means that mothers who had died at a given age are
  # ignored from the denominator, which is good.
  deaths_mean <- data.frame(do.call(rbind,lapply(deaths_l, colMeans, na.rm = T)))
  
  deaths_mean$mother_cohort <- names(deaths_l)
  
  summary_df <- 
    reshape2::melt(deaths_mean, id = "mother_cohort") %>% 
    mutate(
      mother_cohort = as.character(mother_cohort)
      , variable = as.character(variable)
      , age = as.numeric(gsub("^X", "", variable))
      # THis is needed since the matrix starts at age 0, wichih in the minimum value of the age range
      , age = age + min(age_range) - 1
    ) %>% 
    select(mother_cohort, age, value) %>%
    arrange(mother_cohort, age)
  
  # 2.5. Export
  if(export) {
    # browser()
    nam <- paste0("../../Data/estimates/out_child_loss_",name_out,"_", by, ".csv")
    write.csv(summary_df, nam, row.names = F)  
    
    print(paste0(nam, " saved!"))
  }
  
  return(summary_df)
  
}

aggregate_matrix_from_list <- function(mat_name, by = "year", name_out = "women", export = F){
  
  # seed <- str_extract(mat_name, "[0-9]+")
  
  m_name <- gsub(".csv","", gsub("../../Data/estimates/child_loss_all_women_", "", mat_name))
  
  country <- tolower( gsub("_[0-9]+", "", m_name) )
  seed <- gsub("[A-z]+_", "", m_name)
  
  print(paste0("Processing file ", m_name))
  
  aggregate_matrix(
    # Read data
    mat = data.table::fread(mat_name, stringsAsFactors = F)
    , by = by
    , export = export
    , name_out = paste0(name_out, "_", country, "_", seed)
  ) %>% 
    mutate(
      seed = seed
      , country = country
    )
}


# Apply cohort life tables to real-life populatinos
# with the intention of getting the lx column
# where radices are the initial size of birth cohorts 
# of women using wpp data
apply_lt2 <- function(female_births, LTCF, cohorts = 1950:2000, numCores) {
  
  countries <- unique(LTCF$Country)
  
  cl <- makeCluster(numCores)
  
  clusterExport(
    cl
    , varlist = c("countries", "cohorts", "female_births", "LTCF", 'lt_mx')
    , envir = environment()
  )
  
  clusterEvalQ(cl, {
    library(dplyr)
    library(data.table)
  })
  
  print(system.time(
    est_l <- parLapply(cl, countries, worker_apply_lt2, countries, cohorts, female_births, LTCF) 
  ))
  
  stopCluster(cl)
  
  # est_l <- lapply(countries, worker_apply_lt)
  
  data.frame(rbindlist(est_l, use.names = T))  
  
}


asfr_load_and_clean <- function(){
  
  asfr_obs <- readxl::read_xlsx(
    path ="../../Data/wpp_data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx"
    , sheet = "ESTIMATES"
    , skip=16
  )
  
  asfr_pred_medium <- readxl::read_xlsx(
    path ="../../Data/wpp_data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx"
    , sheet = "MEDIUM VARIANT"
    , skip=16
  )
  
  fert_df <- bind_rows(
    asfr_obs
    , asfr_pred_medium
  )
  
  new_names <- c("index", "variant", "country", "notes", "country_code", 
                 "type","parent_code", "year", "X15.19", "X20.24", 
                 "X25.29", "X30.34", "X35.39", "X40.44", "X45.49")
  
  # Change colnames
  colnames(fert_df) <- new_names
  
  fert_df[fert_df == "..."] <- NA
  
  fert_5_5 <- 
    fert_df %>% 
    filter(type %in% "Country") %>% 
    select(country, year, dplyr::starts_with("X")) %>% 
    reshape2::melt(id = c("country", "year")) %>% 
    select(country, year, age = variable, value) %>% 
    mutate_all(list(as.character)) %>% 
    # Remove unwanted punctuation
    mutate(
      country = fix_un_countries(country)
      , age = gsub("\\.|\\?", "-", age)
      , age = gsub("X", "", age)
      , value = as.numeric(value)
    ) %>% 
    arrange(country, year, age) 
  
  return(fert_5_5)
  
}

# a function to convert socsim months to calendar years
asYr <- function(x) {
  return(trunc(FinalSimYear - (endmo - x)/12) +1)
}

# a function to convert socsim months to calendar years
asYr2 <- function(x, FinalSimYear, endmo) {
  return(trunc(FinalSimYear - (endmo - x)/12) +1)
}

# Converts year to socsim month and 
# Returns START month of that year
asMonth <- function(x, FinalSimYear, endmo) {
  return(endmo - (FinalSimYear - 2199)*12)
}

# For mapping
assign_contested_iso3_countries <- function(df) {
  df %>%  
    mutate(
    country = recode(country
           , GRL = "DNK"
           , "SDS" = "SSD"
           # Somaliland is part of Somalia
           , "SOL" = "SOM"
           # Kosovo is part of Serbia
           , "KOS" = "SRB"
           # Recode Gaza and West Bank as palestine
           , "GAZ" = "PSE"
           , "WEB" = "PSE"
           # Northern Cyprus is part of Cyprus
           , "CYN" = "CYP"
           # Western Sahara is part of Morocco
           , "SAH" = "MAR"
    )
  )
          
}


# This can be merged with untion in global
# Get psuedo census of population alive in given year or period
# from genealogical data
census_socsim <- function(df, y, return_ids = T, group_by_sex = F, group_by_age_sex = F) {
  
  df$census <- y
  
  df$death_temp <- df$death
  
  year_df <- 
    df %>% 
    dplyr::filter(birth <= y & death_temp >= y)
  
  if(return_ids) {
    out <- year_df$profileid
  } else {
    if(group_by_sex) {
      out <- year_df %>% 
        dplyr::count(gender, census)
    } else if(group_by_age_sex) {
      out <- year_df %>% 
        dplyr::mutate(
          age_at_census = census - birth
        ) %>% 
        dplyr::count(gender, age_at_census, census) %>% 
        tidyr::complete(gender, age_at_census, census, fill = list(n = 0))
    }
    else{
      out <- year_df
    }
    
  }
  
  return(out)
  
}

# Adapted from Rmisc CI
confidence_interval <- function(x, type = "lower", ci = 0.95) {
  a <- mean(x)
  s <- sd(x)
  n <- length(x)
  error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n)
  
  out <- switch (type,
                 upper = a + error
                 , mean = a
                 , lower = a - error
  )
  return(out)
}

cummulative_no_deaths <- function(age_at_death, age_range) {
  cumsum(age_range %in% age_at_death)  
}

cum_events <- function(mother_age_at_child_death_list, age_range, FinalSimYear, endmo, parallel = T, numCores = 8) {
  
  row_raw <- rep(0, length(age_range))
  
  if(!parallel){
    out_l <- lapply(mother_age_at_child_death_list, worker_cum_events, row_raw, age_range, FinalSimYear, endmo)
  } else {
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    clusterExport(
      cl
      # , varlist = c("mother_age_at_child_death_list", "row_raw")
      , varlist = c("row_raw", "age_range", "FinalSimYear", "endmo")
      , envir = environment()
    )
    
    print('Start computations...')
    
    out_l <- parLapply(cl, mother_age_at_child_death_list, worker_cum_events, row_raw) 
    
    stopCluster(cl)
  }
  
  
  return(out_l)
  
}

estimate_child_death_socsim <- function(opop, sim_name, FinalSimYear, endmo, age_range, account_for_mortality_women = T, numCores){
  # browser()
  
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print("Working with new simulation, seed:")
  print(sim_name)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  
  
  EndYr<-endmo:(endmo-11)
  
  # For created grouped cohorts
  min_year <- 1650
  max_year <- 2150

  by <- 10

  cohorts_yearly <- min_year:(max_year-1)
  cohort_breaks <- seq(min_year, max_year, by)

  cohort_labels <-
    paste(cohort_breaks, cohort_breaks + by, sep = "-")[1:length(cohort_breaks)-1]
  
  # 1. Format socsim to llok like register 
  
  prof <- 
    opop %>% 
    mutate(
      ego_birth_year = asYr2(dob, FinalSimYear, endmo)
      , cohort = cut(ego_birth_year, cohort_breaks, include.lowest = TRUE,
                     right = F, labels = cohort_labels)
      # FOr people who were still alive at the end of the simulation, 
      # assign NA
      , dod = ifelse(dod == 0, NA, dod )
      , ego_death_year = asYr2(dod, FinalSimYear, endmo)
      , age_at_death = ego_death_year - ego_birth_year
      # We need to be careful with this again, but it works for now:
      # assign placeholder age for live women
      # , age_at_death = ifelse(age_at_death == 0, NA, age_at_death)
      , mom = ifelse(mom == 0, NA, mom)
      , pop = ifelse(mom == 0, NA, pop)
    ) %>% 
    select(ego = pid, ego_birth_year, ego_death_year, age_at_death, everything())
  
  # 2. List of mother's child ids 
  
  # # List containing named elements, with names being women' ID.
  # Each element contains a vector with the ids of that woman's children
  
  # Tidyverse alternative
  
  children_of_mothers_df <- 
    opop %>% 
    group_by(mom) %>% 
    summarise(children = paste(pid, collapse = ";")) %>% 
    ungroup %>% 
    filter(mom > 0) 
  
  # As list
  children_of_mothers <- 
    children_of_mothers_df %>% 
    pull(children) %>% 
    # Save as list
    lapply(., function(children) unlist(strsplit(children, ";")) )
  
  names(children_of_mothers) <- children_of_mothers_df$mom
  
  # This is for mothers now, I need to add childlress women
  # to make this 'children of women':
  
  # These are women
  women <- 
    opop %>% 
    filter(fem == 1) %>% 
    pull(pid)
  
  # Get all those who ever were mothers
  
  mothers <- 
    opop %>% 
    # filter(fem == 1) %>% 
    filter(mom > 0) %>% 
    pull(mom) %>% 
    unique() %>% 
    sort()
  
  # who were never reported as a mothers:
  
  childless_women <- women[!women %in% mothers]
  
  # None of these are included in the list
  sum(names(children_of_mothers) %in% childless_women) == 0
  
  # But they should be, so inlcude them via a list element per women
  # containing nothing to represent that this woman never had a child
  
  children_of_childless_women <- rep(list(NA), length(childless_women))
  names(children_of_childless_women) <- childless_women
  
  # Append list
  
  children_of_women <- c(children_of_mothers, children_of_childless_women)
  
  
  # Checks!
  length(mothers) == length(children_of_mothers)
  length(childless_women) == length(children_of_childless_women)
  
  # ~~~~ Estimate values  
  
  # This renaming is just to keep consistent with the register scripts
  kids_of_m <- children_of_women
  
  # Keep as vector of children ids
  children_ids <- unlist(kids_of_m)
  
  # This is people for whom the mother id is known
  # and whose age at death can be estimated (or are still alive)
  length(children_ids)
  
  # 1.2. Dates of death of children per woman
  
  # To see number of kids per women
  elements <- lapply(kids_of_m, length)
  table(unlist(elements))
  
  # Keep as vector of women ids
  women_ids <- rep(names(kids_of_m), elements)
  
  # DEPRECATED 20200219 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # SEEMS LIKE kids_deths was estimating the same as ego_death 
  # below. I chose the keep the later since it's more tidy
  # The data is retrieved from the original df
  # which contains all rows, unfiltered.
  # In this DF, NA represents either an unknown death or the death date
  # of a person who is still alive
  # kids_deaths <- births$child_death_year[match(kids, births$child)] 
  
  # I match it to births df and not births_complete since the latter
  # 'artificially' takes the date of death of live ego's to be 2999
  # while births assigns a more reasonable NA value to them.
  # Nonetheles, for the sake of completion, this is the alternative
  # way of getting this values and then assigning NA to egos 'alive':
  # kids_deaths <- births_complete_dates$death[match(kids, births_complete_dates$ego)] 
  # kids_deaths[kids_deaths == 2999] <- NA
  
  # Examine date deaths of children
  # This seems realistic but there is worrying fall in number of registered cases in the mid-fifties
  # followed by no deaths until 1961, which is expected given change of registers
  # The 'new registers' also become evident after 1950, which cover the whole of Sweden
  # table(kids_deaths)
  # hist(kids_deaths) 
  # 
  # names(kids_deaths) <- women_ids 
  # head(kids_deaths)
  # Again, in this vector, NA represents either an unknown death or the death date
  # of a person who is still alive
  # sum(is.na(kids_deaths))
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # 1.3. Child birth and death dates
  
  # Here, I get the birth dates from prof but could also get it from births or 
  # births_complete_dates
  

  # 20200612
  # Remember that 
  # I assigned NA as death date for women who were still alive when the simulation ended
  
  rows_women <- match(women_ids, prof$ego)
  rows_ego <- match(children_ids, prof$ego)
  
  # There should be a number of non-matches, representing
  # the non-existing children of childless women
  rows_ego %>% is.na %>% sum == length(children_of_childless_women)
  
  women_birth <- prof$ego_birth_year[rows_women]
  ego_death <- prof$ego_death_year[rows_ego]
  
  rm(rows_women)
  rm(rows_ego)
  
  # Check results:
  # THere should be some kids without date of birth or death since these are 
  # the non-existing children of childless women
  # There are more NA values than non-existing children since people who were alive 
  # at start of simulation were also coded as dod = NA
  ego_death %>% is.na %>% sum == length(children_of_childless_women) + sum(is.na(prof$dod))

  # Now, code those with unrealistically high dates of birth as NA
  # since they are currently alive (see comment above)
  # ego_death[ego_death == 2999] <- NA
  
  # 1.4. Estimate woma's age at child death
  
  # Now do the arithmetic by substratcing the date of death of the child from the womens date of birth
  # At this point, each obsevation refers to an ego-child and the value represents
  # the age of the women when this child died. The names of the 
  
  # To recap, there are no complications with the women_birth year
  # For children, those still alive are coded as ego_death = NA
  # Here is good that ego_death is NA if ego survived the last simulation
  # as this means that it will produce an NA value for the mother's age at the time of this child's death
  # Otherwise, it would produce an unrealistic estimate by assigning an age at death for women whose children
  # never really died. Really, this will only affect a small group of women who will probably be excluded
  # from the analysis anyway, which by 20200612 only includes up to cohort 2040.
  # For the covid project, it's irrlevant.
  women_age_at_child_death <- ego_death - women_birth
  # names(women_age_at_child_death) <- children_ids
  
  attr(women_age_at_child_death, 'ego') <- children_ids
  attr(women_age_at_child_death, 'women') <- women_ids
  
  # There are many women aged 100+ at their child's death, which is fine since we have not yet accounted for 
  # the mortality of women
  # This explains why there are so many values which are impossibly high
  # hist(women_age_at_child_death)
  
  # Save as a list
  # where each element is a women containing the ages at which she experienced the death 
  # of a child
  # names(kids_deaths) refers to the id of the women
  # women_age_at_child_death_list2 <- split(women_age_at_child_death, names(kids_deaths))
  women_age_at_child_death_list <- split(women_age_at_child_death, attr(women_age_at_child_death, 'women'))
  
  # Note that only fewer cases remain. These are womens with known children who
  # in turn have known dates of birth and death (or are alive)
  length(women_age_at_child_death_list)
  # The total number of children considered is
  length(women_age_at_child_death)
  
  # 2. Matrix of cumulative child loss by age
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Get a matrix where rows represent individual womens
  # and columns women's age
  # Cells are the cummulative number of children lost to each women at a 
  # given age
  
  # ! Long function 
  # child_loss object, takes like 10-20 min to create in a serial way
  # but about 30 sec using 24 cores
  
  # Ignore a warning " number of items to replace is not a multiple of replacement length"
  # if it is displayed once. This is just once strange case where all deaths happened at age 1024 
  # or something like this.
  
  # A more serious question is how to deal with missing values (ie maternal ages at death}.
  # Currently, these cases are simply dropped from the analysis, meaning that we could be 
  # underestimating the number of child deaths.
  
  child_loss <- cum_events(
    mother_age_at_child_death_list = women_age_at_child_death_list
    , age_range = age_range
    , FinalSimYear = FinalSimYear
    , endmo = endmo
    , parallel = F
    # IN MONA detectCores() == 40
    # using 24 core,s this takes about 30 sec
    , numCores = numCores
  ) 
  
  # In this df, each row is a women
  child_loss_df <- data.frame(do.call(rbind, child_loss))
  
  # Note that parallelised functions might change the order of the data, 
  # so that the women's id will not necesarilly be the same as the names
  # of the elements in the list that was fed to the parallelised function
  # which is why, instead of doing this:
  # child_loss_df$ego <- as.numeric(names(women_age_at_child_death_list)) 
  # I'd rather do:
  child_loss_df$ego <- as.numeric(rownames(child_loss_df))
  # you can always check with:
  identical(rownames(child_loss_df), names(women_age_at_child_death_list))
  
  # Get birth data for each woman-row
  
  # This is merged with births_complete_dates because this df, which was derived from the 
  # prof df, includes birth cohort
  # In practice, mathing to any of the three original dfs is irrelevant
  # since birth data is the same across them all
  
  cd_matrix <- merge(
    child_loss_df
    , prof %>% select(ego, birth = ego_birth_year, cohort) 
    , by = "ego"
    , all.x = T
  ) %>% 
    select(dplyr::starts_with("X"), everything()) %>% 
    mutate(ego = as.numeric(ego)) 

  
  # child_loss_df  %>% arrange(ego) %>%  utils::View("child_loss_df")
  # cd_matrix %>% arrange(ego) %>%  head %>% utils::View("cd_matrix")
  
  if(account_for_mortality_women){
    # print("Accounting for mortality of women")
    # 3. Account for women's mortality ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Assign NA to cells in women/rows if the women had already died 
    # when she reached a certain age.
    # For this, we get a df with death daes of mothesr in df
    
    # 3.1. df with womens dates of birth
    
    # DEPRECATED 20200219 ~~~~~~~~~~~
    # Since it didnt' really make difference to create this df
    # This will be used later
    # women_deaths <- births_complete_dates[!duplicated(births_complete_dates$ego), c("ego", "age_at_death")]
    # nrow(births_complete_dates) / nrow(women_deaths) == 1
    # ~~~~~~~~~~~~~~
    
    # Now we DO want women that survived the simulation to have an actual year of death
    # So no assign the last year of the simulation as the death year for women
    # as otherwise they would PROBABLY never die. 
    women_deaths <- 
      prof[, c("ego", "age_at_death")] %>% 
      mutate(age_at_death = ifelse(is.na(age_at_death), FinalSimYear, age_at_death))
    
    position_women_death <- 
      merge(
        # in this df each ego/row represents a women
        cd_matrix %>% select(ego)
        , women_deaths
        , by = "ego"
        , all.x = T
      ) %>% 
      pull(age_at_death)
    
    sum(is.na(position_women_death)) == 0
    
    
    # DEPCRECATED
    # Deal with NA values
    # Now, basically treating them as though they were alive
    # This can be fixed and checked later on
    # position_women_death[is.na(position_women_death)] <- max(age_range)
    
    # 3.2. Deal with NA values
    
    # There are two problems here. 
    # One is women who are still alive, whose age is 2999 currently
    # Another is women whose age at death is still too high (e.g.)
    # In thi case, this doesn't matter as it is only to determine
    # if a woman was still alive when a given child died
    
    # For womens with reported age too high, assign highest possible age
    # Since we are only considering
    # This shouldn't be an issue in SOCSIM, but make sure anyway
    position_women_death[position_women_death > max(age_range)] <- max(age_range)
    
    # For womens with reported age 0, assign lowes possible age
    table(position_women_death)
    # Basically, we need to recode all those that died before age 16, which is when our
    # matrix of cumulative child deaths starts
    position_women_death[position_women_death < min(age_range)] <- min(age_range)
    
    # Adjust position in matrix since the lowest values corresponds
    # to 16
    position_women_death <- position_women_death - min(age_range) + 1
    
    # Let's assume that if a women dies at age a, she still gets to experience the deaths 
    # of her children that happen at age a
    # A more precise way of doing this, which may be needed for Covid, is to re-do the estimation
    # on a monthly basis not on a yearly basis!!
    
    # Now, for every row of cd_matrix, all cells that are to the right
    # of the cell/age at which a given women died must by filled with NA
    
    m <- max(age_range) - min(age_range) + 1
    
    # Save df as a vector by rows
    deaths <- c( t(cd_matrix[ , 1:length(age_range)]) ) 
    
    # Now, determine which positions need to be replaced by NA
    
    rep <- 1:length(deaths)
    
    rep_cut <- cut(rep, seq(0, length(deaths), length(age_range)))
    
    rep_l <- split(rep, rep_cut)
    
    length(rep_l) == nrow(cd_matrix)
    
    # For each element of the list ('row'), detemrine which positions should be NA
    
    to_replace_with_NA <- unlist( 
      lapply(1:length(rep_l), na_position, rep_l, position_women_death, m) 
    )
    
    # Replace identified positions with NA in vector/matriz of deaths
    
    deaths[to_replace_with_NA] <- NA
    
    # Create matrix from Vector again
    
    deaths_df <- data.frame(matrix(deaths, ncol = length(age_range), byrow = T), stringsAsFactors = F)
    nrow(deaths_df) == nrow(cd_matrix)
    # deaths_df %>% head %>% View
    
    # Asign new matrix back to original data frame of death dates
    
    cd_matrix_final_women <- cd_matrix
    
    cd_matrix_final_women[ , 1:length(age_range)] <- deaths_df
    
    # cd_matrix_final_women %>% arrange(ego) %>% head %>%  utils::View()
    
    rm(rep_l)
    rm(children_of_women)
    
  } else if(!account_for_mortality_women){
    # print("NOT accounting for mortality of women")
    cd_matrix_final_women <- cd_matrix
  }
  
  # 6.3. Export 
  
  # Remember -each row in this matrix represents a women
  # and the columns the cumulative number of child deaths
  # experienced at eacg age.
  
  out_name <- paste0("../../Data/estimates/child_loss_all_women_",sim_name,".csv")
  
  data.table::fwrite(cd_matrix_final_women, file = out_name, row.names = F)
  
  print(paste(out_name, "saved!"))
  
}


# Adapted from Carl Mason
fertRates2 <- function(opop, fseq=c(0,(14:45),100)*12, beautify = T){
  ## Expects opop df and fseq containing age break points
  ## from ratefile. Returns fertility rates undifferentiated by marital status
  
  # Enumerator:
  
  ## age of mother at each person's birth
  opop$momage <- opop$dob-opop[z2na(opop$mom),]$dob
  
  ## age of mother at birth in ordered factor 
  opop$momagec <- cut(opop$momage,breaks=fseq,include.lowest=TRUE,right=FALSE, ordered_result=TRUE)
  
  ## event counts by age category
  fert.numer <- with(opop[!is.na(opop$momage),], tapply(pid,momagec,length)   )
  
  ## Denominator:
  
  ## age at death or censor
  opop$agedOC<- with(opop, ifelse(dod == 0, max(opop$dob),dod) - opop$dob)
  
  opop$agedOCc <- cut(opop$agedOC,breaks=fseq,include.lowest=TRUE,right=FALSE,
                      ordered_result=TRUE)
  
  deathOrCensor <- with(opop, tapply(pid,list(agedOCc,fem),length))
  
  fert.denom <- apply(deathOrCensor, 2, function(x){ rev(cumsum(rev(x))) })
  
  asfr <- fert.numer/fert.denom[,2]
  ## monthly rates should match those in ratefile.min
  
  out <- cbind(asfr/diff(fseq))
  
  if(beautify){
    m <- gsub("\\[|)|\\]", "", rownames(out))
    out <-  
      data.frame(do.call(rbind, strsplit(m, ",")), stringsAsFactors = F) %>% 
      mutate_all(as.numeric) %>% 
      mutate(X1 = X1/12, X2 = X2/12) %>% 
      select(from = X1, to = X2) %>% 
      bind_cols( data.frame(out))
    
  }
  
  return(out)
}


# Given a df with a "country" column, find the appropriate UN SDG region
find_regions <- function(df, allowed_types = "country", region_choice = "default_region", ignore_regions = T, pretty_names = F){
  
  # Save un objects in gobal envir
  format_UN_country_grouping(un_regions)
  
  out <- df %>% 
    left_join(
      un_reg %>% 
        select(type, country = level1, region = starts_with(region_choice))
      , by = c('country')
    ) %>% 
    mutate(
      # Here you chose which regions will be used
      # (default column defined in script 2_UN_country_grouping.R)
      region = factor(region, levels = regions_long)
      # level2 = factor(un_sdg_groups, levels = old_sdg)
      # , cohort2 = paste0("Women born in ", variable)
    ) %>% 
    filter(type %in% allowed_types) %>% 
    # na values in col region are regions like 'world', central america', etc
    # and can safely be ignored
    filter(!is.na(region)) %>% 
    select(-type) %>% 
    select(region, country, everything())

  if(ignore_regions){
    out <- out %>% filter(!region %in% regions_ignore)
    print("Ignoring AUS/NZ and Small island states")
  } 
  
  if(pretty_names){
    out$region <- plyr::mapvalues(out$region, from = regions_long, to = regions_short, warn_missing = F)
  }
    
  return(out)
  
}

# Given a df with a "country" column, find the appropriate UN SDG region
# Same as find_regions but can also match by code
find_regions_code <- function(df, allowed_types = "country", region_choice = "default_region", ignore_regions = T, pretty_names = F){
  # browser()
  # if region already exists
  if (!is.null(df$region)) {
    out <- df
  } else {
   
    # Save un objects in gobal envir
    format_UN_country_grouping(un_regions)
    
    out <- df %>% 
      left_join(
        un_reg %>% 
          select(type, country = level1_iso3, region = starts_with(region_choice)) %>% 
          mutate(country = toupper(country))
        , by = c('country')
      ) %>% 
      mutate(
        # Here you chose which regions will be used
        # (default column defined in script 2_UN_country_grouping.R)
        region = factor(region, levels = regions_long)
        # level2 = factor(un_sdg_groups, levels = old_sdg)
        # , cohort2 = paste0("Women born in ", variable)
      ) %>% 
      filter(type %in% allowed_types) %>% 
      # na values in col region are regions like 'world', central america', etc
      # and can safely be ignored
      filter(!is.na(region)) %>% 
      select(-type) %>% 
      select(region, country, everything())
    
    if(ignore_regions){
      out <- out %>% filter(!region %in% regions_ignore)
      print("Ignoring AUS/NZ and Small island states")
    } 
    
    if(pretty_names){
      out$region <- plyr::mapvalues(out$region, from = regions_long, to = regions_short, warn_missing = F)
    }
    
  }
  
  return(out)
  
}

find_relatives <- function(p, type, rm_ego = T, 
                           rm_parent = T,
                           parallel = T, numCores = 8) {
  
  # browser()
  
  if(is.data.table(p)) p <- as.data.frame(p)
  
  if(type == 'mother') {
    
    rel_out <- as.list(p$mother)
    names(rel_out) <- p$profileid
    
  }
  
  # Of both parents
  if(type == 'sister') {
    
    rel_out <- keep_sib(
      prof
      , rm_ego
      , keep_sex = 'female'
      , parallel
      , numCores
    )
    
  }
  
  if(type == 'grandmother_maternal') {
    
    through <- "mother" # Maternal line
    rel_type <- "mother" # get data on (grand)mother
    
    parent <- p[, through]
    parents_id <- match(parent, p$profileid)
    grandpas <- p[parents_id, rel_type]
    
    keep <- !is.na(grandpas) #Only those with known grandparents
    rel_out <- as.list(grandpas[keep])
    names(rel_out) <- p$profileid[keep]
    
    # p %>% 
    #   dplyr::filter(profileid %in% c(264, 69836161)) %>% 
    #   select(profileid, mother)
    
  }
  
  if(type == 'aunt_maternal') {
    
    rel_out <- keep_aunt_uncle(
      prof
      , through = 'mother'
      , rm_parent
      , keep_sex = 'female'
      , parallel
      , numCores
    )
  } 
  
  if(type == 'children_maternal') {
    
    rel_out <- keep_children(
      prof
      , through = 'mother'
      , keep_sex = NA
      , parallel
      , numCores
    )
  }
  
  if(type == 'children_paternal') {
    
    rel_out <- keep_children(
      prof
      , through = 'father'
      , keep_sex = NA
      , parallel
      , numCores
    )
  }
  
  return(rel_out)
  
}

fix_socsim_countries <- function(df, keep_regions = F, colname_in = "country", colname_out = "country"){
  
  if(keep_regions) ignore <- "gibberish"
  else ignore <- c("africa", "asia", "channel_islands", "europe", "melanesia"
                   , "micronesia", "polynesia", "sub_saharan_africa", "world"
                   ,  "australia_new zealand", "central and southern asia", "channel islands"
                   , "eastern and south-eastern asia", "europe and northern america", "latin america and the caribbean"
                   , "northern africa and western asia", "oceania (excluding australia and new zealand)", "sub-saharan africa"
  )
  
  out <- 
    df %>% 
    rename(country_col = starts_with(colname_in)) %>% 
    mutate(country_col = tolower(country_col)) %>% 
    filter(! country_col %in% ignore) %>% 
    mutate(
      country_col = recode(country_col
                           , china_macao_sar = "Macao"
                           , eswatini = "Swaziland"
                           , lao_people_s_democratic_republic= "Laos"
                           , r_union = "Reunion"
                           , republic_of_korea = "Korea"
                           , saint_lucia = "Saint Lucia"
                           , channel_islands= "Channel Islands" 
                           , cura_ao = "Curacao"
      ), country_col = countrycode(country_col, origin = "country.name", destination = "iso3c", warn = T)
    ) 
  
  colnames(out)[grep("country_col", colnames(out))] <- colname_out
  
  out
  
}

fix_un_countries <- function(x) {
  x <- gsub("\\.|\\?", "", x)
  x <- gsub("/", "_", x)
  tolower(x)
}

# Format sandwiched by age list output from socsim
format_socsim_sandwich_output <- function(socsim_estimates) {
  # browser()
  # Save as df
  
  # Assign an id to each simulation within each country
  
  times <- nrow(socsim_estimates[[1]])
  
  simulation <- sort(rep(1:length(socsim_estimates), times))
  
  # Let's assume that all simulations have the same number of runs
  
  times <- lapply(socsim_estimates, nrow) %>% 
    unlist %>% 
    unique
  # times <- times[order(names(times))]
  
  simulation_no <- table(countries_in_socsim) %>% 
    as.vector %>% 
    unique()
  
  
  if(length(times) != length(simulation_no)) stop("Simulation runs are not the same for all countries!!")
  
  simulation <- sort(rep(1:simulation_no, times))
  
  
  sand_socsim_full <- 
    data.frame(do.call(rbind, socsim_estimates)) %>% 
    mutate(
      # Over-write cohort if exists
      cohort = as.character(cut(birthYr, br, labels = labs, include.lowest = T))
      , source = factor("SOCSIM", levels = c("SOCSIM", "Model"))
    ) %>% 
    group_by(country) %>%
    mutate(
      simulation = as.character(sort(rep(1:simulation_no, times)) )
    ) %>% 
    ungroup() %>% 
    filter(!is.na(cohort))
  
  sand_socsim_full
  
}

# Format sandwiched by age list output from socsim
# Does one country at a time
format_socsim_sandwich_output_country <- function(socsim_estimates) {
  # browser()
  # Save as df
  
  # Assign an id to each simulation within each country
  # Let's assume that all simulations have the same number of rows
  # times <- nrow(socsim_estimates[[1]])
  
  # Note that not all simulations have the same number of rows!!
  
  times <- unlist(lapply(socsim_estimates, nrow))
  
  simulation <- sort(rep(1:length(socsim_estimates), times))
  
  sand_socsim_full <- 
    data.frame(do.call(rbind, socsim_estimates)) %>% 
    mutate(
      # Over-write cohort if exists
      cohort = as.character(cut(birthYr, br, labels = labs, include.lowest = T))
      , source = factor("SOCSIM", levels = c("SOCSIM", "Model"))
    ) %>% 
    mutate(
      simulation = simulation
    )
  
  sand_socsim_full
  
}


# Decide how countries will be groupped in the analysis
# On 20191010, we had decided to group all countries by UN SDG region 
format_UN_country_grouping <- function(un_regions){
  
  # Decide how countries will be groupped in the analysis
  # On 20191010, we had decided to group all countries by UN SDG region 
  
  
  un_reg <<- un_regions %>% 
    # fix text formatting
    mutate_all(.funs = fix_un_countries) %>% 
    # Chose which regions should be used as default
    # Very important as this will shape the final analysis
    # [PREFERRED 20190814]: Use UN SDG regions but remove UAS/NZ and Ocenia (other)
    mutate(default_region = un_sdg_groups) 
  
  # Define labels for using in plots later on
  
  regions_long <<- c(
    "sub-saharan africa"
    , "northern africa and western asia"
    , "central and southern asia"
    , "eastern and south-eastern asia"
    , "latin america and the caribbean"
    , "australia_new zealand"
    , "oceania (excluding australia and new zealand)"
    , "europe and northern america"
  )
  
  regions_short <<- c(
    # "SS Africa"
    "Sub-Sah Africa"
    , "N Africa & W Asia"
    , "C & S Asia"
    , "E & SE Asia"
    , "LATAM & Caribbean"
    , "AUS & NZ"
    , "Oceania (other)"
    , "Europe & N America"
  )
    
    regions_ignore <<- c(
      "australia_new zealand"
      , "oceania (excluding australia and new zealand)"
    )
  
  # print(paste("Objects saved in global envir: un_reg, regions_short, regions_long"))
  
}

get_asfr_socsim <- function(df, sex_keep, y_range, age_breaks, age_labels) {
  
  df <- 
    df %>% 
    mutate(
      dod2 = ifelse(dod == 0, endmo, dod)
      , death_year = asYr2(dod2, FinalSimYear, endmo)
      , birth_year = asYr2(dob, FinalSimYear, endmo)
    ) %>% 
    # filter(between(birth_year, min(y_range), max(y_range))) %>% 
    mutate(gender = ifelse(fem == 0, "male", "female"))
  
  # 1. Denominator - women in reproductive years
  den <- lapply(y_range, get_women_reproductive_age_socsim
                , df = df
                , age_breaks = age_breaks
                , age_labels = age_labels
                , sex_keep = "female"
  ) 
  
  denominator <- data.frame(do.call(rbind, den))
  
  # 2. Enumerator - births to women in given age group
  
  enumerator <- yearly_birth_by_age_socsim(df, sex_keep, y_range,age_breaks, age_labels)
  
  # 3. Rate
  
  asfr <- 
    bind_cols(denominator %>% rename(den = n),
              enumerator %>% select(enu = n)) %>% 
    dplyr::mutate(
      socsim = enu/den
      , socsim = ifelse(is.na(socsim), 0, socsim)
      # Group by 5-years
      , yg = cut(year, breaks = y_breaks, labels = un_y_labs,include.lowest = T)
      , yg = as.character(yg)
    ) %>% 
    group_by(year = yg, age = agegr) %>% 
    summarise(
      socsim = mean(socsim)
    ) %>% 
    ungroup 
  
  return(asfr)
  
}

# New on 2020616, should work for all simu;ations
# needs to be parallelised still!!!
get_asfr_socsim_master <- function(opop_l, FinalSimYear, endmo, age_breaks, age_labels, countries_in_socsim, sims, compare_to_un, ignore_simulations){
  
  print(paste("ignoring simulations", paste(ignore_simulations, collapse = ", ")))
  
  # Load data
  print("Loading data from Cave...")
  
  # Get names of simulation files
  
  #   sim_names <- gsub(
  #   "^socsim_opop_"
  #   , ""
  #   , list.files(pattern = "^socsim_opop_", path = Cave, full.names = F)
  # )
  # 
  # sim_names <- gsub(".csv", "", sim_names)
  
  # Load opop  as list
  
  opops <- list.files(pattern = "^socsim_opop", path = Cave, full.names = T)
  # opop_l <- lapply(opops, data.table::fread, stringsAsFactors = F)
  opop_l <- lapply(opops, function(d) data.frame(data.table::fread(d), stringsAsFactors = F))
  names(opop_l) <- sim_names
  
  # Delete empty sims
  rows <- unlist(lapply(opop_l, nrow))
  keep <- rows > 100
  opop_l <- opop_l[keep]
  
  print("Getting asfr...")
  
  asfr_l <- lapply(seq_along(countries_in_socsim), function(n, sims){
    
    print(n)
    print(countries_in_socsim[n])
    
    get_asfr_socsim(
      df = opop_l[[n]]
      , sex_keep = 'female'
      , y_range = y_range
      , age_breaks = age_breaks
      , age_labels = age_labels
    ) %>% 
      mutate(
        simulation = sims[n]
        , country = countries_in_socsim[n]
        , socsim = socsim * 1000
      ) 
    
  }, sims)
  
  # asfr <- data.frame(do.call(rbind, asfr_l))
  asfr <- bind_rows(asfr_l)
  
  if(compare_to_un) {
    
    out <- 
      asfr %>% 
      left_join(asfr_un %>% rename(un = value), by = c("country", "year", "age")) 
      # pivot_longer(cols = -c(country, year, age, simulation), names_to = "source", values_to = "asfr")
    
  } else{
    out <- asfr
  }
  
  return(out)
  
}

# OLD CODE changed 20200616
# Works with sample of 4 simulations
# get_asfr_socsim_master <- function(opop_l, FinalSimYear, endmo, age_breaks, age_labels, countries_in_socsim, sims, compare_to_un){
#   
#   asfr_l <- lapply(seq_along(countries_in_socsim), function(n, sims){
#     
#     print(countries_in_socsim[n])
#     
#     get_asfr_socsim(
#       df = opop_l[[n]]
#       , sex_keep = 'female'
#       , y_range = y_range
#       , age_breaks = age_breaks
#       , age_labels = age_labels
#     ) %>% 
#       mutate(
#         simulation = sims[n]
#         , country = countries_in_socsim[n]
#         , socsim = socsim * 1000
#       ) 
#     
#   }, sims)
#   
#   asfr <- data.frame(do.call(rbind, asfr_l))
#   
#   if(compare_to_un) {
#     
#     out <- 
#       asfr %>% 
#       left_join(asfr_un %>% rename(un = value), by = c("country", "year", "age")) %>%
#       pivot_longer(cols = -c(country, year, age, simulation), names_to = "source", values_to = "asfr")
#     
#   } else{
#     out <- asfr
#   }
#   
#   return(out)
#   
# }


# Returns a df with ae-specific mortality rates for a given country in a certain age range
# taking as input a socsim opop df
get_asmr_socsim_period <- function(df, country_keep, FinalSimYear, endmo, age_breaks, age_labels, y_breaks, un_y_labs, y_range, compare_to_un = T){
  
  opop <- 
    df %>% 
    mutate(
      dod2 = ifelse(dod == 0, endmo, dod)
      , death_year = asYr2(dod2, FinalSimYear, endmo)
      , birth_year = asYr2(dob, FinalSimYear, endmo)
      , age_death_months = ifelse(dod == 0,NA,dod-dob)
      , age_death = trunc(age_death_months/12)
      , age_death_g = cut(age_death, breaks = age_breaks, labels = age_labels, include.lowest=TRUE,right=FALSE)
      , age_death_g = as.numeric(as.character(age_death_g))
    ) %>% 
    rename(gender = fem)
  
  # 1.1. Numerator - death counts 
  
  # drop the folks in the initial population
  # agedC is aged at death OR censor
  # agedCf is a factor with the age cats used in ratefile.min
  
  numerator <- 
    opop %>% 
    # filter(dob >= firstmonth) %>% 
    filter(dod != 0) %>% 
    dplyr::count(death_year, gender, age_death = age_death_g) %>% 
    select(year = death_year, age = age_death, everything()) %>% 
    filter(year %in% y_range) %>% 
    arrange(year, gender, age)
  
  # 1.2 Denominator 
  
  # Currently, this is poulation who was ever alive during the year
  # Now, it only considers the year of death, meaning that the poulation includes all those who were alive 
  # at the start of a given year and did not die that year.
  # This is consistent with HMD's definition of "alive on January 1st" but could still be corrected
  # to account for "the timing of deaths during the interval and variation in cohort's birthdays by month"
  
  opop_subset <- 
    opop %>% 
    select(pid, birth = birth_year, death = death_year, gender)
  
  yearly_pop_age_sex <- lapply(y_range, census_socsim
                               , df = opop_subset 
                               , return_ids = F
                               , group_by_sex = F
                               , group_by_age_sex = T
  )  
  
  
  denominator <- 
    data.frame(do.call(rbind, yearly_pop_age_sex)) %>% 
    dplyr::filter(!is.na(gender)) %>% 
    dplyr::select(year = census, everything()) %>% 
    dplyr::mutate(
      age = cut(age_at_census,breaks = age_breaks, labels = age_labels,include.lowest=TRUE,right=FALSE)
      , age = as.numeric(as.character(age))
    ) %>% 
    dplyr::group_by(year, gender, age) %>% 
    dplyr::summarise(n = sum(n)) %>% 
    arrange(year, gender, age)
  
  # 2.2.3. Rate
  
  asmr <- 
    numerator %>% 
    full_join(denominator, by = c("year", "gender", "age"), suffix = c("_num", "_den")) %>% 
    mutate(
      socsim = n_num / n_den
      , socsim = ifelse(is.na(socsim), 0, socsim)
      # Group by 5-years
      , yg = cut(year, breaks = y_breaks, labels = un_y_labs,include.lowest = T)
      , yg = as.character(yg)
      , gender = ifelse(gender == 0, "male", "female")
    ) %>% 
    group_by(year = yg, gender, age) %>% 
    summarise(
      socsim = mean(socsim)
    ) %>% 
    ungroup %>%
    mutate(country = country_keep)
  
  asmr_f <- 
    asmr %>% 
    filter(gender == "female") %>% 
    select(-gender)
  
  # 1.3. Compare to UN WPP values 
  
  if(compare_to_un){
    out <- 
      mx_un_f %>% 
      filter(country %in% country_keep ) %>% 
      select(country, year, age, un = mx) %>% 
      full_join(asmr_f, by = c("country", "year", "age")) 
      # pivot_longer(cols = -c(country, year, age), names_to = "source", values_to = "asmr")
  } else{
    out <- asmr_f
  }
  
  
  return(out)  
}

# Returns a df
get_asmr_socsim_master <- function(opop_l, type = "period", countries_in_socsim, sims, FinalSimYear, endmo, age_breaks, age_labels, y_breaks, un_y_labs, y_range, compare_to_un = T){
  
  # Decide which routine to implement, whether period or cohort
  fun <- get(paste0("get_asmr_socsim_", type))
  print(paste("Estimating values for", type))
  
  comp_l <- lapply(seq_along(countries_in_socsim), function(n, sims){
    print(n)
    fun(
      df = opop_l[[n]]
      , country_keep = countries_in_socsim[n]
      , FinalSimYear
      , endmo
      , age_breaks
      , age_labels
      , y_breaks
      , un_y_labs
      , y_range
      , compare_to_un = compare_to_un
    ) %>% 
      mutate(simulation = sims[n])
    
  }, sims)
  
  # asmr_comp <- data.frame(do.call(rbind, comp_l))
  asmr_comp <- bind_rows(comp_l)
  
  return(asmr_comp)
}

# ADapted from first_death, for all relative death's
get_death_dates <- function(r_list, p, parallel = T, numCores = 8, 
                            cohort_breaks, cohort_labels
                            , worker_function = worker_get_dates_all
) {
  
  ids <- names(r_list)
  
  # Get birth cohort 
  
  birth <- as.numeric(p$birth_year[p$profileid %in% ids])
  # birth[birth < min(cohort_breaks) & birth > max(cohort_breaks)] <- NA
  
  coh <- cut(
    birth
    , breaks = cohort_breaks
    , include.lowest = TRUE
    , labels = cohort_labels
    , right = F
  )
  
  # Filter out out-of tear boundary inds
  drop <- unlist(lapply(coh, is.na))
  
  coh <- coh[!drop]
  r_list <- r_list[!drop]
  
  if(!is.data.table(p)) p <- as.data.table(p)
  col <- "death_year"
  
  if(!parallel) {
    # Option A. Non parallelised
    print("Finding data (non-parallelised).")
    dates <- lapply(r_list[1:1000], worker_function, col, p)
  } else {
    # Option B. Parallelise using sockets for Windows
    # http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
    # browser()
    
    # numCores <- detectCores()
    
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    clusterExport(
      cl
      , varlist = c("col", "p")
      , envir = environment()
    )
    
    # Load packages
    clusterEvalQ(cl, library(data.table))
    
    dates <- parLapply(cl, r_list, worker_function, col, p) 
    
    stopCluster(cl)
  }
  
  out_l <- list(dates = dates,coh = coh)
  
  return(out_l)
  
}


# Get dependency rate, young dependency and old dependency rates given an opop_l list
# and a vector of period years
# Level 0 function
get_dependency_rate <- function(opop_l, year_range, tau, parallel = F, type = "dependency", Cave) {
  
  # Load all data
  
  sim_names <- gsub(
    "^socsim_opop_"
    , ""
    , list.files(pattern = "^socsim_opop_", path = Cave, full.names = F)
  )
  
  sim_names <- gsub(".csv", "", sim_names)
  
  opops <- list.files(pattern = "^socsim_opop", path = Cave, full.names = T)
  opop_l <- lapply(opops, data.table::fread, stringsAsFactors = F)
  names(opop_l) <- sim_names
  
  # Get a list with pop by age and sex
  if(!parallel){
    dependency_list <- lapply(opop_l, get_dependency_rate_1, year_range, type, tau)
  } else{
    warning("Sequence undefined!")
  }
  
  # Export as a data frame
  
  dependency_df <- 
    # Assign simulation name
    lapply(seq_along(dependency_list), function(n, sim){
      df <- dependency_list[[n]]
      df["simulation"] <- sim[n]
      df
    }, sim = sim_names) %>% 
    do.call(rbind, .) %>% 
    data.frame() %>% 
    mutate(
      country = gsub("_[0-9]+$", "", simulation)
    ) %>% 
    select(simulation, everything())
  
  dependency_df
}

# OLD - keep 
# get_dependency_rate <- function(opop_l, year_range, parallel = F, type = "dependency") {
#   
#   # Get a list with pop by age and sex
#   if(!parallel){
#     dependency_list <- lapply(opop_l, get_dependency_rate_1, year_range, type)
#   } else{
#     warning("Sequence undefined!")
#   }
#   
#   # Export as a data frame
#   
#   dependency_df <- 
#   # Assign simulation name
#   lapply(seq_along(dependency_list), function(n, sim){
#     df <- dependency_list[[n]]
#     df["simulation"] <- sim[n]
#     df
#   }, sim = names(opop_l)) %>% 
#     do.call(rbind, .) %>% 
#     data.frame() %>% 
#     mutate(
#       country = gsub("_[0-9]+$", "", simulation)
#     ) %>% 
#     select(simulation, everything())
#   
#   dependency_df
# }

# Level 1 worker function, works with opo pdf
get_dependency_rate_1 <- function(opop, year_range, type, tau = 5){
  
  opop2 <- 
    opop %>% 
    mutate(
      dod2 = ifelse(dod == 0, endmo, dod)
      , death_year = asYr2(dod2, FinalSimYear, endmo)
      , birth_year = asYr2(dob, FinalSimYear, endmo)
      , age_death_months = ifelse(dod == 0,NA,dod-dob)
      , age_death = trunc(age_death_months/12)
      , age_death_g = cut(age_death, breaks = age_breaks_mort, labels = age_labels_mort, include.lowest=TRUE,right=FALSE)
      , age_death_g = as.numeric(as.character(age_death_g))
    ) %>% 
    rename(gender = fem) %>% 
    select(pid, birth = birth_year, death = death_year, gender)
  
  # Iterate each year
  all_ages <- 
    lapply(year_range, census_socsim, df = opop2, return_ids = F, group_by_age_sex = T) %>% 
    do.call(rbind, .) %>% 
    data.frame() %>% 
    rename(fem = gender, pop = n)
  
  # Now implement type of dependency rate
  
  # 1. Dependency ratio ~~~~~~~~~~~~~~~~~~~~
  # Numerator:people younger than 15 and older that 64; 
  # denominator: between 15 and 64 years old. 
  
  
  dependency <- 
    all_ages %>% 
    mutate(
      dependent = ifelse(age_at_census < 15 | age_at_census > 64, T, F)
    ) %>% 
    group_by(census, dependent) %>% 
    summarise(n = sum(pop)) %>% 
    ungroup() %>% 
    arrange(census, dependent) %>% 
    mutate(dependency = n / lag(n)) %>% 
    filter(dependent) %>% 
    select(-dependent, - n)
  
  
  # 2. Young dependency ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Numerator: younger than 15 years old. 
  # Dennominator: 15-64
  
  
  young_dependency <- 
    all_ages %>% 
    mutate(
      dependent = ifelse(age_at_census < 15, T, F)
      , dependent = ifelse(age_at_census >= 15 & age_at_census < 65, F, dependent)
      , dependent = ifelse(age_at_census > 65, NA, dependent)
    ) %>% 
    filter(!is.na(dependent)) %>% 
    group_by(census, dependent) %>% 
    summarise(n = sum(pop)) %>% 
    ungroup() %>% 
    arrange(census, dependent) %>% 
    mutate(young = n / lag(n)) %>% 
    filter(dependent) %>% 
    select(-dependent, - n)
  
  
  # 3. Old dependency ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Numerator older than 64 years.  
  # Dennominator: 15-64
  
  old_dependency <- 
    all_ages %>% 
    mutate(
      dependent = ifelse(age_at_census > 64, T, F)
      , dependent = ifelse(age_at_census >= 15 & age_at_census < 65, F, dependent)
      , dependent = ifelse(age_at_census < 15, NA, dependent)
    ) %>% 
    filter(!is.na(dependent)) %>% 
    group_by(census, dependent) %>% 
    summarise(n = sum(pop)) %>% 
    ungroup() %>% 
    arrange(census, dependent) %>% 
    mutate(old = n / lag(n)) %>% 
    filter(dependent) %>% 
    select(-dependent, - n)
  
  # 4. Prospective old-age dependency ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Numerator: people expected to die within a certain number of years.
  warning("Check prospective rate esetimates! 20200522")
  
  # I can't use the previous script since I need individual-level censu data
  prospective <- 
    lapply(year_range, census_socsim, df = opop2, return_ids = F, group_by_age_sex = F) %>% 
    do.call(rbind, .) %>% 
    data.frame() %>% 
    rename(fem = gender) %>% 
    mutate(
      age_at_census = census - birth
      , doomed = ifelse(death_temp - tau <= census, T, F)
      , working = ifelse(age_at_census >= 15 & age_at_census < 65, T, F)
      , working = ifelse(doomed, F, working)
      , dependent = !working
      , kids = ifelse(age_at_census < 15, T, F)
    ) %>% 
    filter(!kids) %>% 
    # Get number of cases by census and age and STATUS
    dplyr::count(census, age_at_census, dependent) %>% 
    tidyr::complete(census, age_at_census, dependent, fill = list(n = 0)) %>% 
    rename(pop = n) %>% 
    group_by(census, dependent) %>% 
    summarise(n = sum(pop)) %>% 
    ungroup() %>% 
    arrange(census, dependent) %>% 
    mutate(prospective = n / lag(n)) %>% 
    filter(dependent) %>% 
    select(-dependent, - n)
  
# Put all together
  output <- 
    dependency %>% 
    left_join(young_dependency, by = "census") %>%
    left_join(old_dependency, by = "census") %>% 
    left_join(prospective, by = "census") %>% 
    rename(year = census)
  
  output
}

# Returns a partial cohort life table (come cols missing) taking as input a socsim opop df
# This function is self-contained (ie requires no data to be pre-leaded in the global envir)
get_lt_socsim_cohort <- function(Cave, sim_names, year_min = 1970, year_max = 2040, FinalSimYear, endmo, nax = NULL, export, path_out){
  
  # These simulations are wrong - have ony a couple hundred rows
  # o back to SOCSIM and see what happened to them...
  # DEPRECATED, but I'll just leave this here since it doesn't
  # matter as these simulations ids no longe exist and I don't
  # want to break anything
  ignore_simulations <- c(
    "Czechia_431022", "Czechia_569865"
    , "C_te_d_Ivoire_431221", "C_te_d_Ivoire_699935"
  )
  
  # print(paste("ignoring simulations", paste(ignore_simulations, collapse = ", ")))
  
    # Load data
  print("Loading data from Cave...")
  # Get names of simulation files
  sim_names <- gsub(
    "^socsim_opop_"
    , ""
    , list.files(pattern = "^socsim_opop_", path = Cave, full.names = F)
  )
  
  sim_names <- gsub(".csv", "", sim_names)
  
  # Load opop  as list
  
  opops <- list.files(pattern = "^socsim_opop", path = Cave, full.names = T)
  opop_l <- lapply(opops, data.table::fread, stringsAsFactors = F)
  names(opop_l) <- sim_names
  # opop_df <- data.frame(do.call(rbind, opop_l))
  
  print("Data loaded! Now saving as df")
  
  # Save as df
  
  times <- unlist(lapply(opop_l, nrow))
  
  simulation <- sort(rep(sim_names, times))
  
  opop_df <- 
    data.frame(do.call(rbind, opop_l)) %>% 
    mutate(
      simulation = simulation
      , sex = ifelse(fem == 0, "male", "female")
      , dod2 = ifelse(dod == 0, endmo, dod)
      , death_year = asYr2(dod2, FinalSimYear, endmo)
      , birth_year = asYr2(dob, FinalSimYear, endmo)
      , age_death_months = ifelse(dod == 0,NA,dod-dob)
      , age_death = trunc(age_death_months/12)
      # , age_death_g = cut(age_death, breaks = age_breaks, labels = age_labels, include.lowest=TRUE,right=FALSE)
      # , age_death_g = as.numeric(as.character(age_death_g))
    ) %>% 
    filter(between(birth_year, year_min, year_max)) %>% 
    filter(!simulation %in% ignore_simulations) %>% 
    select(simulation, birth_year, age_death, sex)
  
  # sort(unique(opop_df$simulation))
  
  # Second, get life table!!
  
  print("Now let;s start getting those cohort life tables...")
  
  # First, pick values for nax
  
  n <- c(diff(0:99), 999)
  if (is.null(nax)) {
    nax <- 0.5 * n
    if (n[2] == 4) {
      if (sex == "male") {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.33
          nax[2] <- 1.352
        }
        else {
          nax[1] <- 0.045 + 2.684 * nmx[1]
          nax[2] <- 1.651 - 2.816 * nmx[1]
        }
      }
      if (sex == "female") {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.35
          nax[2] <- 1.361
        }
        else {
          nax[1] <- 0.053 + 2.8 * nmx[1]
          nax[2] <- 1.522 - 1.518 * nmx[1]
        }
      }
    }
  }
  
  # Now, get life table columns
  
  lt <- 
    opop_df %>% 
    dplyr::count(simulation, sex, birth_year, age_death, name = "d") %>% 
    arrange(simulation, sex, birth_year, age_death) %>% 
    # Add all combinations of age
    mutate(age_death = factor(age_death, levels = 0:99)) %>% 
    tidyr::complete(simulation, sex, birth_year, age_death, fill = list(d = 0)) %>% 
    mutate(age_death = as.numeric(as.character(age_death))) %>% 
    group_by(simulation, sex, birth_year) %>% 
    mutate(
      deaths = cumsum(d)
      , ndx = c(diff(deaths), 0)
      # Normalise
      , ndx = ndx / sum(ndx)
      , nlx = c(1, 1 - cumsum(ndx)[1:(n()-1)])
      , nax = nax
      # , nLx = n * nlx + ndx * nax
      , nLx = lead(nlx, n = 1, default = 0) + (ndx*nax)
      , Tx = rev(cumsum(rev(nLx)))
      , ex = Tx/nlx
    ) %>% 
    ungroup() %>% 
    select(simulation, sex, cohort = birth_year, age = age_death, nlx, nLx, Tx, ex) 
  
  # Group by simulation
  print("Aggregating by simulation...")
  
  lt_ag <- 
    lt %>% 
    mutate(country = gsub("_[0-9]+","",simulation)) %>% 
    select(-simulation) %>% 
    group_by(country, sex, cohort, age) %>% 
    summarise_all(mean) %>% 
    ungroup %>% 
    filter(!country %in% "China_and_dependencies") %>% 
    mutate(
      country = recode(country
                       , Republic_of_Korea = "South Korea"
                       , Cura_ao = "Curacao"
                       , Lao_People_s_Democratic_Republic = "Laos"
                       , China_Macao_SAR = "Macao"
                       , R_union = "Reunion"
                       , Eswatini = "Swaziland"
      )
    , country = countrycode(country, origin = "country.name", destination = "iso3c", warn = F)
    ) %>% 
    # Keep only countries
    filter(!is.na(country)) %>% 
    arrange(country, cohort, sex, age)
  
  print("All done!!")
  
  if(export){
    print("Exporting....")
    
    print("Saving full dataset in one cvs")
    
    data.table::fwrite(lt_ag, "../../Data/derived/LTC_socsim.csv", row.names = F)
    
   #  print("2/2 Saving as separate csv files. I'll save one csv per country")
   #  
   # export_l <- split(lt_ag, lt_ag$country)
   #  
   #  for(df in export_l){
   #    con <- unique(df$country)
   #    # print(paste("Exporting", con))
   #    data.table::fwrite(
   #      df %>% select(-country)
   #      , file = paste0(path_out, "lt_coh_",con, ".csv")
   #      , row.names = F
   #    )
   #  }
    
  } else {
    return(lt_ag)    
  }
  
  
}

get_MACB_stable_pop <- function(LTCF, ASFRC, cos_model){
  # \mu is approximated as:
  
  # \begin{equation}
  # \mu = \frac{\sum_{15}^{50}{ (x + 0.5) e^{-r(x + 0.5)}f(x)l(x) }}{\sum_{15}^{50}{e^{-r(x + 0.5)}f(x)l(x)}}
  # \end{equation}
  
  # Get Lotka's r for each cohort
  fx_lx_l <- 
    LTCF %>% 
    select(cohort = Cohort, age = Age, lx) %>% 
    left_join(
      ASFRC %>% 
        select(cohort = Cohort, age = Age, ASFR)
      , by = c("cohort", "age")
    ) %>% 
    mutate(ASFR = ifelse(is.na(ASFR), 0, ASFR)) %>% 
    filter(cohort %in% cos_model) %>% 
    split(., .$cohort)
  
  macb <- unlist(lapply(fx_lx_l, get_MACB_stable_pop_1))
  
}

get_MACB_stable_pop_1 <- function(fx_lx){
  
  ages <- unique(fx_lx$age)
  fx <- fx_lx$ASFR
  lx <- fx_lx$lx
  
  # Find Lotka's R
  r <- LotkaRCoale(fx = fx, Lx = fx, x = ages)
  
  # Get MACB
  
  # NUmerator
  num <- sum( (ages + 0.5) * exp(-r*(ages + 0.5)) * fx * lx )
  # Denominator
  den <- sum( exp(-r*(ages + 0.5)) * fx * lx )
  
  num/den
  
}


# Conduct censuses to get population size from socsim simulations
get_pop_size <- function(year_range, country_keep, Cave, FinalSimYear, endmo){
  
  # Get names of simulation files
  sim_names_full <- gsub(
    "^socsim_opop_"
    , ""
    , list.files(pattern = "^socsim_opop_", path = Cave, full.names = T)
  )
  
  countries_in_socsim <-
    list.files(pattern = "^socsim_opop_", path = Cave, full.names = F) %>% 
    gsub("^socsim_opop_", "",.) %>% 
    gsub(".csv", "", .) %>% 
    gsub("_[0-9]+", "", .) %>% 
    # tolower() %>% 
    unique() 
  
  country_df <- 
    data.frame(country = countries_in_socsim) %>% 
    fix_socsim_countries(keep_regions = T) %>% 
    mutate(old = countries_in_socsim) 
  
  # keep only relevant countries
  
  if(country_keep == "all"){
  
  sims_keep <- sim_names_full
    
    sims_keep_countries_old <- 
      sims_keep %>% 
      gsub(Cave, "", .) %>% 
      gsub(".csv", "", .) %>% 
      gsub("^socsim_opop_", "",.) %>% 
      gsub("_[0-9]+", "", .)
    
    sims_keep_countries_new <- country_df$country[match(sims_keep_countries_old, country_df$old)]
    
  } else{
    
    c_keep <- countrycode(country_keep, origin = "country.name", destination = "iso3c", warn = T)
    
    socsim_old_keep <- country_df$old[match(c_keep, country_df$country)]
    
    sims_keep <- sim_names_full[grepl(paste0(socsim_old_keep, collapse = "|"), sim_names_full)]
    
    sims_keep_countries_old <- 
      sims_keep %>% 
      gsub(Cave, "", .) %>% 
      gsub(".csv", "", .) %>% 
      gsub("^socsim_opop_", "",.) %>% 
      gsub("_[0-9]+", "", .)
    
    sims_keep_countries_new <- country_df$country[match(sims_keep_countries_old, country_df$old)]
  }
  
  # Get pop size
  
  sizes <- 
    lapply(sims_keep, get_pop_size2, year_range, FinalSimYear, endmo) %>% 
    do.call(rbind, .) %>% 
    data.frame %>% 
    mutate(country = sims_keep_countries_new) %>% 
    select(country, everything())
  
  colnames(sizes)[2:ncol(sizes)] <- year_range
  
  return(sizes)    
}

# Worker fuction
get_pop_size2 <- function(sim, year_range, FinalSimYear, endmo){
  print(paste("Working on", sim))
  
  # sim <- sim_names_full[1]
  
  df <- 
    data.frame(fread(sim)) %>% 
    mutate(
      dod2 = ifelse(dod == 0, endmo, dod)
      , death = asYr2(dod2, FinalSimYear, endmo)
      , birth = asYr2(dob, FinalSimYear, endmo)
    ) %>% 
    select(profileid = pid, death, birth)
  
  ids <- 
    lapply(year_range, function(y, df){
      census_socsim(
        df = df 
        , y = y
        , return_ids = T
      )    %>% 
        length()
    }, df) %>% 
    unlist()
  
  names(ids) <- year_range
  
  return(ids)
  
}

# This function does NOT filter on sex since is uses the original
# opop data.frame, which includes everyon in the population
get_rels <- function(ego, opop, omar, kappa, tau){
  ## collect all relatives of ego; add some useful quantities
  if(opop[ego,]$mom == 0) {return(NULL)} # do not use initial population
  
  spouse <- omar[opop[ego,]$marid,]$hpid
  
  spouseids <- (omar %>% filter(wpid == ego))$hpid
  
  spouse <- opop %>% filter(pid %in% spouseids)
  
  kids <- opop %>% filter(mom == ego) 
  
  gmother <- opop %>% filter(pid %in% opop[ego,"mom"])
  
  gfather <- opop %>% filter(pid %in% opop[ego,"pop"])
  
  s.parents <- opop %>% filter(pid %in% unlist(opop[spouseids,c("mom","pop")]))
  
  mother <- opop[ego,]
  
  stepkids <- opop %>% filter(pop %in% spouseids, mom != ego)
  
  rels <- bind_rows(
    kids = kids
    , gmother = gmother
    , gfather = gfather
    , mother = mother
    , spouse = spouse
    , s.parents = s.parents
    , stepkids = stepkids
    , .id ="who"
  ) %>% 
    mutate(
      # Assign enddate if person was still alive at end of simulation
      Dod = ifelse(dod == 0, endmo, dod)
      # Age when last observed
      , aged = Dod - dob
      # Assign generation relative to ego
      , gen = who2gen(who)
      # now get the date that corresponds to tau years before this person
      # died. Since this will mainly be used for grandmothers, it doesn't
      # matter that it may give negative values for people who lived less
      # than tau years (eg child deaths)
      , dod_tau = ifelse(dod == 0, NA, Dod - tau*12)
      # Month in which this person would have celebrated their 
      # kappa birthday if they survice to that age
      , birthday_kappa = ifelse(Dod <= (dob + kappa*12), Dod, dob + kappa*12)
      # , birthday_10 = ifelse(Dod <= (dob + 120),Dod,dob+120)
    )
  
  return(rels)  
}


get_women_reproductive_age_socsim <- function(df, year, age_breaks, age_labels, sex_keep = "female") {
  
  df$census <- year
  
  df$death_temp <- df$death_year
  
  out <-
    df %>% 
    dplyr::filter(birth_year <= year & death_temp >= year) %>% 
    dplyr::filter(gender %in% sex_keep) %>%  
    dplyr::mutate(
      age_at_census = census - birth_year
      , agegr_at_census = cut(age_at_census, age_breaks, labels = age_labels, include.lowest = TRUE, 
                              right = F)
      , agegr_at_census = as.character(agegr_at_census)
    ) %>% 
    dplyr::filter(!is.na(agegr_at_census)) %>% 
    dplyr::count(agegr_at_census, census) %>% 
    mutate(
      agegr_at_census = factor(agegr_at_census, levels = age_labels)
      # , census = factor(census, levels = y_range)
    ) %>% 
    tidyr::complete(agegr_at_census, fill = list(n = 0)) %>% 
    select(year = census, agegr = agegr_at_census, n) %>% 
    arrange(year, agegr)
  
  return(out)
}

keep_aunt_uncle <- function(p, through = 'mother', rm_parent, 
                            keep_sex, parallel, numCores) {
  
  # browser()
  print("Keeping only relevant cases...")
  #Filter out non-applicable cases
  p <- p %>% dplyr::mutate(parents = paste0(father, "_",mother))
  
  p2 <- p %>% 
    dplyr::filter(!is.na(father) & !is.na(mother))
  
  # Keep only those where mother has known granparent of given sex
  
  parent <- p2[, through]
  
  grandpas <- p2[match(parent, p2$profileid), ] %>% 
    select(grandma = mother, grandpa = father) 
  
  p3 <- cbind(
    p2 ,
    grandpas
  ) %>% 
    select(profileid, parents, grandma, grandpa, parent_ref = dplyr::contains(through)) %>% 
    na.omit %>% 
    dplyr::mutate(
      grandpas = paste(grandpa, grandma, sep = "_")
    ) %>% 
    select(profileid, parents, grandpas, parent_ref)
  
  print("Done.")
  
  print("Start locating aunts or uncles...")
  
  args <- split(p3, seq(nrow(p3)))
  
  if(!parallel) {
    # Option A. Non parallelised
    print("Finding auts (non-parallelised).")
    out <- lapply(args[1:100], worker_find_aunts_uncles, p, rm_parent, keep_sex)
  } else {
    # Option B. Parallelise using sockets for Windows
    # http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
    # browser()
    
    # numCores <- detectCores()
    
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    # Load packages
    # clusterEvalQ(cl, library(dplyr))
    
    clusterExport(
      cl
      , varlist = c("p", "rm_parent", "keep_sex")
      , envir = environment()
    )
    
    out <- parLapply(cl, args, worker_find_aunts_uncles
                     , p, rm_parent, keep_sex) 
    
    stopCluster(cl)
  }
  
  names(out) <- p3$profileid
  
  return(out)
  
}

keep_children <- function(p, through, keep_sex, parallel, numCores) {
  # browser()
  print("Keeping only relevant cases...")
  
  p2 <- p %>% dplyr::rename(parent = dplyr::contains(through)) 
  
  # Keep only those who are mothers/fathers
  parents_id <- na.omit(unique(unlist(p2$parent)))
  
  p3 <- p2 %>% 
    dplyr::filter(profileid %in% parents_id)
  
  args <- p3$profileid
  
  print("Done.")
  print("Start locating children...")
  
  if(!parallel) {
    # Option A. Non parallelised
    print("Finding children (non-parallelised).")
    out <- lapply(args, worker_find_children, p2, keep_sex)
  } else {
    # Option B. Parallelise using sockets for Windows
    # http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
    # browser()
    
    # numCores <- detectCores()
    
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    # Load packages
    # clusterEvalQ(cl, library(dplyr))
    
    clusterExport(
      cl
      , varlist = c("p", "p2", "keep_sex")
      , envir = environment()
    )
    
    print(system.time(
      out <- parLapply(cl, args, worker_find_children, p2, keep_sex) 
    ))
    
    stopCluster(cl)
  }
  
  names(out) <- p3$profileid
  
  return(out)
  
}

# Given a vector of country name codes, returns only those for which an equivalent file does
# not yet exist
keep_only_new_simulations <- function(df, pattern, path_to_search = "Data/estimates"){

  # Get existing files
  socsim_estimates_files <- list.files(path = path_to_search, pattern = pattern, full.names = F)
  existing <- gsub(".csv", "", socsim_estimates_files)
  existing <- gsub(pattern, "", existing)
  existing <- gsub("socsim_", "", existing)

  new <- gsub("_[0-9]+", "", gsub("^socsim_opop_", "", gsub(".csv$", "", df$opop)))
  
  df[!new %in% existing, ]
  
}

# Get siblings ids as list
keep_sib <- function(p, rm_ego, keep_sex, parallel, numCores) {
  # browser()
  print("Keeping only relevant cases...")
  #Filter out non-applicable cases
  p2 <- 
    p %>% 
    dplyr::filter(!is.na(father) & !is.na(mother)) %>% 
    dplyr::mutate(parents = paste0(father, "_",mother))
  
  keep <- 
    p2 %>% 
    dplyr::count(parents) %>% 
    dplyr::filter(n > 1) %>% 
    pull(parents)
  
  p3 <- p2 %>% dplyr::filter(parents %in% keep)
  
  print("Done.")
  
  print("Start locating siblings...")
  
  args <- split(select(p3, profileid, parents), seq(nrow(p3)))
  
  if(!parallel) {
    # Option A. Non parallelised
    print("Finding siblings (non-parallelised).")
    out <- lapply(args, worker_find_sib, p, p3, rm_ego, keep_sex)
  } else {
    # Option B. Parallelise using sockets for Windows
    # http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
    # browser()
    
    # numCores <- detectCores()
    
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    # Load packages
    # clusterEvalQ(cl, library(dplyr))
    
    clusterExport(
      cl
      , varlist = c("p", "p3", "rm_ego", "keep_sex")
      , envir = environment()
    )
    
    print(system.time(
      out <- parLapply(cl, args, worker_find_sib, p, p3, rm_ego, keep_sex) 
    ))
    
    stopCluster(cl)
  }
  
  names(out) <- p3$profileid
  
  return(out)
  
}


lag_fertility <- function(cohort, xsf, age_range, rep_age) {
  
  # print(paste("Working on cohort:", cohort))
  
  fertvec <- NULL
  
  for (a in age_range){
    
    # Get a vector of all _1F_{x, t-a-x} values for women
    
    for (x in xsf){
      
      fert <- NULL
      fertsum <- NULL
      
      if((x_lag <- a - x) %in% rep_age){
        fert <- ASFRC[ASFRC$Cohort == cohort & ASFRC$Age == x_lag, "ASFR"]  
      } else {
        fert <- 0
      }
      
      fertsum <- sum(fertsum, fert)
    }
    
    fertvec <- c(fertvec, fertsum)
    
  }
  
  return(fertvec)
}


lexis_coord_cohort_sand <- function(row){
  
  id <- row['id']
  n <- as.numeric(row[c("cohort", "age")])
  names(n) <- c("cohort", "age")
  
  xcoord <- c(n['cohort'], n['cohort'] + 1, n['cohort'] + 1, n['cohort'])
  ycoord <- c(n['age'], n['age'], n['age'] + 1, n['age'] + 1)
  
  data.frame(
    id = id
    , cohort = xcoord
    , age = ycoord
    , row.names = NULL, stringsAsFactors = F)
  
}


lexis_coord_period_sand <- function(row){
  # browser()
  id <- row['id']
  n <- as.integer(row[2:3])
  names(n) <- names(row)[2:3]
  
  xcoord <- c(n['year'], n['year'] + 1, n['year'] + 2, n['year'] + 1)
  ycoord <- c(n['age'], n['age'], n['age'] + 1, n['age'] + 1)
  
  data.frame(
    id = id
    , year = xcoord
    , age = ycoord
    , row.names = NULL, stringsAsFactors = F)
  
}

# adapted from pacman package
library2 <- function (package1, ...) {
  packages <- c(package1, ...)
  for (package in packages) {
    if (package %in% rownames(installed.packages())) {
      suppressPackageStartupMessages( do.call(library, list(package)) )
      print(paste("library2:",package, "loaded."))
    }
    else {
      tryCatch({
        install.packages(package)
        suppressPackageStartupMessages( do.call(library, list(package)) )
      }, error = function(e) {
      })
    }
  }
}


# Lotka's r can be estimated using TIm RIffe's Lotka package:
# https://sites.google.com/site/timriffepersonal/r-code/lotka/lotkarcoale
# FromTIm RIffe's Lotka package
# Needs:
# fx 	vector of fertility rates. Usually daughters born to mothers.
# Lx 	vector of lifetable exposures, L(x), with a l0 radix of 1.
# x 	vector of ages. e.g. 0:110 or alpha:beta, where alpha is the youngest age at reproduction and beta is the oldest age at reproduction.
LotkaRCoale <-
  function(fx,Lx,x){
    # from Coale, Ansley J. (1957) A New Method for Calculating Lotka's r- the Intrinsic Rate of Growth in a Stable Population.
    # Population Studies, Vol. 11 no. 1, pp 92-94
    R0 <- Rmomentn(fx,Lx,x,0)
    # first assuming a mean generation time of 29
    ri <- log(R0)/29
    for (i in 1:15){ # 10 is more than enough!
      deltai <- sum(exp(-ri*x)*fx*Lx)-1
      # the mean generation time self-corrects 
      # according to the error produced by the Lotka equation
      ri <- ri+(deltai/(29-(deltai/ri)))
    }
    return(ri)	
  }

# Builds a life table by using the mortality rate schedule to calculate 
# the subsequent columns of the table
# Adapted from LifeTables::lt.mx
lt_mx <- function (nmx, sex = "female", age = c(0, 1, seq(5, 110, 5)), 
                   nax = NULL, radix = 1E6) {
  # browser()
  n <- c(diff(age), 999)
  if (is.null(nax)) {
    nax <- 0.5 * n
    if (n[2] == 4) {
      if (sex == "male") {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.33
          nax[2] <- 1.352
        }
        else {
          nax[1] <- 0.045 + 2.684 * nmx[1]
          nax[2] <- 1.651 - 2.816 * nmx[1]
        }
      }
      if (sex == "female") {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.35
          nax[2] <- 1.361
        }
        else {
          nax[1] <- 0.053 + 2.8 * nmx[1]
          nax[2] <- 1.522 - 1.518 * nmx[1]
        }
      }
    }
  }
  nqx <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx <- c(nqx[-(length(nqx))], 1)
  # nqx[nqx > 1] <- 1
  for (i in 1:length(nqx)) {
    if (nqx[i] > 1) nqx[i] <- 1
  }
  nage <- length(age)
  npx <- 1 - nqx
  l0 = radix
  lx <- round(cumprod(c(l0, npx)))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLx <- n * lxpn + ndx * nax
  
  # Deal with open age interval for Lx:
  # Normally, I would just take it from the abriged life tables
  # but this is not possible since the abridged life tables are 
  # also grouped in 5-calendar-year groupps so that there is no
  # 'original' value to get this from and it must be computed
  # anew.
  # Lx = lx/(Inf_nmx_100)
  # Note: This method will only work if lx > 0 for the
  # open age interval. In some cases, this is not true if the radix
  # is too small and the mortality too high (eg niger 1950)
  # For this reason, I cahnged to radix from 1E5 to 1E6 on 20200108
  
  nLx[nage] <- lx[nage]/nmx[nage]
  
  
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  
  # Without rounding
  lt <- data.frame(
    age = age
    , ax = c(nax[-length(nax)], NA)
    , mx = nmx
    , qx = nqx
    , px = npx
    , dx = ndx
    , lx = lx
    , Lx = nLx
    , Tx = Tx
    , ex = ex
  )
  
  lt.top.age <- min(which(nqx == 1))
  lt <- lt[1:lt.top.age, ]
  return(lt)
}


# lag_a is the number of year to be added to 'a'
# this is needed for estimating sandwichness
macohort <- function(cohort, xs, age_range, lag_a = 0, ASFRC, pop, LTCF){
  # if(cohort == 2034) browser()
  # print(paste("Working on cohort:", cohort))
  
  Mavec <- NULL
  
  for (a in age_range){
    # print(a)
    a <- a + lag_a
    
    t <- cohort + a
    
    fertvec <- NULL
    survvec <- NULL
    popvec <- NULL
    
    for (x in xs){
      # print(x)
      # Part 1: Get a vector of all _1F_{x, t-a-x}
      co <- t - a - x
      fert <- NULL
      fert <- ASFRC[ASFRC$Cohort == co & ASFRC$Age == x, "ASFR"]
      fertvec <- c(fertvec, fert)
      
      # Part 2: Get a vector of all _K_{x, t-a-x} values for women
      co <- t - a - x
      poptmp <- NULL
      poptmp <- pop[pop$cohort == co & pop$age == x, "Female"]
      popvec <- c(popvec, poptmp)
      
      # Part 3: Get a vector of all l_{x+a, t-a-x}
      co <- t - a - x
      survtmp<-NULL
      enu <- NULL
      deno <- NULL
      if (x + a <= 100){
        enu <- LTCF[LTCF$Cohort == co & LTCF$Age == x + a, "lx"]
        deno <- LTCF[LTCF$Cohort == co & LTCF$Age == x, "lx"]
        survtmp <- enu/deno
      }
      if (x + a > 100){	
        survtmp <- 0
      }
      survvec <- c(survvec, survtmp)
    }
    
    # Implement equation now that we have the relevant values
    
    Ma <- NULL
    Ma <- sum(fertvec * popvec / sum(fertvec * popvec) * survvec)
    Mavec <- c(Mavec, Ma)
  }
  
  return(Mavec)
}

macohort_stable <- function(lx, macb, age_range, lag_a = 0){
  
  macb100 <- rep(floor(macb), 101)
  ages <- 0:100 + lag_a
  
  num <- lx[macb100 + ages] 
  num[is.na(num)] <- 0
  den <- lx[macb100]
  
  m1 <- num/den
  
  m1[age_range]
  
}


# mean absolute error
mae <- function(real, predicted){
  e <- abs(real - predicted)
  return( mean(e) )
}

# Estimate sandwichness by hand using the KC model
# for several countries in parallel
# Could be parallelised
model_sandwich_estimates <- function(country_n, kappa = 5, tau = 5, ASFRC_all, LTCF_all, female_births_all, lx_df_full, cos_model, min_age_model, max_age_model, age_range_model, rep_age, return_components = T){
  # browser()
  # if(country_n == "guatemala") browser()
  
  print(" @@@@@@@@@@@ ")  
  print(paste("Starting work on country", country_n, "for cohorts:", paste(cos_model, collapse = ";")))
  # 1. Data format 
  
  # 1.1 Keep only data for relevant country 
  
  ASFRC <- 
    ASFRC_all %>% 
    filter(country %in% country_n)
  
  LTCF <- 
    LTCF_all %>% 
    filter(Country %in% country_n)
  
  female_births <- 
    female_births_all %>% 
    filter(country %in% country_n)
  
  pop <- 
    lx_df_full %>% 
    filter(country %in% country_n) %>% 
    select(country, cohort, age, Female = lx)
  
  # 2. Get equation components 
  
  # 2.1. Product of F_{a-x, c} 
  
  # This should only go to 5, according to equation
  
  # The probability that the mother of age 30 has at least one 
  # child less than kappa years old (ignoring the survivorship of children 
  # for the moment) should be [1 – Prob(woman had zero children in the
  # previous kappa years)].
  
  # If we consider f(a) as an approximation of having a child at age a, 
  # then we can think of giving birth as “dying” and [1 – f(a)] as the 
  # probability of “surviving” giving birth (=not giving birth). In
  # that case the probability that a woman of age 30 in front of us has
  # at least one child less than kappa years old should be: 
  #   
  # 1 -  { [1-f(30 - 1)]* [1-f(30 - 2)] * ... * [1-f(30 - kappa)]}
  # 
  # That is 1 minus the probability that she never gave birth during the previous kappa years.
  
  
  not_given_birth_mat <- matrix(NA,length(cos_model),length(age_range_model))
  rownames(not_given_birth_mat) <- paste(cos_model)
  colnames(not_given_birth_mat) <- age_range_model
  
  for (cohort in cos_model){
    not_given_birth_mat[paste(cohort),] <- prob_having_had_birth(
      cohort = cohort
      , xsf = 1:kappa
      , age_range = age_range_model
      , rep_age = rep_age
      , ASFRC = ASFRC
    )
  }
  
  given_birth_mat <- 1 - not_given_birth_mat
  
  # 2.2 M_{a,c} 
  
  # Probability that mother is alive for a randomly chosen 
  # individual at each age
  
  MaMAT <- matrix(NA,length(cos_model),length(age_range_model))
  rownames(MaMAT) <- paste(cos_model)
  
  for (cohort in cos_model){
    MaMAT[paste(cohort),] <- macohort(
      cohort = cohort
      , xs = xs
      , age_range = age_range_model
      , lag_a = 0
      , ASFRC = ASFRC
      , pop = pop
      , LTCF = LTCF
    )	
  }
  
  colnames(MaMAT) <- paste(min_age_model:max_age_model)
  
  # 2.3. M_{a+tau, c} 
  
  # Probability that mother is alive for a randomly chosen 
  # individual at each age a + tau years
  
  MaMAT_tau <- matrix(NA,length(cos_model),length(age_range_model))
  rownames(MaMAT_tau) <- paste(cos_model)
  
  for (cohort in cos_model){
    MaMAT_tau[paste(cohort),] <- macohort(
      cohort = cohort
      , xs = xs
      , age_range = age_range_model
      , lag_a = tau
      , ASFRC = ASFRC
      , pop = pop
      , LTCF = LTCF
    )	
  }
  
  colnames(MaMAT_tau) <- paste(min_age_model:max_age_model)
  
  # 3. Implement equation 
  
  s <- given_birth_mat * MaMAT * (1 - (MaMAT_tau / MaMAT ) )
  
  
  if(return_components) {
    out <- model_sandwich_components(s, given_birth_mat, MaMAT, MaMAT_tau, country_n)
  } else {
    out <- 
      s %>% 
      as.data.frame %>% 
      mutate(
        cohort = as.character(rownames(.))
      ) %>% 
      pivot_longer(-cohort, names_to = "age", values_to = "value") %>% 
      mutate(
        age = as.numeric(gsub("V", "", age))
        , value = value 
        , source = "Model"
        , country = country_n
      ) %>% 
      select(country, cohort, age, source, value)
  }
  
  return(out)
}

# Estimate sandwichness by hand using the KC model in a stable pop
# for several countries 
model_sandwich_estimates_stable <- function(country_n, kappa = 5, tau = 5, ASFRC_all, LTCF_all, cos_model, min_age_model, max_age_model, age_range_model, rep_age){
  # browser()
  # if(country_n == "guatemala") browser()
  
  print(" @@@@@@@@@@@ ")  
  print(paste("Starting work on country", country_n, "for cohorts:", paste(cos_model, collapse = ";")))
  # 1. Data format 
  
  # 1.1 Keep only data for relevant country 
  
  ASFRC <- 
    ASFRC_all %>% 
    filter(country %in% country_n)
  
  LTCF <- 
    LTCF_all %>% 
    filter(Country %in% country_n)
  
  
  # 2. Get equation components 
  
  # 2.1. Product of F_{a-x} 
  
  # This should only go to kappa, according to equation
  
  # The probability that the mother of age 30 has at least one 
  # child less than kappa years old (ignoring the survivorship of children 
  # for the moment) should be [1 – Prob(woman had zero children in the
  # previous kappa years)].
  
  # If we consider f(a) as an approximation of having a child at age a, 
  # then we can think of giving birth as “dying” and [1 – f(a)] as the 
  # probability of “surviving” giving birth (=not giving birth). In
  # that case the probability that a woman of age 30 in front of us has
  # at least one child less than kappa years old should be: 
  #   
  # 1 -  { [1-f(30 - 1)]* [1-f(30 - 2)] * ... * [1-f(30 - kappa)]}
  # 
  # That is 1 minus the probability that she never gave birth during the previous kappa years.
  
  
  not_given_birth_mat <- matrix(NA,length(cos_model),length(age_range_model))
  rownames(not_given_birth_mat) <- paste(cos_model)
  colnames(not_given_birth_mat) <- age_range_model
  
  for (cohort in cos_model){
    not_given_birth_mat[paste(cohort),] <- prob_having_had_birth(
      cohort = cohort
      , xsf = 1:kappa
      , age_range = age_range_model
      , rep_age = rep_age
      , ASFRC = ASFRC
    )
  }
  
  given_birth_mat <- 1 - not_given_birth_mat
  
  # 2.2 M_{a} 
  
  # Probability that mother is alive for a randomly chosen 
  # individual at each age
  
  # In a stable population, we can approximate $M_1(a)$ 
  # in relation to the mean age at childbearing $\mu$ 
  # if we assume this to be the average length of a 
  # generation \cite{Keyfitz2005}:
  
  # $$
  #   M_1(a) \approx \frac{l(\mu + a)}{l(\mu)}
  # $$
  
  # Get mean age at childbirth in stable pop
  # Returns named vector
  macbs <- get_MACB_stable_pop(LTCF, ASFRC, cos_model)
  
  # Iterates over multiple cohorts
  
  MaMAT <- matrix(NA,length(cos_model),length(age_range_model))
  rownames(MaMAT) <- paste(cos_model)
  
  for (cohort in cos_model){
    MaMAT[paste(cohort),] <- macohort_stable(
      lx = LTCF %>% filter(Cohort == cohort) %>% pull(lx)
      , macb = macbs[paste(cohort)]
      , age_range = age_range_model
      , lag_a = 0
    )	
  }
  
  colnames(MaMAT) <- paste(min_age_model:max_age_model)
  
  # 2.3. M_{a+tau, c} 
  
  # Probability that mother is alive for a randomly chosen 
  # individual at each age a + tau years
  
  MaMAT_tau <- matrix(NA,length(cos_model),length(age_range_model))
  rownames(MaMAT_tau) <- paste(cos_model)
  
  for (cohort in cos_model){
    MaMAT_tau[paste(cohort),] <- macohort_stable(
      lx = LTCF %>% filter(Cohort == cohort) %>% pull(lx)
      , macb = macbs[paste(cohort)]
      , age_range = age_range_model
      , lag_a = tau
    )	
  }
  
  colnames(MaMAT_tau) <- paste(min_age_model:max_age_model)
  
  # 3. Implement equation 
  
  s <- given_birth_mat * MaMAT * (1 - (MaMAT_tau / MaMAT ) )
  s[is.nan(s)] <- 0
  
  out <- 
    s %>% 
    as.data.frame %>% 
    mutate(
      cohort = as.character(rownames(.))
    ) %>% 
    pivot_longer(-cohort, names_to = "age", values_to = "value") %>% 
    mutate(
      age = as.numeric(gsub("V", "", age))
      , value = value 
      # , source = "Model"
      , country = country_n
    ) %>% 
    select(country, cohort, age, value)
  
  return(out)
}

# GIven the different components of Eq 1, return a df with each component
# listed separately to plot them
model_sandwich_components <- function(s, given_birth_mat, MaMAT, MaMAT5, country_n){
  
  s_df <- 
    s %>% 
    as.data.frame %>% 
    mutate(
      cohort = rownames(.)
    ) %>% 
    pivot_longer(-cohort, names_to = "age", values_to = "value") %>% 
    mutate(
      age = as.numeric(gsub("V", "", age))
      , value = value 
    )    
  
  # Plot the cumulative function 
  
  # Prob sanwiched
  
  d <- 
    s_df %>% 
    filter(between(age, 15, 55)) %>% 
    group_by(cohort) %>% 
    mutate(value = cumsum(value)) %>% 
    ungroup %>% 
    ggplot() +
    geom_line(aes(x = age, y = value, colour = cohort)) +
    theme_bw()
  
  prob_having_child <- 
    given_birth_mat %>% 
    as.data.frame %>% 
    mutate(
      cohort = rownames(.)
    ) %>% 
    pivot_longer(-cohort, names_to = "age", values_to = "value") %>% 
    pull(value)
  
  prob_mom_alive <- 
    MaMAT %>% 
    as.data.frame %>% 
    mutate(
      cohort = rownames(.)
    ) %>% 
    pivot_longer(-cohort, names_to = "age", values_to = "value") %>% 
    pull(value) 
  
  # Consolidate
  
  prob_mom_will_die <-  (1 - (MaMAT5 / MaMAT ) ) %>% 
    as.data.frame %>% 
    mutate(
      cohort = rownames(.)
    ) %>% 
    pivot_longer(-cohort, names_to = "age", values_to = "value") %>% 
    pull(value)
  
  all_df <- 
    s_df %>% 
    mutate(
      a_prob_having_child = prob_having_child
      , b_pob_mom_alive = prob_mom_alive
      , c_prob_mom_will_die = prob_mom_will_die
      , `d_mom_alive*mom_will_die` =  prob_mom_will_die*prob_mom_will_die
      # a_prob_having_child = prob_having_child
      # , `b_having_child*mom_alive` = prob_having_child*prob_mom_alive
      # , c_prob_mom_will_die = prob_mom_will_die
      # , `d_having_child*mom_alive*mom_will_die` = prob_having_child*prob_mom_alive*prob_mom_will_die
      
    ) %>% 
    # rename( prob_sandwich = value  ) %>% 
    select(- value) %>% 
    pivot_longer(cols = a_prob_having_child:`d_mom_alive*mom_will_die`, names_to = "variable", values_to = "value") %>% 
    mutate(country = country_n)  
    # filter(between(age, 15, 55))
  
  return(all_df)
  
}


# ADapted from Carl
mortRates <- function(opop, acat, firstmonth){
  
  # lower age bounds of age categories of mortality rates
  lastmonth <- max(opop$dod)  
  # drop the folks in the initial population
  # agedC is aged at death OR censor
  # agedCf is a factor with the age cats used in ratefile.min
  
  opop0<- 
    opop %>% 
    filter(dob >= firstmonth) %>% 
    mutate (
      agedC = ifelse(dod ==0,lastmonth,dod) - dob
      , agedCf = cut(agedC,breaks = acat,include.lowest=TRUE,right=FALSE)
      , aged = ifelse(dod == 0,NA,dod-dob)
      , agedf = cut(aged,breaks = acat,include.lowest=TRUE,right=FALSE)
    )
  
  # count the deaths by age and sex
  deaths<- 
    opop0 %>% 
    filter(dod != 0) %>% 
    count(agedf,fem) %>% 
    pivot_wider(names_from=fem, values_from=n)
  
  # count the numbers at risk -- the number alive at the begininning of each age cat
  
  atrisk <- 
    opop0 %>% 
    count(agedCf,fem) %>%  
    pivot_wider( names_from= fem, values_from = n)
  
  atrisk$x <- acat[-length(acat)]
  # there has to be a better way of doing a cumsum in tidyverse?
  atrisk <-
    atrisk %>% 
    arrange(-x) %>% 
    mutate(
      atrisk.m = cumsum(`0`)
      , atrisk.f = cumsum(`1`)
    ) %>% 
    arrange(x)
  
  mrates <- 
    left_join(
      deaths
      , atrisk
      , by=c("agedf" = "agedCf")
    )
  
  mrates$w <- diff(acat)
  
  mrates %>% 
    mutate(
      mort.m = 1-(1-(`0.x` / atrisk.m))^(1/w)
      , mort.f = 1-(1-(`1.x` / atrisk.f))^(1/w)
    ) %>% 
    select(agedf,mort.m,mort.f)
  
}


mx_load_and_clean <- function(who = "BOTH_SEXES", root = "../../Data/wpp_data/"){
  
  # load
  path <- paste0(root, who,".xlsx")
  
  lt_per_obs_B <- readxl::read_xlsx(
    path= path
    , sheet = "ESTIMATES"
    , skip = 16
  )
  
  # Predicted values for 2020-2050
  lt_per_pred1_B <- readxl::read_xlsx(
    path= path
    , sheet = "MEDIUM 2020-2050"
    , skip = 16
  )
  
  # Predicted values for 2050-2100
  lt_per_pred2_B <- readxl::read_xlsx(
    path= path
    , sheet = "MEDIUM 2050-2100"
    , skip = 16
  )
  
  lt_per_B <- bind_rows(
    lt_per_obs_B
    , lt_per_pred1_B
    , lt_per_pred2_B
  )
  
  # Format
  
  lt_per_B[lt_per_B == "…"] <- NA
  
  # Change column names of life tables
  
  new_names <- c("id", "variant", "country" , "notes", "country_code", "type", "parent_code",
                 "year", "age", "interval"
                 , "mx", "qx", "px", "lx", "dx", "Lx", "Sx", "Tx", "ex", "ax")
  
  # Change colnames
  colnames(lt_per_B) <- new_names
  
  lt <- 
    lt_per_B %>% 
    filter(type == "Country") %>%
    select(country, year, age, mx, ex) %>%
    mutate(
      country = fix_un_countries(country)
      , mx = as.numeric(mx)
      , ex = as.numeric(ex)
    )
  
  return(lt)
}

myfxHCLramp <- function(H,C=95,L,N=5){
  # H and L must be of equal length
  colsi <- c()
  for (i in 1:(length(H)-1)){
    Hi <- seq(H[i],H[i+1],length=(N+1))
    Hi[Hi<0] <- Hi[Hi<0]+360
    colsi <- c(colsi,hcl(h=Hi,c=C,l=seq(L[i],L[i+1],length=(N+1)))[ifelse(i==1,1,2):(N+1)])
  }
  colsi
}

# use to create a vector of positions to substitute with NA values
na_position <- function(n, rep_l, position_mother_death, m) {
  # print(n)
  
  p <- position_mother_death[n]
  
  if(!is.na(p)) rep_l[[n]][p:m]
  else numeric(0) 
}


parents_cols <- function(links, prof, infer_gender = T) {
  
  # To add gender of parent
  l <- merge(
    links,
    prof %>% select(profileid, gender),
    by.x = 'parent',
    by.y = 'profileid'
  ) 
  
  parents <- reshape2::dcast(
    l
    , ... ~ gender
    , value.var = 'parent'
    , fun.aggregate = sum
    , na.rm = F
  )
  
  colnames(parents) <- c("child", "mother", "father", "parent_sex_unknown")
  
  parents[parents==0] <- NA
  
  # Fill in missing parents if their gender can be inferred
  if(infer_gender) {
    
    add_f <-  !is.na(parents$parent_sex_unknown) & 
      is.na(parents$father) & !is.na(parents$mother)
    
    add_m <-  !is.na(parents$parent_sex_unknown) & 
      !is.na(parents$father) & is.na(parents$mother)
    
    parents$father[add_f] <- parents$parent_sex_unknown[add_f]
    parents$mother[add_m] <- parents$parent_sex_unknown[add_m]
    
    parents$parent_sex_unknown[add_f | add_m] <- NA
  }
  
  parents2 <- merge(
    prof
    , parents
    , by.x = 'profileid'
    , by.y = 'child'
    , all.x = T
  ) %>% 
    select(father, mother, parent_sex_unknown)
  
  return(parents2)
  
}


prob_having_had_birth <- function(cohort, xsf, age_range, rep_age, ASFRC) {
  
  # print(paste("Working on cohort:", cohort))
  
  survived_birth_vec <- NULL
  
  for (a in age_range){
    
    # Get a vector of all _1F_{x, t-a-x} values for women
    
    survived_birth_prod <- NULL
    
    for (x in xsf){
      
      survived_birth <- NULL
      
      if((x_lag <- a - x) %in% rep_age){
        # If we consider f(a) as an approximation of having a child at age a, 
        # then we can think of giving birth as “dying” and [1 – f(a)] as the 
        # probability of “surviving” giving birth (=not giving birth). 
        survived_birth <- 1 - ASFRC[ASFRC$Cohort == cohort & ASFRC$Age == x_lag, "ASFR"]  
      } else {
        survived_birth <- 1 - 0
      }
      
      survived_birth_prod <- prod(survived_birth_prod, survived_birth)
    }
    
    survived_birth_vec <- c(survived_birth_vec, survived_birth_prod)
    
  }
  
  return(survived_birth_vec)
}

# Created for fixing UN country names in Berkeley Unix system
remove_encoding <- function(x){
  x %>% 
    gsub("\xf4", "o", .) %>% 
    gsub("\xe7", "a", .) %>% 
    gsub("\xe9", "e", .)
}

# From TIm RIffe's Lotka package
# used to estimate Lotka's r
Rmomentn <-
  function(fx,Lx,x,n=0){
    sum((x^n)*fx*Lx)
  }

# This function returns, for each ego, the months during wich this ego was sandiwhced
# Value is a list where each element is a list of months correspnding to each ego
# and an element of length zero for egos who were never sandwiched
sandwich_months<- function(ego, opop, omar, kappa, tau, type = "maternal"){
  # browser()
  print(ego)
  library(dplyr)
  
  # Returns a data frame with a row for each type of relative
  # This can be made more efficient since we are only interseted in kids
  # not in all relatives
  # In the resulting df, there is one row per kid
  rels <- get_rels(ego, opop, omar, kappa, tau) 
  
  
  if(!is.null(rels)) {
    
    # A. KIDS
    # Get a matrix where rows are the months lived by ego's kid
    # until the kid reached aged kappa or died, whichever happened first
    # and columns are kids
    kids <- rels %>% filter(who == "kids")
    lowerMonths <-  mapply(':',kids$dob,kids$birthday_kappa)
    
    # Save as vector
    dim(lowerMonths)<-NULL
    lowerMonths<-unique(unlist(lowerMonths))  
    
    if(type == "maternal"){
      
      # B. Mothers
      # For mother, do the same but this time consider whether
      # mom is within tau years of dying
      # It's a bit confusing since in the rels df, "mother" is 
      # actually the ego and "gmother" is egos' mother
      gm <- rels %>% filter(who == "gmother")
      
      # In case all grandparents were still alive at end of simulation
      # then, their dod_tau cannot be estimated
      if( sum(gm$dod) == 0 ) upperMonths <- NA
      else upperMonths <- mapply(':', gm$dod_tau, gm$Dod)  
      
      
      
    } else if(type == "grandmaternal") {
      
      # PENDING
      # C. Grandmothers
      
      # For grandmother, do the same but this time consider whether
      # grandma is within 
      # In this case it's maternal grandma only since we filtered on
      # women
      
    }
    
    
    dim(upperMonths) <- NULL
    upperMonths <-unique(unlist(upperMonths))
    
    # Get the months during which an ego had both children and mothers 
    # or grandparents
    # close to death
    sandMonths <- dplyr::intersect(lowerMonths, upperMonths)
    
  } else {
    sandMonths <- numeric(0)
  }
  
  
  return(sandMonths)
}


sandwich_months_parallel <- function(opop, omar, bcs, current_country, kappa, tau, numCores = 2) {
  # browser()
  # if(current_country=="usa") browser()
  
  print(paste("Starting new simulation  for ", current_country))
  print(Sys.time() )
  
  # Non parallel
  # estimates_list <- lapply(bcs$pid, sandwich_months, opop, omar)
  
  cl <- makeCluster(numCores)
  
  clusterExport(
    cl
    , varlist = c("get_rels", "who2gen", "opop", "omar", "endmo", "kappa", "tau")
    , envir = environment()
  )
  
  # clusterApply(cl, fun = library(dplyr))
  
  estimates_list <- parLapply(cl, bcs$pid, sandwich_months, opop, omar, kappa, tau) 
  
  stopCluster(cl)
  
  return(estimates_list)
  
}


# What share of the pop will die in tau years?
# Runs on socsim output (transformed data formar socsim_opop...)
share_pop_within_tau_years_of_death <- function(year_range, tau, parallel = F, numCores = 20, Cave){
  
  opops <- list.files(pattern = "^socsim_opop", path = Cave, full.names = T)
  
  # Get a list with pop by age and sex
  if(!parallel){
    doomed_list <- lapply(opops, share_pop_within_tau_years_of_death1, year_range, tau, Cave)
  } else{
    
    cl <- makeCluster(numCores)
    
    clusterExport(
      cl
      , varlist = c("year_range", "tau", "Cave", "endmo", "asYr2", "FinalSimYear", "age_breaks_mort", "age_labels_mort", "census_socsim")
      , envir = environment()
    )
    
    doomed_list <- parLapply(cl, opops, share_pop_within_tau_years_of_death1, year_range, tau, Cave)
    
    stopCluster(cl)
    
  }
  
  doomed_df <- 
    bind_rows(doomed_list) %>% 
    # Get countries xxx
    fix_socsim_countries()
  
  doomed_df
  
}

share_pop_within_tau_years_of_death1 <- function(opop_name, year_range, tau = 5, Cave){
  
  library(dplyr)
  library(tidyr)
  
  simulation <- gsub(paste0(Cave, "|", "socsim_opop_|.csv"), "", opop_name)
  country <- gsub("_[0-9]+", "", simulation)
  
  print(paste0("Starting new sim: ", simulation))
  
  opop <- data.table::fread(opop_name, stringsAsFactors = F)
  
  opop2 <- 
    opop %>% 
    mutate(
      dod2 = ifelse(dod == 0, endmo, dod)
      , death_year = asYr2(dod2, FinalSimYear, endmo)
      , birth_year = asYr2(dob, FinalSimYear, endmo)
      , age_death_months = ifelse(dod == 0,NA,dod-dob)
      , age_death = trunc(age_death_months/12)
      , age_death_g = cut(age_death, breaks = age_breaks_mort, labels = age_labels_mort, include.lowest=TRUE,right=FALSE)
      , age_death_g = as.numeric(as.character(age_death_g))
    ) %>% 
    rename(gender = fem) %>% 
    select(pid, birth = birth_year, death = death_year, gender)
  
  
  will_die <- 
    lapply(year_range, census_socsim, df = opop2, return_ids = F, group_by_age_sex = F) %>% 
    do.call(rbind, .) %>% 
    data.frame() %>% 
    rename(fem = gender) %>% 
    mutate(
      age_at_census = census - birth
      , doomed = ifelse(death_temp - tau <= census, "die", "live")
    ) %>% 
    # Get number of cases by census and age and STATUS
    dplyr::count(census, age_at_census, doomed) %>% 
    tidyr::complete(census, age_at_census, doomed, fill = list(n = 0)) %>% 
    pivot_wider(names_from = "doomed", values_from = n) %>% 
    # group_by(census)
    mutate(
      share = die/(live+die)
      , simulation = simulation
      , country = country
      ) %>% 
    select(simulation, cohort = census, age = age_at_census, share, country)
  
  will_die
  
}

# Top level function that gets opop and omar csv files for a given number of simulations
# and returns no value but saves outputs to Data/estimate
socsim_sandwich_estimates_top <- function(kappa = 18, tau = 5, type = "sandwich", sim_range, paths_opop, paths_omar, Cave, path_out = "Data/estimates/", ages_socsim_in_months, in_laws = T, sex_keep = "both", pat){
  
  # browser()
  
  # PArameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  countries_long <- gsub(".csv","",gsub("^socsim_opop_", "", paths_opop))
  seed <- gsub("[A-z]+_","", countries_long)
  countries2 <-  gsub("_[0-9]+","", countries_long)
  con <- unique(countries2)
  
  # 0. Messages ~~~~~~~~~~~~~~~~~~~~~
  
  print(paste("~~~~~~~~~~~~ Starting range ~~~~~~~~~~~~~~~~~~"))
  print(paste("Simulation outputs for", con, "- there are", length(countries2), "simulations"))
  print(paste("Estimating share of sex ",sex_keep," in", type, "by age!"))
  print(paste("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
  
  # 
  # print(
  #   paste(
  #     "Estimating sandwichness for cohorts"
  #     # , paste(range(cos_or), collapse = "-")
  #     , "by taking the average of the +/-"
  #     , ifelse(cos_size == 1, 0, cos_size)
  #     , "years around each birth cohort from socsim"
  #   )
  # )
  
  # 1. Read opop and omar ~~~~~~~~~~
  
    # Load opop  as list
  
  opop_l <- lapply(paste0(Cave, "/", paths_opop), data.table::fread, stringsAsFactors = F)
  names(opop_l) <- countries_long
  
  # Load omar 
  
  omar_l <- lapply(paste0(Cave, "/", paths_omar), data.table::fread, stringsAsFactors = F)
  names(omar_l) <- countries_long
  
  # Create vector to re-use in mapply functionin function later on
  # ages_to_keep_socsim <-  split(rep(ages_socsim_in_months, length(countries2)), sort(rep(countries2, length(ages_socsim_in_months))) )
  
  # Since they are for each country, needs to split by simulation run
  ages_to_keep_socsim <-  split(rep(ages_socsim_in_months, length(countries2)), sort(rep(seed, length(ages_socsim_in_months))) )
  
  
  # 3. Estimations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  socsim_sandwich <- mapply(
    FUN = socsim_sandwich_estimates_flex
    , opop_l
    , omar_l
    , current_country = countries2
    , kappa = kappa
    , tau = tau
    , ages_to_keep = ages_to_keep_socsim
    , type = type
    , in_laws = in_laws
    , sex_keep = "both"
    # , average_across_simulation_years = T
    , SIMPLIFY = F
  )
  
  names(socsim_sandwich) <- countries_long
  # socsim_sandwich <- estimates_list[seq(1, 20, 2)]
  
  # 2. Format output as df ~~~~~~~~~~~~~~~~~~~
  
  socsim_estimates_formatted <- format_socsim_sandwich_output_country(socsim_sandwich)
  
  in_law_lab <- ifelse(in_laws, "in_laws", "blood_kin")
  
  # Export df
  write.csv(
    socsim_estimates_formatted
    # , paste0(path_out, "socsim_", type, "_", sex_keep, "_", in_law_lab, "_", kappa, "_", tau,"_",con,".csv")
    , paste0(path_out, "socsim_", pat, con,".csv")
    , row.names = F)
}

# Get sandwichness estiamtes from SOCSIM
# In a function to parallelise more easily
# Proportion women sandwiched over age.
# Considering if a women has a a child (boy or girl) aged kappa or less
# while having a parent within tau years of death.
# 
socsim_sandwich_estimates <- function(opop, omar, current_country, kappa = 5, tau = 5){
  # browser()
  # if(current_country=="usa") browser()
  # 1.2. Filter df 
  # Create data frame for analysis, which is 
  # Keep df with women in cohorts of interste only 
  # Note that this does not limit the sex of children
  # but only of the eg-mothers!
  
  bcs <- opop %>% 
    filter(asYr2(dob, FinalSimYear, endmo) %in% cos) %>% 
    filter(fem == 1)
  
  # 2. Determine months of sandwichness 
  
  # This returns a list where each element is a vector of the months
  # during which each ego was sandwiched. 
  # An element of length zero for egos who were never sandwiched
  
  # NOTE that this function return the values for WOMEN, not MOTHERS
  
  # This takes a while
  # Parallelise in Windows
  months_of_sandwich <- 
    sandwich_months_parallel(
      opop = opop
      , omar = omar
      , bcs = bcs
      , current_country = current_country
      , kappa = kappa
      , tau = tau
      , numCores = numCores
    )
  
  # 3. Sandwichness over life course 
  
  # Determine the number of months that ego (here called 'mother')
  # was actually sandwiched.
  # This means not which 'calendar months' someone was sandwiched
  # but which months of an ego's life she was sandwiched if 
  # the birth month is "month 0"
  
  # This function returnsa matrix of logicals:
  ego.months <- mapply(FUN = function(mother_dob, months_sandwiched){
    m_sand <- months_sandwiched - mother_dob
    res <- (ages_model_in_months %in% m_sand)
    return((res))
  }, bcs$dob, months_of_sandwich)
  
  # After flipping it, rows represents an egos lifecourse 
  # considering only the ages 15-55 (in months).
  # Each column represents, one ego-month, ie.
  # from ego's perspective one month and the content of the cells
  # indicated if ego was sandwiched in this month
  ego.months <- data.frame(t(ego.months)) 
  
  # 4. Account for mortality of egos 
  
  # This far, we are not account for the fact that not all
  # women will reach age a - some will die before that and
  # will therefore not experience the levels of snadwichness
  # prevalent at those ages.
  
  # One way of dealing with this is to replace the logical values
  # in ego.months with NA for women who died before they reached that age
  # In ego.months, rows are egos taken directly from bcd, so we can get their
  # dates of death from there too.
  # But remember that columns in ego.months represent ego's lifecourse
  # IN MONTHS!
  
  # Get the 'special' column of date of death, which
  # assigns enddate if person was still alive at end of simulation
  Dod <- ifelse(bcs$dod == 0, endmo, bcs$dod) 
  
  # Now, for each row, find the positions [ , position] that need
  # to be replaced with NA. These are ages at which a given ego 
  # was no longer alive and could have therefore not experienciend
  # sandwichness
  
  # An easy way to do this is with the age at death
  age_at_death_in_months <- Dod - bcs$dob
  
  # The age (columns) in ego.months do NOT start at age 0 months
  # but at the value defined in min_age_model*12
  # Rescale our vector accordingly
  # And add 1 to make sure that the first NA that is assinged
  # per row corresponds to the first month that a person was actually dead
  position <- age_at_death_in_months - min_age_model*12 + 1
  
  # Negative values are people who died before reaching age 180 months
  position[position <= 0] <- 1
  
  # -1 since the last column is birth year
  last_position <- ncol(ego.months) 
  
  # Remove positions after max age of interest
  position[position > last_position] <- last_position
  
  for(n in seq_along(position)) {
    ego.months[ n, position[n]:last_position] <- NA  
  }
  
  # Add extra columns
  
  ego.months$birthYr <- asYr2(bcs$dob, FinalSimYear, endmo)
  
  # 4.2. Transform to long 
  
  # When transforming to long, we remove all characteristic
  # identifying egos, so that each row tells us if sandwichness
  # was observed for a given birth_cohort/age cmbination
  # but we don't know who the ego is. This will come in handy
  # below, when we aggregate by birth cohort and age
  # This is huge since there is a cohort/age combination for each ego
  # even if they weren't sandwiched! This is, of course, what we need
  # since we'll get the proportion sandwiched considering those that were
  # and were not sandwiched!
  
  ego.months.long <- 
    ego.months %>% 
    pivot_longer(
      cols = - birthYr
      , names_to = "age_in_months"
      , values_to = "sandwiched_this_month"
    )
  
  # rm("ego.months")
  
  # 5. Get proportion sandwiched 
  
  # Getting the mean gives the proportional sandwiched.
  # This works because ego.months.long has all possible 
  # cohort/age combinations for each ego
  # even if they weren't sandwiched. 
  
  by.coh.age <-
    ego.months.long %>% 
    group_by(birthYr, age_in_months) %>% 
    summarise(proportion.sw = mean(sandwiched_this_month, na.rm = T)) %>% 
    ungroup %>% 
    mutate(age = parse_number(age_in_months)) %>% 
    arrange(birthYr, age)
  
  # rm("ego.months.long")
  
  # Convert to standard format
  
  sand_socsim_full <- 
    by.coh.age %>% 
    mutate(
      cohort = as.character(cut(birthYr, br, labels = labs, include.lowest = T))
      , age = round(15 +(age/12))
      , source = "SOCSIM (10-year mean)"
      , country = current_country
      , kappa_tau = paste0(kappa, "_", tau)
    ) %>% 
    select(country, cohort, birthYr, age, value = proportion.sw, source, kappa_tau) %>% 
    distinct(birthYr, age, .keep_all = T) 
    # filter(between(age, 15, 55))
  
  return(sand_socsim_full)
  
  # rm("by.coh.age")
  
}

# This function returns a list of opop or omar
# given a set of rsave files like the ones returned
# by socsim in the sandwich paper
unpack_simulation_rsaves <- function(Cave){
  
  SIMS <- list.files(Cave, pattern = "sims.Rsave", recursive = T, full.names = T)
  
  print(paste0("This function will overwite files in ", Cave))
  
  for(s in SIMS){
    
    print(s)
    load(s)
    
    # Names for output files
    n_out <- paste0(Cave, "/", "socsim_", c("opop", "omar"),"_", isoc$country, "_", isoc$seed, ".csv")
    
    write.csv(sims$opop, n_out[1], row.names = F)
    write.csv(sims$omar, n_out[2], row.names = F)
    
  }
  
  print(paste("opop and omar files saved as csv."))
  
}


# This function returns a list of opop or omar
# given a set of rsave files like the ones returned
# by socsim in the sandwich paper
unpack_simulation_rsaves <- function(Cave){
  
  SIMS <- list.files(Cave, pattern = "sims.Rsave", recursive = T, full.names = T)
  # SIMS <-   SIMS[grepl("Tanzania|Macedonia", SIMS)]
  
  print(paste0("This function will overwite files in ", Cave))
  
  for(s in SIMS){
    print(paste(s))
    load(s)
    
    # Names for output files
    n_out <- paste0(Cave, "/", "socsim_", c("opop", "omar"),"_", isoc$country, "_", isoc$seed, ".csv")
    
    write.csv(sims$opop, n_out[1], row.names = F)
    write.csv(sims$omar, n_out[2], row.names = F)
    
  }
  
  print(paste("opop and omar files saved as csv."))
  
}

rsaves_count_rows <- function(Cave){
  
  SIMS <- list.files(Cave, pattern = "sims.Rsave", recursive = T, full.names = T)
  
  lapply(SIMS, function(s) {
    load(s)
    nrow(sims$opop)
  })
  
}

# Get all the interesting relatives of ego
# 
who2gen<-function(w){
  ## used by get_rels to assign generation 
  generation<-data.frame(stringsAsFactors = F,
                         who = c("gmother" , "gfather", "s.parents",     "mother", "spouse", "kids", "stepkids" ),
                         gen = c(  -1,          -1,          -1,            0,        0,   1,           1)
  )
  return(generation[match(w,generation$who),]$gen)
}

worker_apply_lt2 <- function(con, countries, cohorts, female_births, LTCF) {
  print(con)
  
  l <- lapply(cohorts, function(coh) {
    
    radix <- female_births %>% 
      filter(country %in% con) %>% 
      filter(year %in% coh) %>% 
      pull(value)
    
    nmx <- LTCF %>% 
      filter(Country %in% con) %>% 
      filter(Cohort %in% coh) %>% 
      pull(mx)
    
    age <- 0:(length(nmx)-1)
    
    lt_mx(nmx = nmx, age = age, radix = radix) %>% 
      mutate(country = con, cohort = coh, age = age) %>% 
      select(country, cohort, age, lx)  
    
  })
  
  rbindlist(l, use.names = T)
  
}

worker_cum_events <- function(d, row_raw, age_range, FinalSimYear, endmo) {
  
  # EMpty row
  row <- row_raw
  
  # These are the woman's ages at which she experienced the death of a child
  # In reality, this is how old a woman would have been if she had survived 
  # to see all of her children die. 
  # As such, it does not reflect 'experienced' child deaths, but the master script
  # gives the option to account for the mortality of women, so this should not be an issue here
  
  deaths <- as.numeric(d)
  deaths <- na.omit(deaths)
  # Any deaths that happened at very old ages, assumed that they happened at maximum
  # age observed
  # This is done since otherwise, they would fall outside of the matrix range (ie
  # there are no more columns to the right to put these values in)
  deaths[deaths > max(age_range)] <- max(age_range)
  # Deaths before age 16 or wathever, assign to min age
  deaths[deaths < min(age_range)] <- min(age_range)
  # as I assinged an arbitrary age of 2999 to this cases
  # deaths[deaths > 120] <- NA
  
  if(length(deaths) > 0) {
    # This is implemented like this in case a mother lost two kids on when she was aged a
    freq <- table(deaths)
    freq_value <- as.numeric(freq)
    # Need to substract min(age_range) since the row does not start from age 1
    # Note that freq also orders the values but do it again just in case
    # These are the positions in the matrix
    position <- sort(as.numeric(names(freq))) - min(age_range) + 1
    
    # if(length(position) != length(freq_value)) browser()
    # if(length(row[position]) != length(freq_value)) print(wrong <<- wrong + 1)
    
    row[position] <- freq_value
    
    row_sum <- cumsum(row)
  } else{
    row_sum <- row
  }
  
  return(row_sum)
  
}


worker_get_dates_all <- function(ids, col, p) {
  
  if(all(is.na(ids))) {
    out <- NA
  } else {
    out <- as.numeric( unlist( p[ profileid %in% ids, col, with = FALSE] ) )
  }
  
  return(out)
}



yearly_birth_by_age_socsim <- function(df, sex_keep, y_range, age_breaks, age_labels) {
  
  col <- ifelse(sex_keep == "male", "pop", "mom")
  df$parent_birth <- df$birth_year[match(df[, col], df$pid)]
  
  out <- 
    df %>% 
    select(birth_year, parent_birth) %>% 
    filter(between(birth_year, min(y_range), max(y_range))) %>% 
    dplyr::mutate(
      parent_age = birth_year - parent_birth 
      , parent_agegr_factor = cut(parent_age, age_breaks, age_labels, include.lowest = TRUE, 
                           right = F)
      # , parent_agegr = as.character(parent_agegr_factor)
      , birth_year_factor = factor(birth_year, levels = y_range)
    ) %>%
    dplyr::count(birth_year = birth_year_factor, parent_agegr = parent_agegr_factor) %>% 
    tidyr::complete(birth_year, parent_agegr, fill = list(n = 0)) %>% 
    dplyr::filter(
      !is.na(parent_agegr)
      , birth_year %in% y_range 
    ) %>% 
    select(year = birth_year, agegr = parent_agegr, n) %>% 
    arrange(year, agegr)
  
  
  return(out)
  
}

z2na <- function(x) {x[x==0] <- NA; return(x)}


# New functions for grandsandwichness ----

get_rels_flex <- function(ego, opop, omar, kappa, tau){
  ## collect all relatives of ego; add some useful quantities
  if(opop[ego,]$mom == 0) {return(NULL)} # do not use initial population
  
  spouse <- omar[opop[ego,]$marid,]$hpid
  
  spouseids <- (omar %>% filter(wpid == ego))$hpid
  
  spouse <- opop %>% filter(pid %in% spouseids)
  
  kids <- opop %>% filter(mom == ego) 
  
  gkids <- opop %>% filter(mom %in% kids$pid)
  
  gmother <- opop %>% filter(pid %in% opop[ego,"mom"])
  
  gfather <- opop %>% filter(pid %in% opop[ego,"pop"])
  
  s.parents <- opop %>% filter(pid %in% unlist(opop[spouseids,c("mom","pop")]))
  
  mother <- opop[ego,]
  
  stepkids <- opop %>% filter(pop %in% spouseids, mom != ego)
  
  rels <- bind_rows(
    kids = kids
    , gkids = gkids
    , gmother = gmother
    , gfather = gfather
    , mother = mother
    , spouse = spouse
    , s.parents = s.parents
    , stepkids = stepkids
    , .id ="who"
  ) %>% 
    mutate(
      # Assign enddate if person was still alive at end of simulation
      Dod = ifelse(dod == 0, endmo, dod)
      # Age when last observed
      , aged = Dod - dob
      # Assign generation relative to ego
      , gen = who2gen(who)
      # now get the date that corresponds to 5 years before this person
      # died. Since this will mainly be used for grandmothers, it doesn't
      # matter that it may give negative values for people who lived less
      # than 5 years (eg child deaths)
      , dod_tau = ifelse(dod == 0, NA, Dod - tau*12)
      # Month in which this person would have celebrated their 
      # 5 birthday if they survice to that age
      , birthday_kappa = ifelse(Dod <= (dob + kappa*12), Dod, dob + kappa*12)
      # , birthday_10 = ifelse(Dod <= (dob + 120),Dod,dob+120)
    )
  
  return(rels)  
}


# ~~~~~~~~~~~ ------------
# EDIT in-laws here
# ~~~~~~~~~~~ ------------
# This function returns, for each ego, the months during wich this ego was sandiwhced
# or grandsanwiched.
# Value is a list where each element is a list of months correspnding to each ego
# and an element of length zero for egos who were never sandwiched
sandwich_months_flex <- function(ego, opop, omar, kappa, tau,  type = "sandwich", in_laws = T, sex_keep = "both"){
  # browser()
  # print(ego)
  library(dplyr)
  
  # Returns a data frame with a row for each type of relative
  # This can be made more efficient since we are only interseted in kids
  # not in all relatives
  # In the resulting df, there is one row per kid
  rels <- get_rels_flex(ego, opop, omar, kappa, tau) 
  
  if(!is.null(rels)) {
    
    # A. Mothers
    # For mother, do the same but this time consider whether
    # mom is within 5 years of dying
    # It's a bit confusing since in the rels df, "mother" is 
    # actually the ego and "gmother" is egos' mother
    
    # OLD CODE for women only
    # gm <- rels %>% filter(who == "gmother")
    
    # NEW code, for two-sex population
    # sex_keep <- "both"
    parent_gen <- switch(sex_keep, both = c("gmother", "gfather"), women = "gmother", men = "gfather")
    gm <- rels %>% filter(who %in% parent_gen)
    
    # In case all grandparents were still alive at end of simulation
    # then, their dod_tau cannot be estimated
    if( sum(gm$dod) == 0 ) upperMonths <- NA
    else upperMonths <- mapply(':', gm$dod_tau, gm$Dod)  
    
    dim(upperMonths) <- NULL
    upperMonths <-unique(unlist(upperMonths))

    # B. In-laws
    if(in_laws){
      # Note that get_rels_flex already returns step-parents as (s.parents)
      
      s_parents <- rels %>% filter(who == "s.parents")
      
      if( sum(s_parents$dod) == 0 ) stepMonths <- NA
      else stepMonths <-  mapply(':',s_parents$dod_tau, s_parents$Dod)
      
      # Save as vector
      dim(stepMonths)<-NULL
      stepMonths <- unique(unlist(stepMonths))
      
      # Append to parent vector of months
      upperMonths <- unique(c(upperMonths, stepMonths))
    }
    
    # C. Children or grandchildren
    
    if(type == "sandwich"){
      
      # A. KIDS
      # Get a matrix where rows are the months lived by ego's kid
      # until the kid reached aged kappa or died, whichever happened first
      # and columns are kids
      kids <- rels %>% filter(who == "kids")
      lowerMonths <-  mapply(':',kids$dob,kids$birthday_kappa)
      
      # Save as vector
      dim(lowerMonths)<-NULL
      lowerMonths <- unique(unlist(lowerMonths))    
      
    } else if(type == "grandsandwich") {
      
      # A. GRANDCHILDREN
      # Get a matrix where rows are the months lived by ego's grandkid
      # until the grandkid reached aged tau or dies, whichever happened first
      # and columns are grandkids
      gkids <- rels %>% filter(who == "gkids")
      lowerMonths <-  mapply(':', gkids$dob, gkids$birthday_kappa)
      
      # Save as vector
      dim(lowerMonths)<-NULL
      lowerMonths <- unique(unlist(lowerMonths))  
      
    }
    
    
    # Get the months during which an ego had both children and mothers 
    # or grandparents
    # close to death
    sandMonths <- dplyr::intersect(lowerMonths, upperMonths)
    
  } else {
    sandMonths <- numeric(0)
  }
  
  
  return(sandMonths)
}

sandwich_months_parallel_flex <- function(opop, omar, kappa, tau, bcs, current_country, type = "sandwich", in_laws = T, sex_keep = "both", numCores = 2) {
  # browser()
  # if(current_country=="usa") browser()
  
  print(paste0("Starting new simulation  for ", current_country))
  print(Sys.time() )
  
  os <- Sys.info()["sysname"]
  
  # Non parallel (for legacy only)
  # estimates_list <- lapply(bcs$pid, sandwich_months, opop, omar)

  # Parallel  
  if(os == "Windows"){
    cl <- makeCluster(numCores)
    
    clusterExport(
      cl
      , varlist = c("get_rels_flex", "who2gen", "opop", "omar", "endmo", "type", "kappa", "tau", "in_laws", "sex_keep")
      , envir = environment()
    )
    
    estimates_list <- parLapply(cl, bcs$pid, sandwich_months_flex, opop, omar, kappa, tau, type, in_laws, sex_keep) 
    
    stopCluster(cl)    
  } else if(os == "Linux") {
    estimates_list <- mclapply(bcs$pid, sandwich_months_flex, mc.cores = numCores, opop, omar, kappa, tau, type, in_laws)
  }
  
  return(estimates_list)
  
}


# Get sandwichness estiamtes from SOCSIM
# In a function to parallelise more easily
# Proportion women sandwiched over age.
# Considering if a women has a a child (boy or girl) aged kappa or less
# while having a parent within tau years of death.
# age_to_keep limits the lifecourse ages of women in which to consider sandwichness
socsim_sandwich_estimates_flex <- function(opop, omar, current_country, kappa = 5, tau = 5, ages_to_keep, type = "sandwich", in_laws = T, sex_keep = "both"){
  # browser()
  # if(current_country=="usa") browser()
  # 1.2. Filter df 
  # Create data frame for analysis, which is 
  # Keep df with women in cohorts of interste only 
  # Note that this does not limit the sex of children
  # but only of the eg-mothers!
  
  sex_filter <- switch (sex_keep, women = 1, men = 0, both = c(0,1) )
  
  bcs <- 
    opop %>% 
    filter(asYr2(dob, FinalSimYear, endmo) %in% cos) %>% 
    filter(fem %in% sex_filter)
  
  # 2. Determine months of sandwichness 
  
  # This returns a list where each element is a vector of the months
  # during which each ego was sandwiched. 
  # An element of length zero for egos who were never sandwiched
  # This takes a while
  # Parallelise in Windows
  months_of_sandwich <- 
    sandwich_months_parallel_flex(
      opop = opop
      , omar = omar
      , kappa = kappa
      , tau = tau
      , bcs = bcs
      , current_country = current_country
      , type = type
      , in_laws = in_laws
      , sex_keep = "both"
      , numCores = numCores
    )
  # NOTE that this function return the values for WOMEN, not MOTHERS
  
  # browser()
  
  # 3. Sandwichness over life course 
  
  # Determine the number of months that ego (here called 'mother')
  # was actually sandwiched.
  # This means not which 'calendar months' someone was sandwiched
  # but which months of an ego's life she was sandwiched if 
  # the birth month is "month 0"
  
  # This function returnsa matrix of logicals:
  ego.months <- mapply(FUN = function(mother_dob, months_sandwiched){
    m_sand <- months_sandwiched - mother_dob
    res <- (ages_to_keep %in% m_sand)
    return((res))
  }, bcs$dob, months_of_sandwich)
  
  # After flipping it, rows represents an egos lifecourse 
  # considering only the ages 15-55 (in months).
  # Each column represents, one ego-month, ie.
  # from ego's perspective one month and the content of the cells
  # indicated if ego was sandwiched in this month
  ego.months <- data.frame(t(ego.months)) 
  
  # 4. Account for mortality of egos 
  
  # This far, we are not account for the fact that not all
  # women will reach age a - some will die before that and
  # will therefore not experience the levels of snadwichness
  # prevalent at those ages.
  
  # One way of dealing with this is to replace the logical values
  # in ego.months with NA for women who died before they reached that age
  # In ego.months, rows are egos taken directly from bcd, so we can get their
  # dates of death from there too.
  # But remember that columns in ego.months represent ego's lifecourse
  # IN MONTHS!
  
  # Get the 'special' column of date of death, which
  # assigns enddate if person was still alive at end of simulation
  Dod <- ifelse(bcs$dod == 0, endmo, bcs$dod) 
  
  # Now, for each row, find the positions [ , position] that need
  # to be replaced with NA. These are ages at which a given ego 
  # was no longer alive and could have therefore not experienciend
  # sandwichness
  
  # An easy way to do this is with the age at death
  age_at_death_in_months <- Dod - bcs$dob
  
  # The age (columns) in ego.months do NOT start at age 0 months
  # but at the value defined in min_age_model*12
  # Rescale our vector accordingly
  # And add 1 to make sure that the first NA that is assinged
  # per row corresponds to the first month that a person was actually dead
  position <- age_at_death_in_months - min_age_model*12 + 1
  
  # Negative values are people who died before reaching age 180 months
  position[position <= 0] <- 1
  
  # -1 since the last column is birth year
  last_position <- ncol(ego.months) 
  
  # Remove positions after max age of interest
  position[position > last_position] <- last_position
  
  for(n in seq_along(position)) {
    ego.months[ n, position[n]:last_position] <- NA  
  }
  
  # Add extra columns
  
  ego.months$birthYr <- asYr2(bcs$dob, FinalSimYear, endmo)
  
  # 4.2. Transform to long 
  
  # When transforming to long, we remove all characteristic
  # identifying egos, so that each row tells us if sandwichness
  # was observed for a given birth_cohort/age cmbination
  # but we don't know who the ego is. This will come in handy
  # below, when we aggregate by birth cohort and age
  # This is huge since there is a cohort/age combination for each ego
  # even if they weren't sandwiched! This is, of course, what we need
  # since we'll get the proportion sandwiched considering those that were
  # and were not sandwiched!
  
  ego.months.long <- 
    ego.months %>% 
    pivot_longer(
      cols = - birthYr
      , names_to = "age_in_months"
      , values_to = "sandwiched_this_month"
    )
  
  # rm("ego.months")
  
  # 5. Get proportion sandwiched 
  
  # Getting the mean gives the proportional sandwiched.
  # This works because ego.months.long has all possible 
  # cohort/age combinations for each ego
  # even if they weren't sandwiched. 
  
  by.coh.age <-
    ego.months.long %>% 
    group_by(birthYr, age_in_months) %>% 
    summarise(proportion.sw = mean(sandwiched_this_month, na.rm = T)) %>% 
    ungroup %>% 
    mutate(age = parse_number(age_in_months)) %>% 
    arrange(birthYr, age)
  
  # rm("ego.months.long")
  
  # Convert to standard format
  
  # labs and br parameters defined in parameters.R
  
  out <- 
    by.coh.age %>% 
    mutate(
      cohort = as.character(cut(birthYr, br, labels = labs, include.lowest = T))
      , age = round(15 +(age/12))
      , source = "SOCSIM (10-year mean)"
      , country = current_country
      , kappa_tau = paste0(kappa, "_", tau)
    ) %>% 
    select(country, cohort, birthYr, age, value = proportion.sw, source, kappa_tau) %>% 
    distinct(birthYr, age, .keep_all = T)  

  return(out)
  
}
