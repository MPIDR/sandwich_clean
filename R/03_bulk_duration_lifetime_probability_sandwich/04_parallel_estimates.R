# Parallelisation # Code adapted from Carl's: 
# 01ProcessSim.r 
# See 04_example_for_one_country to see how the actual crunching of the simulation
# works for on country only as a example.

# NOTES FOR INTERPRETING THE OUTPUT
# Output for each country-simulation is a named list where $by.Cohort
# stores the  main results, somethink of the form:

# $by.Cohort
#                         1970        2040
# mnt.k_kappa       221.6950147 204.8602151
# mnt.gk_kappa      198.1085044 175.8086022
# mnt.Pndeath       159.5351906 167.0645161
# mnt.sandwich       55.8533724  53.2602151
# mnt.gsandwich      56.0527859  46.8559140
# mnt.everborn        1.9677419   1.7397849
# mnt.geverborn       3.6950147   2.9440860
# mnt.young.grent   402.7562408 381.1440860
# mnt.young.ggrent  175.6291513 145.8636364
# mnt.cond.sandwich   0.5323547   0.5806319
# mnt.cond.happy      0.6538797   0.6196405
# mnt.Nhrents.k5      1.8383448   1.7777082
# mnt.Nndrents.k5     1.3340807   1.4530888
# mnt.nwrk.1218       4.0986641   3.3818160
# mnt.nndeath.1218    0.1017821   0.0408592
# mnt.nk5.1218        1.4916432   0.7220332

# I still need to write a codebook of what all of these things mean, but
# for now the most relevant are: 

# - mnt.sandwich
# Months of life spent sandwiched
# - mnt.gsandwich
# Months of life spent grandsandwiched
# - mnt.k_kappa
# Months of live spent with a child under kappa years og age
# - mnt.gk_kappa
# Months of live spent with a grandchild under kappa years og age
# - mnt.Pndeath
# mnths ego alive with parents near death

# UPDATE 20201209 ~~~~~~~~~~~
# The main results now include rows for the 
# share of a cohort that is expected to spend
# at least one year (>=12months) (grand)sandwiched
# "share.sand"
# "share.gsand"
# ~~~~~~~~~~~

# 1. locate all the sims files ----

SIMS <- list.files(Cave, pattern = "sims.Rsave", recursive = T, full.names = T)

# SIMS <- SIMS[grep("anzania",SIMS)]
# SIMS <- SIMS[grep("fgha",SIMS)]
# SIMS[grep("acedonia",SIMS)]
# SIMS <- SIMS[grep("USA",SIMS)]
# SIMS <- SIMS[grep("echia",SIMS)]

if(!length(SIMS)){
  print("Can't find any sims.Rsaves bailing out")
  stop()
}


# 2. Run estiamtions in parallel -----

cl <- makeCluster(numCores)
registerDoParallel(cl)
getDoParWorkers() #see how many cores can be used symultaneously

#~ split the SIMS into chunks that will run separately
#~ JUST in case something goes wrong we won't lose the whole batch
XIMS<-split(SIMS,f=trunc((1:length(SIMS)/numCores)))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TEMP
# Re-run for selected countries only
# XIMS<-split(
#   # SIMS[grepl("Tanzania|Afghanistan|Macedonia|Czec", SIMS)]
#   # , sort(rep(1:4, 5))
#   # SIMS[grepl("Tanzania", SIMS)]
#   SIMS[grepl("Afghanistan|Tanzania|Macedonia", SIMS)]
#   # , sort(rep(1:5, 5))
#   , c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L)
# )
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

foo <-list()

for(i in 1:length(XIMS)){
  #~for(i in 1:2){
  #~ Test for existance of results to prevent accidental overwrite
  #~ and to allow u to rerun without starting over
  # bufile <- sprintf("%s/%03d.Rsave",ResultsRepo,i)
  bufile <- sprintf("%s/%03d.Rsave",out_temp_specific,i)
  print(paste0("Start working on ", bufile))
  
  #~ the silent option does not work so this WILL generate
  #~ error messages but it will keep rolling so it goes.
  suppressWarnings(
    ok <- try(load(file=bufile),silent=TRUE)  
  )
  
  # if(class(ok) != "try-error"){
  #   #~ if bufile is loadable then we skip this
  #   print(paste(bufile," exists and works... skipping Crunch"))
  #   next
  # }
  #~ Here is where the work gets done
  #~ run crunch and store results in foo
  
  foo[[i]] <- try(foreach(sfile = XIMS[[i]]) %dopar% {
    print(sfile)
    # sfile = XIMS[[i]][1]
    # sfile <- XIMS[[1]][23]
    # crunchRes <- CrunchSim(sfile,Obj)
    
    crunchRes <-
      CrunchSim(
      sfile
      , Obj = NA
      , kappa_kids = kappa_kids
      , kappa_gkids = kappa_gkids
      , tau = tau
      , brtYR
      # DECLARE which outputs you would like the function to return:
      # Number of months, on average, that members of each birth cohort will spend in 
      # a state of sandwichness throughout all of their lifes
      , duration_of_sandwich = T
      # If include_in_laws, parents in law (of last spouse only) will also be considered
      # when estimating the months spent sandwiched
      , include_in_laws = include_in_laws
      , first_experience_of_death = T
      , kinship_network_size = T
      # , sex_keep = sex_keep
    )
    
    resfile <- sprintf(
      "%s/%s%03dcrunch.Rsave",
      # ResultsRepo,
      out_temp_specific,
      crunchRes$sim.name,crunchRes$seed
    )
    
    print(resfile)
    save(file=resfile,crunchRes)
    return(crunchRes)
  }
  )
  
  
  #~ if the crunch went well, save the result as a numbered Rsave
  if(class(foo[[i]]) != "try-error"){
    #~ %dopar% worked 
    FOO<-foo[[i]]
    save(file = bufile, FOO)
    print(paste(bufile, " written"))
  }else{
    print(paste("something screwed up in XIMS i=",
                i,bufile,"Not written"))
  }
  #~ erase FOO so we don't accidently save it if next run screws up
  try(rm(FOO),silent=TRUE)
}

stopCluster(cl)


print(paste("%dopar% estimation done! Each iteration saved a file of the form"
            , "'Guatemala_Medium433035crunch.Rsave'", "in",   out_temp_specific))

print(Sys.time())

# 3. Export ---------------------------------------------------

#~ foo holds ALL results save it temporarily  then we'll change
#~ it's shape and save it better
save(file = paste0(out_temp_specific, "/tempsavefoo.Rsave"),foo)
#~load(file="/72hours/tempsavefoo.Rsave")

print(paste("Large (unpractical) 'foo' file saved as ", paste0(out_temp_specific, "/tempsavefoo.Rsave")))

#~ make foo into something more useable
goo<-list()
count<-0
for(i in 1:length(foo)){
  for(j in 1:length(foo[[i]])){
    count<-count+1
    goo[[count]]<-foo[[i]][[j]]
  }
}

length(goo)

foo <- goo

names(foo) <- 
  unlist(sapply(foo,function(x){
    return(paste(x["sim.name"],x["seed"],sep='_'))
  }))

#~ Finalfoo.Rsave is the thing that 01Analyse will use

save(file = paste0(out_temp_specific, "/", pat, "_Finalfoo.Rsave"), foo)
# load(file= paste0(out_temp_specific, "/", pat, "_Finalfoo.Rsave"))


# save(file= paste0(out_temp, "/", kappa_kids, "_", tau, "_and_",  kappa_gkids, "_", tau,"/Finalfoo.Rsave"), foo)
# load(file= paste0(out_temp, "/", kappa_kids, "_", tau, "_and_",  kappa_gkids, "_", tau,"/Finalfoo.Rsave"))

# Export months sandwiched as csv ======================

# Since some gave error, just ignore them for now...

# Identify who gave error

status <- unlist(lapply(foo, '[[', 1))
failed <- grepl("Error", status)

results <- foo[!failed]

# Keep only relevant data

cols_keep <- c(
  # This is duration sadnwcihed
  "mnt.sandwich", "mnt.gsandwich"
  # This is the duration with a dependent child
  , "mnt.k_kappa", "mnt.gk_kappa"
  # This are months with parent close to death
  , "mnt.Pndeath" 
  , "share.sand"
  , "share.gsand"
               )

# Key! Re-load data to avoid issues


file_names <- list.files(path = out_temp_specific, pattern = "_Medium[0-9]+crunch.Rsave", full.names = T)

from_output <- lapply(file_names, function(f){
  load(f)
  crunchRes
})

# crunchRes = from_output[[1]]

sandwich_duration_months <- 
  data.frame(
    # do.call(rbind,   lapply(results, function(crunchRes){
    do.call(rbind,   lapply(from_output, function(crunchRes){
      t(crunchRes$by.Cohort)[ , cols_keep] %>% 
        data.frame() %>% 
        rownames_to_column() %>% 
        mutate(
          original = gsub("_Medium", "", crunchRes$sim.name)
          , original = recode(original
                              , China_Macao_SAR = "Macao"
                              , China_Taiwan_Province_of_China = "Taiwan"
                              , China_Hong_Kong_SAR = "Hong Kong"
                              , Eswatini = "Swaziland"
                              , Lao_People_s_Democratic_Republic= "Laos"
                              , R_union = "Reunion"
                              , Republic_of_Korea = "Korea"
                              , Saint_Lucia = "Saint Lucia"
                              , Channel_Islands= "Channel Islands"
                              , Cura_ao = "Curacao"
          )
        )
    })
    )) %>% 
  mutate(country = countrycode(original, origin = "country.name", destination = "iso3c", warn = T))

colnames(sandwich_duration_months) <- c("cohort"
                                        , "sandwiched", "gsandwiched"
                                        , "with_child", "with_gchild"
                                        , "with_frail_parent"
                                        # 20201207 new size
                                        , "share_sand", "share_gsand"
                                        , "original", "country"
                                        )

# sandwich_duration_months$country %>% unique %>% sort

write.csv(
  sandwich_duration_months
  ,  paste0("Data/estimates/duration_",pat,".csv")
  # ,  paste0("../../Data/estimates/duration_",pat,".csv")
  , row.names = F)

print(paste("All estimates crunched and saved in their final form in", paste0("Data/estimates/duration_",pat,".csv")))
