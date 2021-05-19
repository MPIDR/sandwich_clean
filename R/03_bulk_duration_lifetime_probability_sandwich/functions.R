# 
# FROM CARL for duration of sandwichness and first experience of death -----------------

# a function to convert socsim months to calendar years
asYr <- function(x) {
  return(trunc(FinalSimYear - (endmo - x)/12) +1)
}

# ~~ add2opop =======

add2opop<-function(opop,omar){
  ## Create additional columns of opop for things like Mother's
  ## mother's age at birth of ego. These will be used by both cohort
  ## and period results
  
  opop$spouse<-ifelse(opop$fem,
                      (omar[zna(opop$marid),"hpid"]),
                      (omar[zna(opop$marid),"wpid"]))
  
  ## Age at own death
  opop$aod<- opop$dod-opop$dob
  ## age of (g)mother at ego's birth
  opop$Mage<- opop$dob- opop$dob[zna(opop$mom)]
  opop$GMage<- opop$dob- opop$dob[ zna(opop$mom[ zna(opop$mom)])]
  ## Age of (g)mom at birth of AVERAGE kid 
  Mage<-with(opop,tapply(Mage,asYr(dob),mean))
  
  GMage<-with(opop,tapply(GMage,asYr(dob),mean))
  ## month of mom's death 
  opop$Mdeath<- zna(opop[zna(opop$mom),"dod"])  ##NA if mom is still alive
  opop$Fdeath<- zna(opop[zna(opop$pop),"dod"])  ##NA if father is still alive
  ### Grandparental Death month
  ## month of Maternal Grandmother's death
  opop$MMdeath<-opop$GMdeath<-opop$Mdeath[match(opop$mom, opop$pid)]
  #+#
  opop$FMdeath<-opop$Mdeath[match(opop$pop, opop$pid)]
  opop$MFdeath<-opop$Fdeath[match(opop$mom, opop$pid)]
  opop$FFdeath<-opop$Fdeath[match(opop$pop, opop$pid)]
  #-#
  ## great grandmother?
  opop$MMMdeath<-opop$GGMdeath<-opop$GMdeath[match(opop$mom, opop$pid)]
  #+#
  opop$MMFdeath<-opop$MFdeath[match(opop$mom, opop$pid)]
  opop$MFFdeath<-opop$FFdeath[match(opop$mom, opop$pid)]
  opop$MFMdeath<-opop$FMdeath[match(opop$mom, opop$pid)]
  
  opop$FMMdeath<-opop$MMdeath[match(opop$pop, opop$pid)]
  opop$FMFdeath<-opop$MFdeath[match(opop$pop, opop$pid)]
  opop$FFFdeath<-opop$FFdeath[match(opop$pop, opop$pid)]
  opop$FFMdeath<-opop$FMdeath[match(opop$pop, opop$pid)]
  #-#
  
  
  ##age of ego at (G)Gmom's death
  opop$ageMMdeath<-opop$ageGmomMDeath<- opop$GMdeath - opop$dob
  opop$ageMMMdeath<-opop$ageGGmomMDeath<- opop$GGMdeath - opop$dob
  #+#
  opop$ageMFdeath<- opop$MFdeath - opop$dob
  opop$ageFFdeath<- opop$FFdeath - opop$dob
  opop$ageFMdeath<- opop$FMdeath - opop$dob
  
  opop$ageMMFdeath<- opop$MMFdeath - opop$dob
  opop$ageMFFdeath<- opop$MFFdeath - opop$dob
  opop$ageMFMdeath<- opop$MFMdeath - opop$dob    
  
  opop$ageFMMdeath<- opop$FMFdeath - opop$dob
  opop$ageFMFdeath<- opop$FMFdeath - opop$dob
  opop$ageFFFdeath<- opop$FFFdeath - opop$dob
  opop$ageFFMdeath<- opop$FFMdeath - opop$dob    
  
  
  opop$FM<-opop$mom[match(opop$pop, opop$pid)]
  opop$MM<-opop$mom[match(opop$mom, opop$pid)]
  opop$MF<-opop$pop[match(opop$mom, opop$pid)]
  opop$FF<-opop$pop[match(opop$pop, opop$pid)]
  
  opop$FFM<-opop$FM[match(opop$pop, opop$pid)]
  opop$FMM<-opop$MM[match(opop$pop, opop$pid)]
  opop$FMF<-opop$MF[match(opop$pop, opop$pid)]
  opop$FFF<-opop$FF[match(opop$pop, opop$pid)]
  
  opop$MFM<-opop$FM[match(opop$mom, opop$pid)]
  opop$MMM<-opop$MM[match(opop$mom, opop$pid)]
  opop$MMF<-opop$MF[match(opop$mom, opop$pid)]
  opop$MFF<-opop$FF[match(opop$mom, opop$pid)]
  opop$cohort<-asYr(opop$dob)
  #-#
  return(opop)
}

at15<-function(pid,opop,KidsOf,eage=15*12, tau){
  # browser()
  ##at15<-function(pid,eage=15*12){
  ## expects a VECTOR of pids uses KidsOf
  ## expects pesonid, opop (post add2opop) and kidsOf returns and
  ## object full of information on the generational breakdown of famil
  ## on ego's 15th (or other) birthday
  ##pid<-21688
  ##  if(opop[pid,"dod"]-opop[pid,"dob"]< eage)  return()
  
  res<-list(
    ## ego is in g3 
    pid=pid)
  
  ## great grandparents
  res$g0=lapply(pid,
                function(pid){
                  unlist(opop[pid,
                              c("MMM","MMF","MFM","MFF",
                                "FMM","FMF","FFM","FFF")])})
  
  
  ## gparents
  
  res$g1<-lapply(pid,
                 function(pid){
                   unlist(opop[pid,
                               c("MM","MF","FM","FF")])})
  
  
  
  ## parents, uncles aunts
  
  res$g2<-lapply(res$g1,
                 function(x){unique(as.vector(unlist(KidsOf[x])))})
  
  ##cousins and siblings
  res$g3<- lapply(res$g2,
                  function(x){unique(as.vector(unlist(KidsOf[x])))})
  
  res$rents<-lapply(pid,function(p){unlist(opop[p,c("mom","pop")])})
  ##sibs only
  res$sibs<-lapply(res$rents,
                   function(x){unique(as.vector(unlist(KidsOf[x])))})
  
  ## children nieces nephews
  res$g4<-lapply(res$sibs,
                 function(x){unique(as.vector(unlist(KidsOf[x])))})
  
  ##
  ref.month<-opop[pid,"dob"]+eage
  
  # browser()
  return(
    c(
      at15.lt5.sibs=
        mean(unlist(lapply(Vlt5yr(res$sibs,opop,ref.month),sum))),
      at15.lt5.g4=
        mean(unlist(lapply(Vlt5yr(res$g4,opop,ref.month),sum))),
      at15.wrk.g1=
        mean(unlist(lapply(wk1<-Vwrkage(res$g1,opop,ref.month),sum))),
      
      at15.wrk.rents=
        mean(unlist(lapply(wk2<-Vwrkage(res$rents,opop,ref.month),sum))),
      
      at15.wrk.sibs=
        mean(unlist(lapply(wk3<-Vwrkage(res$sibs,opop,ref.month),sum))),
      
      at15.wrk.all=
        mean(unlist(mapply(wk1,wk2,wk3,SIMPLIFY=FALSE,
                           FUN=sum))),
      
      at15.nd.g0 = mean(unlist(lapply(x0<-VnearDeath(res$g0,opop,ref.month),sum))),
      at15.nd.g1 = mean(unlist(lapply(x1<-VnearDeath(res$g1,opop,ref.month),sum))),
      at15.nd.rent = mean(unlist(lapply(x2 <- VnearDeath(res$rents,opop,ref.month),sum))),
      at15.nd.sibs = mean(unlist(lapply(x3<-VnearDeath(res$sibs,opop,ref.month),sum))),
      
      at15.nd.all = mean(unlist(mapply(x0,x1,x2,x3,SIMPLIFY=FALSE,
                           FUN=sum)))
    )
  )
}

at65<-function(pid,opop,KidsOf,eage=65*12, tau){
  ##pid<-21688
  ## expects VECTOR of pid, opop (post add2opop) and KidsOf returns and
  ## returns mean numbers of kin of certain ages on ego's 65 birthday
  
  
  ## function for extracting children into convenient list form
  ko<-function(p){
    lapply(p,function(x){unique(as.vector(unlist(KidsOf[x])))})
  }
  
  res<-list(
    ## ego is in g3 
    pid=pid
    
  )                
  
  ## parents
  res$rents<-lapply(pid,function(p){unlist(opop[p,c("mom","pop")])})
  
  
  res$ego.and.spouse<-
    lapply(pid,function(p){unlist(opop[p,c("pid","spouse")])})
  
  
  res$kids=  ko(res$ego.and.spouse)
  
  ## gkids
  res$gkids<-ko(res$kids)
  res$ggkids<-ko(res$gkids)
  
  ref.month<-opop[pid,"dob"]+eage
  
  return(
    c(
      at65.lt5.kids=
        mean(unlist(lapply(Vlt5yr(res$kids,opop,ref.month),sum))),
      at65.lt5.gkids=
        mean(unlist(lapply(Vlt5yr(res$gkids,opop,ref.month),sum))),
      
      at65.lt5.ggkids=
        mean(unlist(lapply(Vlt5yr(res$ggkids,opop,ref.month),sum))),
      
      at65.wkr.es=
        mean(unlist(wkr1<-lapply(Vwrkage(res$ego.and.spouse,opop,ref.month),
                                 sum,
                                 na.rm=TRUE))),
      at65.wkr.kids=
        mean(unlist(wkr2<-lapply(Vwrkage(res$kids,opop,ref.month),
                                 sum,
                                 na.rm=TRUE))),
      
      at65.wkr.gkids=
        mean(unlist(wkr3<-lapply(Vwrkage(res$gkids,opop,ref.month),
                                 sum,
                                 na.rm=TRUE))),
      
      at65.wrk.all=
        mean(unlist(mapply(wkr1,wkr2,wkr3,FUN=sum,SIMPLIFY=FALSE))),
      
      at65.nd.rents=
        mean(unlist(d2<-lapply(VnearDeath(res$rents,opop,ref.month, tau),
                               sum,
                               na.rm=TRUE))),
      
      at65.nd.kids=
        mean(unlist(d4<-lapply(VnearDeath(res$kids,opop,ref.month, tau),
                               sum,
                               na.rm=TRUE))),
      
      at65.nd.egosp=
        mean(unlist(d3<-lapply(VnearDeath(res$ego.and.spouse,opop,ref.month, tau),
                               sum,
                               na.rm=TRUE))),
      
      at65.nd.all=
        mean(unlist(mapply(d2,d3,d4,FUN=sum,SIMPLIFY=FALSE)))
      
    ))
}

# ~~ CrunchSim ================
CrunchSim<-function(sfile,Obj=NA, brtYR, kappa_kids = 18, kappa_gkids = 15, tau = 5, include_in_laws, duration_of_sandwich, first_experience_of_death, kinship_network_size){
  
  #~ give this a string pointing to an Rsave file that holds
  #~ simulation results and this will do all the processing that
  #~ must be done it shall be called in a %dopar% loop over SIMS. It
  #~ needs a couple of things from Obj
  
  #~ The output of this monster is a single matrix of where quantities
  #~ of interest relating at a birth cohort are on the rows and each
  #~ col is a year of birth.  The idea is that this each simulation
  #~ will reduce to a matrix like this one which can be combined in
  #~ apply(... mean) sort of way
  
  if(exists("by.Cohort")) rm("by.Cohort")
  
  load(file=sfile)
  
  print(isoc$stem)
  
  # names(sims)
  
  VIVO <- is.list(Obj)
  
  if(VIVO){
    #~ in vivo means an Obj is supplied, for testing we run invitro
    #~ mysteriously this works where attach() does not in %dopar%
    #~ FinalYrSim and endmo are needed
    for(ob in names(Obj)){
      assign(ob,pos=1,value=Obj[[ob]])
    }
  }
  #  opop<-sims$opop
  omar<-sims$omar
  
  
  #~  #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
  #~ Create additional columns of opop for things like Mother's
  #~ mother's age at birth of ego.
  #~  #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
  
  opop <- add2opop(sims$opop,omar)
  
  #~ #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
  #~ Create a list of the kids of each who is a parent  used widely below
  #~ kidsOf[paste(pid)] is the call  BUT much faster is
  #~ KidsOf[pid] Note the case of K
  #~ #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
  kidsOf<-with(opop,{c(
    tapply(pid,mom,c),
    tapply(pid,pop,c)
  )})
  
  #~ MUCH faster if we use numbers instead of names to select
  #~  head(sort(names(kidsOf)))
  #~ yes there are two "0"s :/
  kidsOf["0"]<-NULL;  kidsOf["0"]<-NULL
  KidsOf<-list()
  KidsOf[as.numeric(names(kidsOf))]<-kidsOf
  
  if(duration_of_sandwich){
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~ mnts.alive produces a cohorts worth of information on months of
    #~ coexistance with various sorts of relatives 
    
    temp.mt.alive <- sapply(brtYR, function(x){ 
      mnts.alive.with(opop[opop$cohort==x,"pid"], opop,omar,KidsOf, kappa_kids, kappa_gkids, tau, include_in_laws)
    })
    
    # For debugging~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # pid <- opop[opop$cohort==brtYR[1],"pid"]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    by.Cohort<-temp.mt.alive
    colnames(by.Cohort)<-brtYR 
    
    # gc()
  }
  
  if(first_experience_of_death){
    #~ firstDeath.Coh is a list of vectors of stuff about teh expereince
    #~ of first death.  Each column is a cohort so. It is necessary to
    #~ jigger inelegantly with rownames b/c NAs come down as shorter
    #~ vectors.
    
    firstDeath.Coh<-sapply(brtYR,
                           function(yr){
                             unlist(
                               firstDeath(opop[opop$cohort==yr,"pid"],
                                          opop,
                                          KidsOf)
                             )
                           })    
    
    # It should tack onto by.Cohort without angst
    
    if(!exists("by.Cohort")){
      by.Cohort <- firstDeath.Coh
      colnames(by.Cohort)<-brtYR 
    } else {
      colnames(firstDeath.Coh)<-brtYR 
      by.Cohort <- rbind(by.Cohort, firstDeath.Coh)
    }
    
  }

  if(kinship_network_size){
    #~# #~# #~#~ #~# #~#
    #~ kinship network at age 15
    #~ #~ #~ #~ #~ #~ #~
    temp.at15<-
      sapply(brtYR, function(x){
               at15(pid = opop[opop$cohort==x,"pid"], opop = opop, KidsOf = KidsOf, eage=15*12, tau = tau)
             })
    
    if(!exists("by.Cohort")){
      by.Cohort <- temp.at15
      colnames(by.Cohort)<-brtYR 
    } else {
      colnames(temp.at15)<-brtYR
      
      by.Cohort<-rbind(by.Cohort,
                       temp.at15)
    }
    
    #~#~#~# same for at65
    temp.at65 <-
      sapply(brtYR, function(x){
        at65(opop[opop$cohort==x,"pid"],opop, KidsOf, tau)
      })
    
    colnames(temp.at65)<-brtYR
    
    by.Cohort<-rbind(by.Cohort,
                     temp.at65) 
  }
  
  # Export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # browser()
  sim.name<-gsub(isoc$sup,pattern='.sup',replacement='')
  
  # Remove : as these don't work for filenames in Windows
  sim.name <- gsub(":", "_", sim.name)
  
  simres<-list(sup=isoc$sup,
               seed=isoc$seed,
               sim.name=sim.name,
               #~byYr=cbind(byYr1,sandProp[paste(Years),]),
               by.Cohort=by.Cohort,
               dim.opop=dim(sims$opop))
  
  return(simres)
  
}


# ~~ firstDeath =======

firstDeath<-function(pid,opop,KidsOf){
  ## expects pesonid, opop (post add2opop) and kidsOf returns and
  ## object full of relations and age of ego at the death of various
  ## relations
  ##  pid<-opop[opop$cohort==2015,"pid"]
  ## function for extracting children into convenient list form
  ko<-function(p,KidsOf){
    lapply(p,function(x){unique(as.vector(unlist(KidsOf[x])))})
  }
  
  res<-list(
    ## ego is in g3 
    pid=pid)
  
  ## great grandparents
  res$g0=lapply(pid,
                function(pid){
                  unlist(opop[pid,
                              c("MMM","MMF","MFM","MFF",
                                "FMM","FMF","FFM","FFF")])})
  
  
  
  ## gparents
  res$g1=lapply(pid,
                function(pid){
                  unlist(opop[pid,
                              c("MM","MF","FM","FF")])})
  
  ## parents, uncles aunts
  res$g2=ko(res$g1,KidsOf)
  ##cousins and siblings
  res$g3=ko(res$g2,KidsOf)
  ## children nieces nephews
  res$g4=ko(res$g3,KidsOf)
  
  get.fdeath<-function(gpids,ego.pid,opop){
    ## gets age of ego at first death experience from among the gpids
    ## If ego dies before all gpids this will return Inf and will
    ## warn except that warning is disabled
    gp.dod<-lapply(gpids,function(p){unlist(opop[p,"dod"])})
    ego.dob<-lapply(ego.pid,function(p){unlist(opop[p,"dob"])})
    options(warn=-1)
    rez<-mapply(gp.dod,ego.dob,SIMPLIFY=FALSE,
                FUN=function(dt,eb){x<-dt-eb ; min(x[x>0])})
    options(warn=0)
    return(rez)
  }
  ##Age of ego at first corpse of each generation
  
  ageDeathOf<-cbind(
    as.vector(unlist(get.fdeath(res$g0,pid,opop))),
    as.vector(unlist(get.fdeath(res$g1,pid,opop))),
    as.vector(unlist(get.fdeath(res$g2,pid,opop))),
    as.vector(unlist(get.fdeath(res$g3,pid,opop))),
    as.vector(unlist(get.fdeath(res$g4,pid,opop)))
  )
  colnames(ageDeathOf)<-paste("g",0:4,sep='')
  options(warn=0) ## turn warnings back on -- should notbe needed
  ##min and which.min are cool with Inf
  ageFirstDeath<-
    cbind(
      apply(ageDeathOf,MARGIN=1,min),
      apply(ageDeathOf[,-1],MARGIN=1,min),
      apply(ageDeathOf,MARGIN=1,which.min)-1,
      apply(ageDeathOf[,-1],MARGIN=1,which.min)
    )
  colnames(ageFirstDeath)<-c("g0","g1","gen0","gen1")
  
  ##g0= age at first death INCLUDING GGP
  ##g1=                    EXCLUDING
  ##gen=  generation of first relation to die
  ## counts of gen of first corpse
  countGfC0<-
    table(factor(ageFirstDeath[,"gen0"],level=0:4))
  names(countGfC0)<-
    paste("countGfC0",names(countGfC0),sep=':')
  countGfC1<-
    table(factor(ageFirstDeath[,"gen1"],level=1:4))
  names(countGfC1)<-
    paste("countGfC1",names(countGfC1),sep=':')
  ## proportions GfC
  propGfC0<-
    countGfC0/sum(countGfC0)
  names(propGfC0)<-
    gsub(x=  names(propGfC0),pattern='count',replacement='prop')
  propGfC1<-
    countGfC1/sum(countGfC1)
  names(propGfC1)<-
    gsub(x=  names(propGfC1),pattern='count',replacement='prop')
  ## age at first experience of death AfC
  meanAfCg0<-mean(ageFirstDeath[,"g0"])
  meanAfCg1<-mean(ageFirstDeath[,"g1"])
  ## ageDeathOf[is.infinite(ageDeathOf)]<-NA
  ## meanAfC<-
  ##  apply(ageDeathOf,2,mean,na.rm=TRUE)
  ## names(meanAfC)<-
  ##   paste("meanAfC",names(meanAfC),sep=':')
  
  Rez<-c(
    countGfC0,
    countGfC1,
    propGfC0,
    propGfC1,
    "meanAfCg:0"=  meanAfCg0,
    "meanAfCg:1"=  meanAfCg1
  )
  ##round(Rez,3)
  return(Rez)
  
  
}

# mnts.alive.with ========
mnts.alive.with <- function(pid,opop,omar,KidsOf, kappa_kids = 18, kappa_gkids = 15, tau = 5, include_in_laws = T){
  # browser()
  # ~!~~~~~~~~
  # 20201207 update - also estimate share sandwiched
  # ~!~~~~~~~~
  
  #~ returns a vector of quantities of interest regarding months of
  #~cosurvival it take VECTOR of pid rather than a scalar so a whole
  #~cohort can be run at once
  
  # pid <- opop[opop$cohort==brtYR[1],"pid"]
  
  # ko ('kids of'?) takes a list of unique ids as input and returns a list where each element
  # is an ego and it's contents a vector of ids identifying all the kids of that ego
  ko <- function(p){
    lapply(p,function(x){unique(as.vector(unlist(KidsOf[x])))})
  }
  
  
  #~  children of ego; grandchildren of ego  AND last spouse
  kids.ego <- ko(pid)
  names(kids.ego)<-paste(pid)
  
  # 
  kids.spouse<- ko(opop[pid,"spouse"])
  names(kids.spouse)<-paste(pid)
  
  kids.es <- mapply(kids.ego,kids.spouse,FUN=c)
  kids.es <-lapply(kids.es,unique)
  gkids.es <- ko(kids.es)
  
  sibs <- mapply(ko(opop[pid,"mom"]),
                 ko(opop[pid,"pop"]),
                 FUN=function(x,y){c(x,y)})
  
  g4 <- ko(sibs)
  
  rents<-lapply(pid,function(p){unlist(opop[p,c("mom","pop")])})
  grents<-lapply(pid,function(p){unlist(opop[p,c("MM","MF","FM","FF")])})
  
  #~  months under kappa, where kappa can be either for children or grandchildren
  under_kappa <-
    function(kids, kappa){
      #~ list of vectors of months where in kids are under 5
      unique(as.vector(
        unlist(
          mapply(opop[kids,"dob"],
                 pmin(opop[kids,"dob"] + (kappa*12 - 1),
                      (opop[kids,"dob"] + (kappa*12 - 1)*(opop[kids,"dod"]==0)+
                         opop[kids,"dod"] *  (opop[kids,"dod"]!=0))),
                 FUN=seq)
        )))}
  
  #~ months wherein egos kids are alive and under 5
  #~ just ego's kids but ego+spouses grandkids
  
  kid5.alive <- lapply(kids.ego, under_kappa, kappa_kids)
  gkid5.alive <- lapply(gkids.es, under_kappa, kappa_gkids)
  
  #~ kids cannot be born before ego so only dod matters
  temp.k5 <- mapply(kid5.alive,opop[pid,"dod"],FUN="<=")
  mnt.k5 <- lapply(temp.k5, sum)
  
  #~ gkids could theoretically be born before ego but naaaah.
  temp.gk5 <- mapply(gkid5.alive,opop[pid,"dod"],FUN="<=")
  mnt.gk5 <- lapply(temp.gk5,sum)

  
  if(include_in_laws){
    #~ #~ #~ #~
    #~ months with dying parents+rents of last spouse+spouse
    #~ #~ #~ #~
    #~ line up ego and spouse's parents dates of death
    ego.and.spouse.rents.dod<-
      cbind(
        opop[pid,c("Mdeath","Fdeath")],
        opop[opop$spouse[pid],c("Mdeath","Fdeath")]
      )  
  } else {
    ego.and.spouse.rents.dod <- opop[pid,c("Mdeath","Fdeath")]
  }

  #~ vector of months where parents are near death
  eas.rents.ndeath <- apply(
    ego.and.spouse.rents.dod,
    MARGIN=1,
    FUN=function(x, tau){
      x <- na.omit(x)
      unique(as.vector(mapply(x- (tau*12-1),x,FUN=seq)))
    }, tau )
  
  tempA.Pndeath<-mapply(
    eas.rents.ndeath,
    opop[pid,"dod"],
    FUN="<="
  )
  
  #~ parents can die before ego's birth.. at least in laws can
  tempB.Pndeath <- mapply(
    eas.rents.ndeath,
    opop[pid,"dob"],
    FUN=">="
  )
  
  mnt.Pndeath<-lapply(
    mapply(tempA.Pndeath,tempB.Pndeath,FUN="*"),
    sum
  )
  
  
  #~ sandwich
  mnt.sandwich<-
    lapply(
      mapply(
        mapply(kid5.alive, #~kids 5 and p's dying
               eas.rents.ndeath,
               FUN=function(x,y){x[x %in% y]}),
        mapply(opop[pid,"dob"], #~ ego alive
               opop[pid,"dod"],
               FUN=seq),
        FUN="%in%"),
      sum)
  
  mnt.gsandwich<-
    lapply(
      mapply(
        mapply(gkid5.alive, #~kids 5 and p's dying
               eas.rents.ndeath,
               FUN=function(x,y){x[x %in% y]}),
        mapply(opop[pid,"dob"], #~ ego alive
               opop[pid,"dod"],
               FUN=seq),
        FUN="%in%"),
      sum)
  
  # Share ever sandwiched
  # !!! 20201203 
  # Share of cohort ever sandwiched is the % who 
  # reported at least 1 month in a sandwich state, 
  # estiamted from the lifelines 
  sand_m_vec <- unlist(mnt.sandwich)
  share.sand <- sum(sand_m_vec >= 12) / length(sand_m_vec)
  
  # Share ever grandsandwiched
  gsand_m_vec <- unlist(mnt.gsandwich)
  # hist(gsand_m_vec)
  share.gsand <- sum(gsand_m_vec >= 12) / length(gsand_m_vec)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #~ months alive with grandparents *not*near death
  
  mnt.young.grent <-
    (apply(
      opop[pid,c("FMdeath","MMdeath","MFdeath","FFdeath")]
      , MARGIN=1
      , FUN=max
      # ) - 60) - opop[pid,"dob"]
    ) - tau*12) - opop[pid,"dob"]
  
  mnt.young.ggrent<-
    (apply(opop[pid,c("MMMdeath","MMFdeath", 
                      "MFFdeath","MFMdeath", "FMMdeath","FMFdeath",
                      "FFFdeath", "FFMdeath")],
           MARGIN=1,
           FUN=max)- tau*12)-opop[pid,"dob"]
  
  #~ conditional on having children < 5;how many months of helpful parents?
  #~ how many of sick?
  #~ sandwich
  
  #~ what proportion of months with kids under 5 are spent with
  #~ at least one near death parent. ego's pov
  #~ this excluded people with NO kids
  # browser()
  
  mnt.cond.sandwich<-
    mapply(
      as.data.frame(t(opop[pid,c("Mdeath","Fdeath")])),
      opop[pid,"dod"],
      kid5.alive,
      FUN=function(rdod,dod,k5, kappa_kids){
        temp1<-outer(k5[k5<dod], (rdod- (kappa_kids*12) ),">=")
        temp2<-outer(k5[k5<dod], (rdod),"<=")
        mean(apply((temp1*temp2),1,max))}, kappa_kids)
  
  #~summary(mnt.cond.sandwich)
  #~ proportion of months with kids<5 spent with at least one of
  #~ ego's own parents alive and NOT near death
  
  mnt.cond.happy<-
    mapply(
      as.data.frame(t(opop[pid,c("Mdeath","Fdeath")])),
      opop[pid,"dod"],
      kid5.alive,
      FUN=function(rdod,dod,k5, kappa_kids){
        temp1<-outer(k5[k5<dod], (rdod-(kappa_kids*12)),"<=")
        mean(apply(temp1,1,max))}, kappa_kids)
  
  
  #~ mean number of healthy parents alive  during a month
  #~ wherein eog also is alive and has  kids  < 5
  mnt.Nhrents.k5<-
    mapply(
      as.data.frame(t( ego.and.spouse.rents.dod)),
      kid5.alive,
      opop[pid,'dod'],
      FUN=function(rd,k5,dod, kappa_kids){
        healthy.pmnts<-outer(k5[k5<dod], (rd-(kappa_kids*12)),"<=")
        return(sum(healthy.pmnts)/nrow(healthy.pmnts))
      }, kappa_kids)
  
  #~ mean number of near death parents alive in each month during which
  #~ ego has at least one kid under 5 max posible = 4 produces a value
  #~ for each pid 
  mnt.Nndrents.k5<-
    mapply(
      as.data.frame(t( ego.and.spouse.rents.dod)),
      kid5.alive,
      opop[pid,'dod'],
      FUN=function(rd,k5,dod, kappa_kids){
        temp1<-outer(k5[k5<dod], (rd-(kappa_kids*12)),">=") #~alive past onset
        temp2<-outer(k5[k5<dod], (rd),"<=") #~ but before death
        healthy.pmnts<-temp1&temp2
        return(sum(healthy.pmnts)/nrow(healthy.pmnts))
        
      }, kappa_kids)
  #~summary(mnt.Nndrents.k5)    
  
  #~ Average number of 20-60 year olds in family during ego's age 12-18
  ego.12.18<-mapply(opop[pid,'dob'],
                    opop[pid,'dod'],
                    FUN=function(b,d){
                      temp1<-seq(b+(12*12),b+(18*12))
                      return(temp1[temp1<d])})
  
  grents.rents.sibs<-
    mapply(sibs,
           g4,
           rents,
           g4,
           FUN=function(a,b,c,d){c(a,b,c,d)}
    )
  
  mnt.nwrk.1218<-
    mapply(
      ego.12.18,
      rb<-lapply(grents.rents.sibs,
                 function(p){opop[p,"dob"]}),
      rd<-lapply(grents.rents.sibs,
                 function(p){opop[p,"dod"]}),
      FUN=function(e1218,relB,relD){
        t1<-outer(e1218,relB+(20*12), ">=")
        t2<-outer(e1218,relB+(60*12), "<=")
        t3<-outer(e1218,relD-(5*12), "<=")
        return(sum(t1*t2*t3)/(length(e1218)))
      }
    )           
  
  #~ averge number of relatives near death during ego's12-18 yrs.
  mnt.nndeath.1218<-
    mapply(
      ego.12.18,
      rb<-lapply(grents.rents.sibs,
                 function(p){opop[p,"dob"]}),
      rd<-lapply(grents.rents.sibs,
                 function(p){opop[p,"dod"]}),
      FUN=function(e1218,relB,relD){
        t1<-outer(e1218,relD-(5*12), ">=")
        t2<-outer(e1218,relD, "<=")
        return(sum(t1*t2)/(length(e1218)))
      }
    )           
  #~summary(mnt.nndeath.1218)
  #~ average number of relations under 5 while ego is 12-18
  
  mnt.nk5.1218<-
    mapply(
      ego.12.18,
      rb<-lapply(grents.rents.sibs,
                 function(p){opop[p,"dob"]}),
      rd<-lapply(grents.rents.sibs,
                 function(p){opop[p,"dod"]}),
      FUN=function(e1218,relB,relD){
        t1<-outer(e1218,relB+(5*12), "<=")
        t2<-outer(e1218,relB, ">=")
        return(sum(t1*t2)/(length(e1218)))
      }
    )           
  #~summary(mnt.nk5.1218)
  
  return(
    c(
      #~mnths ego alive with kid(s) under kappa
      mnt.k_kappa = mean(unlist(mnt.k5)),
      mnt.gk_kappa=mean(unlist(mnt.gk5)),
      #~mnths ego alive with parents near death
      mnt.Pndeath=mean(unlist(mnt.Pndeath)),
      #~mnts ego alive and sandwiched
      mnt.sandwich=mean(unlist(mnt.sandwich)),
      mnt.gsandwich=mean(unlist(mnt.gsandwich)),
      mnt.everborn=mean(sapply(kids.ego,length)),
      mnt.geverborn=mean(sapply(gkids.es,length)),
      mnt.young.grent=mean(mnt.young.grent[mnt.young.grent>0]),
      mnt.young.ggrent=mean(mnt.young.ggrent[mnt.young.ggrent>0]),
      mnt.cond.sandwich=mean(mnt.cond.sandwich,na.rm=TRUE),
      mnt.cond.happy=mean(mnt.cond.happy,na.rm=TRUE),
      mnt.Nhrents.k5=mean(mnt.Nhrents.k5,na.rm=TRUE),
      mnt.Nndrents.k5=mean(mnt.Nndrents.k5,na.rm=TRUE),
      mnt.nwrk.1218=mean(mnt.nwrk.1218,na.rm=TRUE),
      mnt.nndeath.1218=mean(mnt.nndeath.1218,na.rm=TRUE),
      mnt.nk5.1218=mean( mnt.nk5.1218,na.rm=TRUE)
      # added 20201207
      , share.sand = share.sand
      , share.gsand = share.gsand
    )
  )
}

ReadSocOut<-function(stem){
  ## stem is path to directory + what comes before .opop
  ## Will read .opop and .omar .opox ,otx and assign useful names
  
  
  
  print(paste("Looking in ",stem))
  print("reading opop...")
  
  opop<-try(read.table(file=paste(stem,".opop",sep='')),silent=TRUE)
  if(class(opop) == "try-error"){
    warning( paste( paste(stem,".opop",sep=''), "is empty or NOT FOUND",sep=' '))
    warning("Nothing good can happen without an opop file... exiting")
    return()
  }else{
    names(opop)<-c("pid","fem","group",
                   "nev","dob","mom","pop","nesibm","nesibp",
                   "lborn","marid","mstat","dod","fmult")
    rownames(opop)<-opop$pid
    print(paste("read ",nrow(opop), " records into opop"))
  }
  ####### opox
  print("reading opox")
  opox<-try(read.table(file=paste(stem,".opox",sep=''),sep=""),silent=TRUE)
  if(class(opox)=="try-error"){
    print("opox empty or missing skipped")
  }else{
    rownames(opox)<-opop$pid
  }
  
  ##### omar
  print("reading omar")
  omar<-try(read.table(file=paste(stem,".omar",sep=''),sep=""),
            silent=TRUE)
  
  if(class(omar)=="try-error"){
    print("skipping empty .omar file")
  }else{
    names(omar)<-c("mid","wpid","hpid","dstart","dend",
                   "rend","wprior","hprior")
    rownames(omar)<-omar$mid
  }
  
  
  
  ##### Transition history
  
  
  
  print("reading otx (transition history")
  otx<-try(read.table(file=paste(stem,".otx",sep=''),sep=""),
           silent=TRUE)
  if(class(otx)=="try-error"){
    print("skipping empty .otx  (transition history) file")
  }else{
    names(otx)<-c("pid","dot","fromg","tog","pos")
    
    rownames(otx)<-paste(otx$pid,otx$pos,sep='')
  }
  
  
  return(foo<-list(opop=opop,
                   omar=omar,
                   opox=opox,
                   otx=otx
  ))
}

Vlt5yr<-function(gpid,opop,ref=ref.month){
  ## undead and under 5 should count
  dob<-lapply(gpid,function(x){unlist(opop[x,"dob"])})
  dod<-lapply(gpid,function(x){unlist(opop[x,"dod"])})
  
  
  res<-mapply(dod,ref,dob,SIMPLIFY=FALSE,
              FUN=function(d,r,b){
                (b<=r) &
                  ((d>=r)|(d==0))&
                  ((b+60)>=r)})
  return(res)
  
  ##  (opop[gpid,"dob"] <= ref) &
  ##  ((opop[gpid,"dod"] >= ref)|(opop[gpid,"dod"]==0))  &
  ##  ((opop[gpid,"dob"] +60) >= ref)
  
}

VnearDeath <- function(gpid,opop,ref=ref.month, ...){
  ## dod==0 ?undead should not be counted as near dead
  ## as long as ref is more than 5 years from endmo
  ## (opop[gpid,"dob"] <= ref) &
  ## (opop[gpid,"dod"] >= ref)
  ## (opop[gpid,"dod"]-60) <= ref

  # FIX!! 20200615 Diego:
  # This is bc I got furstrated that at15 wouldn't wotk with a variable value 
  # for tau. So, I still need to figure the VnearDeath(res$rents.....) chunk to make it work with 
  # a tau variable
  if(!exists("tau")) tau <- 5
  
  dob<-lapply(gpid,function(x){unlist(opop[x,"dob"])})
  dod<-lapply(gpid,function(x){unlist(opop[x,"dod"])})
  result<-mapply(dob,ref,dod,SIMPLIFY=FALSE,
                 FUN=function(b,r,d){
                   (b < r) &
                     (d >= r) &
                     ((d- (tau*12)) <= r)})
  
  return(result)
}

Vwrkage<-function(gpid,opop,ref=ref.month){
  ## undead over 20 and less than 60 should count
  dob<-lapply(gpid,function(x){unlist(opop[x,"dob"])})
  dod<-lapply(gpid,function(x){unlist(opop[x,"dod"])})
  
  res<-mapply(dod,ref,dob,SIMPLIFY=FALSE,
              FUN=function(d,r,b){
                (b+(20*12) <= r) &
                  ((d >=r)|(d==0)) &
                  ((b+(60*12)) >= r)})
  return(res)
  
  ##((opop[gpid,"dob"] +(20*12)) <= ref) &
  ##  ((opop[gpid,"dod"] >= ref)|(opop[gpid,"dod"]==0)) &
  ##  ((opop[gpid,"dob"] +(60*12)) >= ref)
  
}

zna <- function(x){return(ifelse(x==0,NA,x))}

# z2na<-function(x){x[x==0]<-NA;return(x)}
