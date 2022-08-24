options(repos = c(CRAN="https://cloud.r-project.org"))

detachAllPackages <- function(keep = NULL, keep.basic = TRUE, unload = FALSE,
                              force = FALSE) {
  # function for detaching all attached packages (except basic ones)
  basic.packages <- c("package:stats","package:graphics","package:grDevices",
                      "package:utils","package:datasets","package:methods",
                      "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1,
                                  TRUE, FALSE)]
  if (!is.null(keep)){
    package.list <- setdiff(package.list, paste("package", keep, sep = ":"))
  }
  if (keep.basic){
    package.list <- setdiff(package.list, basic.packages)
  }
  if (length(package.list) > 0) {
    for (package in package.list) detach(package, character.only = TRUE, unload = unload)
  }
}
detachAllPackages(keep = NULL, unload = FALSE, force = T)

invisible(lapply(paste0("package:", names(utils::sessionInfo()$otherPkgs)),   # Unload add-on packages
                 detach,
                 character.only = TRUE, unload = TRUE))

pacman::p_install(boot,knitr,tidyr,stringi,stringr,ggplot2,Hmisc,kableExtra,plotly,janitor,
                  rbokeh,zoo,broom,sqldf,devtools,codebook,data.table,panelr,RColorBrewer,lsmeans,
                  finalfit,ggiraph,sf,treemapify,dplyr,tidyverse,epiR,survminer,ggfortify,survMisc,
                  foreign,Hmisc,gridExtra,reshape2,stargazer,tableone,MatchIt,cobalt,eha,igraph,Amelia,
                  DiagrammeR ,mstate,flexsurv,muhaz,Metrics,Rfast,grid,gridExtra)

devtools::install_github("hputter/mstate")

if(!grepl("4.0.2",R.version.string)){stop("Different version (must be 4.0.2)")}
path<-rstudioapi::getSourceEditorContext()$path

if (grepl("CISS Fondecyt",path)==T){
  setwd("C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2019 (github)/SUD_CL/");load("C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2019 (github)/mult_state_4_apr22.RData")
} else if (grepl("andre",path)==T){
  setwd('C:/Users/andre/Desktop/SUD_CL/');load("E:/Mi unidad/Alvacast/SISTRAT 2019 (github)/mult_state_4_apr22.RData")
} else if (grepl("E:",path)==T){
  setwd("E:/Mi unidad/Alvacast/SISTRAT 2019 (github)/SUD_CL/");load("E:/Mi unidad/Alvacast/SISTRAT 2019 (github)/mult_state_4_apr22.RData")
} else {
  setwd("G:/Mi unidad/Alvacast/SISTRAT 2019 (github)/SUD_CL/");load("G:/Mi unidad/Alvacast/SISTRAT 2019 (github)/mult_state_4_apr22.RData")
}

lmaj_res_1_1095<-
  LMAJ(pr1_sel, #An "msdata" object, as for instance prepared by link{msprep}
       from=1, ##Either a single state or a set of states in the state space 1,...,S
       s=365*3,  #The prediction time point s from which transition probabilities are to be obtained
       method ="aalen" #The method for calculating variances, as in probtrans
  )
lmaj_res_2_1095<-
  LMAJ(pr1_sel, #An "msdata" object, as for instance prepared by link{msprep}
       from=2, ##Either a single state or a set of states in the state space 1,...,S
       s=365*3,  #The prediction time point s from which transition probabilities are to be obtained
       method ="aalen" #The method for calculating variances, as in probtrans
  )
lmaj_res_3_1095<-
  LMAJ(pr1_sel, #An "msdata" object, as for instance prepared by link{msprep}
       from=3, ##Either a single state or a set of states in the state space 1,...,S
       s=365*3,  #The prediction time point s from which transition probabilities are to be obtained
       method ="aalen" #The method for calculating variances, as in probtrans
  )
lmaj_res_4_1095<-
  mstate::LMAJ(pr1_sel, #An "msdata" object, as for instance prepared by link{msprep}
       from=4, ##Either a single state or a set of states in the state space 1,...,S
       s=90,  #The prediction time point s from which transition probabilities are to be obtained
       method ="aalen" #The method for calculating variances, as in probtrans
  )


<div style="border: 1px solid #ddd; padding: 5px; overflow-y: scroll; height:650px; overflow-x: scroll; width:100%">
  ```{r clus_mstate,eval=T, echo=T, paged.print=TRUE, error=T}
## Function that calculates the working independence Aalen-Johansen estimator 
## of the population-averaged transition probabilities. Standard errors and
## and 95% confidence intervals and bands are also calculated.
## Auxiliary function to calculate the landmark version
## of the working independence Aalen-Johansen estimator.
## This is a modification of the LMAJ function in the 
## R package mstate.
invisible("NO se puede cambiar por el actualizado de mstate, porque tienen distintos argumentos y me voy a demorar mucho en rehacerlo")
LMAJ2 <- function (msdata, tmat, id, s, h, j, weighted){
  if (is.null(tmat)) 
    stop("msdata object should have a \"trans\" attribute")
  K <- nrow(tmat)
  if (any(is.na(match(h, 1:K)))) 
    stop("h should be subset of 1:K with K number of states")
  attr(msdata, "trans") <- tmat
  xss <- xsect(msdata, s)
  infrom <- xss[xss$state %in% h,id]
  msdatas <- cutLMms(msdata, LM = s)
  msdatasfrom <- msdatas[msdatas[,id] %in% infrom, ]
  c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), 
              data = msdatasfrom)
  A0 <- msfit(c0, trans = tmat, variance=FALSE)
  if(weighted==TRUE){
    c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), 
                weights=(1/clust.size), data = msdatasfrom)
    A.wt <- basehaz(c0, centered=FALSE)
    A.wt$strata <- as.numeric(A.wt$strata)
    for(trn in sort(unique(A.wt$strata))){
      fun <- stepfun(A.wt[A.wt$strata==trn,"time"], 
                     c(0,A.wt[A.wt$strata==trn,"hazard"]))
      A0$Haz[A0$Haz$trans==trn,"Haz"] <- fun(A0$Haz[A0$Haz$trans==trn,"time"])
    }
  }
  pt0 <- probtrans(A0, predt = s, variance=FALSE)[[h]][,c("time",
                                                          paste("pstate", j, sep=""))]
  return(pt0)
}

patp <- function(data, tmat, cid, id, h, j, s=0, weighted=FALSE,
                 LMAJ=FALSE, B=100, cband=FALSE){
  check.ic <- aggregate(data[,cid], by=list(data[,id]), 
                        FUN=sd, na.rm=TRUE)$x
  check.ic <- check.ic[!is.na(check.ic)]
  if(length(check.ic)>0){
    if(max(check.ic)>0){
      stop("Same unit(s) in more than 1 cluster (violation of the independent clusters assumption)")
    }
  }
  
  if(B<=0 & cband==TRUE){
    stop("Condidence bands cannot be caclulated based on <=0 bootstrap samples")
  } else if (B<1000 & cband==TRUE){
    warning("It is recommended to use at least 1000 bootstrap samples for confidence band calculation")
  }
  if(LMAJ==FALSE){
    c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), data=data,
                method = "breslow")
    
    A0 <- msfit(object = c0, trans = tmat, variance=FALSE)
    if(weighted==TRUE){
      ## msfit does not currently support weights and thus
      ## the weighted by cluster size cumulative transition
      ## intensities need to be manually inserted into A0
      M0 <- aggregate(rep(1,times=nrow(data)),
                      by = list( data[,cid], data[,id]),
                      FUN = mean)
      
      M <- aggregate(M0$x,
                     by = list( M0$Group.1),
                     FUN = sum)
      colnames(M) <- c(cid, "clust.size")
      data <- merge(data, M, by=cid)
      data <- data[order(data[,cid],data[,id]),]
      class(data) <- c("msdata", "data.frame")
      
      c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), 
                  weights=(1/clust.size), data=data,
                  method = "breslow")
      A.wt <- basehaz(c0, centered=FALSE)
      A.wt$strata <- as.numeric(A.wt$strata)
      for(trn in sort(unique(A.wt$strata))){
        fun <- stepfun(A.wt[A.wt$strata==trn,"time"], 
                       c(0,A.wt[A.wt$strata==trn,"hazard"]))
        A0$Haz[A0$Haz$trans==trn,"Haz"] <- fun(A0$Haz[A0$Haz$trans==trn,"time"])
      }
    }
    P0 <- probtrans(A0, predt = s, 
                    variance=FALSE)[[h]][,c("time",
                                            paste("pstate", j, sep=""))]
  } else {
    if(weighted==TRUE){
      M0 <- aggregate(rep(1,times=nrow(data)),
                      by = list( data[,cid], data[,id]),
                      FUN = mean)
      
      M <- aggregate(M0$x,
                     by = list( M0$Group.1),
                     FUN = sum)
      colnames(M) <- c(cid, "clust.size")
      data <- merge(data, M, by=cid)
      data <- data[order(data[,cid],data[,id]),]
      class(data) <- c("msdata", "data.frame")
    }
    P0 <- LMAJ2(msdata=data, tmat=tmat, id=id, s=s, h=h, j=j, weighted=weighted)
  }
  
  colnames(P0) <- c("time", paste("P", h, j, sep=""))
  if(B==0){
    return(P0)
  } else {
    n <- length(unique(data[,cid]))
    boot <- msboot(patp_b, data=data, 
                   id=cid, B=B, verbose=0,
                   tmat=tmat, id2=id, h=h, j=j, s=s, times=P0$time,
                   wiaj_hat=P0[,paste("P", h, j, sep="")],
                   n=n, weighted=weighted, LMAJ=LMAJ)
    sigma <- apply(boot, 1, sd)
    se <- sigma/sqrt(n)
    
    ## cloglog transformation
    ll <- exp(-exp(log(-log(P0[,paste("P", h, j, sep="")]))-qnorm(0.975)*se/
                     (P0[,paste("P", h, j, sep="")]*
                        log(P0[,paste("P", h, j, sep="")]))))
    ul <- exp(-exp(log(-log(P0[,paste("P", h, j, sep="")]))+qnorm(0.975)*se/
                     (P0[,paste("P", h, j, sep="")]*
                        log(P0[,paste("P", h, j, sep="")]))))
    
    res <- cbind(P0, se, ll, ul)
    
    if(cband==TRUE){
      q_t <- 1/(1+sigma^2)
      
      jump.times <- P0$time[diff(c(P0[1,paste("P", h, j, sep="")],
                                   P0[,paste("P", h, j, sep="")]), lag=1)!=0]
      quant <- quantile(jump.times, probs=c(.05,.95))
      range <- (P0$time>=quant[1] & P0$time<=quant[2] & 
                  P0[,paste("P", h, j, sep="")]>0)
      
      B_t <- q_t[range]*boot[range,]/(log(P0[range, paste("P", h, j, sep="")])*
                                        P0[range, paste("P", h, j, sep="")])
      B_t <- abs(B_t)
      c_a <- apply(B_t, 2, max)
      c_a <- quantile(c_a, probs=0.95)
      
      ## cloglog transformation
      ll.band <- exp(-exp(log(-log(P0[,paste("P", h, j, sep="")]))+c_a/(sqrt(n)*q_t)))
      ll.band[!range] <- NA
      ul.band <- exp(-exp(log(-log(P0[,paste("P", h, j, sep="")]))-c_a/(sqrt(n)*q_t)))
      ul.band[!range] <- NA
      
      res <- cbind(res, ll.band, ul.band)
    }
    return(res)
  }
}

patp_b <- function(data, tmat, id2, h, j, s, times, wiaj_hat, n, weighted, LMAJ){
  if(LMAJ==FALSE){
    c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), data=data,
                method = "breslow")
    
    A0 <- msfit(object = c0, trans = tmat, variance=FALSE)
    if(weighted==TRUE){
      c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), 
                  weights=(1/clust.size), data=data,
                  method = "breslow")
      A.wt <- basehaz(c0, centered=FALSE)
      A.wt$strata <- as.numeric(A.wt$strata)
      for(trn in sort(unique(A.wt$strata))){
        fun <- stepfun(A.wt[A.wt$strata==trn,"time"], 
                       c(0,A.wt[A.wt$strata==trn,"hazard"]))
        A0$Haz[A0$Haz$trans==trn,"Haz"] <- fun(A0$Haz[A0$Haz$trans==trn,"time"])
      }
    }
    
    P0 <- probtrans(A0, predt = s, 
                    variance=FALSE)[[h]][,c("time",
                                            paste("pstate", j, sep=""))]
  } else {
    P0 <- LMAJ2(msdata=data, tmat=tmat, id=id2, s=s, h=h, j=j, weighted=weighted)
  }
  
  P0_t <-stepfun(P0$time, c(P0[1,paste("pstate", j, sep="")], 
                            P0[,paste("pstate", j, sep="")]))                
  
  return(sqrt(n)*(P0_t(times)-wiaj_hat))
}

patp_test <- function(data, tmat, cid, id, group, h, j, s=0,
                      weighted=FALSE, LMAJ=FALSE, B=1000){
  check.ic <- aggregate(data[,cid], by=list(data[,id]), 
                        FUN=sd, na.rm=TRUE)$x
  check.ic <- check.ic[!is.na(check.ic)]
  if(length(check.ic)>0){
    if(max(check.ic)>0){
      stop("Same unit(s) in more than 1 cluster (violation of the independent clusters assumption)")
    }
  }
  
  if(B<=0){
    stop("Tests cannot be performed based on <=0 bootstrap samples")
  } else if (B<1000){
    warning("It is recommended to use at least 1000 bootstrap samples for two-sample testing")
  }
  n <- length(unique(data[,cid]))
  groups <- unique(data[,group])
  groups <- sort(groups[!is.na(groups)])
  if(length(groups)!=2){
    stop("Number of groups != 2")
  }
  
  if(weighted==TRUE){
    M0 <- aggregate(rep(1,times=nrow(data)),
                    by = list(data[,cid],
                              data[,group],
                              data[,id]),
                    FUN = mean)
    
    M <- aggregate(M0$x,
                   by = list(M0$Group.1, M0$Group.2),
                   FUN = sum)
    colnames(M) <- c(cid, group, "clust.size")
    data <- merge(data, M, by=c(cid,group))
    data <- data[order(data[,cid],data[,id]),]
    class(data) <- c("msdata", "data.frame")
  }
  
  Pt <- list()
  tms <- list()
  for(g in groups){
    if(LMAJ==FALSE){
      c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans),
                  data=data[data[,group]==g,], method = "breslow")
      
      A0 <- msfit(object = c0, trans = tmat, variance=FALSE)
      if(weighted==TRUE){
        c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), 
                    weights=(1/clust.size), data=data[data[,group]==g,], 
                    method = "breslow")
        A.wt <- basehaz(c0, centered=FALSE)
        A.wt$strata <- as.numeric(A.wt$strata)
        for(trn in sort(unique(A.wt$strata))){
          fun <- stepfun(A.wt[A.wt$strata==trn,"time"], 
                         c(0,A.wt[A.wt$strata==trn,"hazard"]))
          A0$Haz[A0$Haz$trans==trn,"Haz"] <- fun(A0$Haz[A0$Haz$trans==trn,"time"])
        }
      }
      
      P0 <- probtrans(A0, predt = s, variance=FALSE)[[h]][,c("time",
                                                             paste("pstate", j, sep=""))]
    } else {
      P0 <- LMAJ2(msdata=data[data[,group]==g,], tmat=tmat, 
                  id=id, s=s, h=h, j=j, weighted=weighted)
    }
    if(length(Pt)==0){
      Pt[[1]] <- stepfun(P0$time, c(P0[1,paste("pstate", j, sep="")], 
                                    P0[,paste("pstate", j, sep="")]))
      tms[[1]] <- P0$time[diff(c(P0[1,paste("pstate", j, sep="")],
                                 P0[,paste("pstate", j, sep="")]), lag=1)!=0]
    } else {
      Pt[[2]] <- stepfun(P0$time, c(P0[1,paste("pstate", j, sep="")], 
                                    P0[,paste("pstate", j, sep="")]))
      tms[[2]] <- P0$time[diff(c(P0[1,paste("pstate", j, sep="")],
                                 P0[,paste("pstate", j, sep="")]), lag=1)!=0]
    }
  }
  
  tms <- sort(unique(c(tms[[1]], tms[[2]])))
  
  if(nrow(data[data$from==j,])>0){
    tS <- sort(unique(c(data[data$to==j,"from"],j)))
  } else {
    tS <- sort(unique(data[data$to==j,"from"]))
  }
  
  EY <- NULL
  EY.t <- function(t,dt){
    sum(dt$Tstart<t & dt$Tstop>=t)/n
  }
  for(i in tS){
    for(g in groups){
      dat <- unique(data[data$from==i & data[,group]==g,
                         c(id, "from", "Tstart", "Tstop")])
      if(is.null(EY)){
        EY <- sapply(tms, EY.t, dt=dat)
      } else {
        EY <- cbind(EY, sapply(tms, EY.t, dt=dat))
      }
    }
  }
  Wt <- rowProds(EY)/rowSums(EY)
  tms <- tms[!is.na(Wt)]
  if(length(tms)==0 | max(Wt, na.rm=TRUE)==0){
    stop("Weights NA or 0 for all timepoints")
  }
  Wt <- Wt[!is.na(Wt)]
  D_hat=(Pt[[1]](tms) - Pt[[2]](tms))
  
  Diff_boot <- msboot(patp_test_b, data=data, 
                      id=cid, B=B, verbose=0,
                      tmat=tmat, id2=id, group=group, h=h, j=j, s=s, 
                      times=tms, D_hat=D_hat, Wt=Wt,
                      n=n, weighted=weighted, LMAJ=LMAJ)
  #2022
  return_patp_test <- list()
  return_patp_test$diff_boot <-Diff_boot
  
  KS <- max(abs(sqrt(n)*Wt*D_hat))
  return_patp_test$KS<-KS
  KS.b <- apply(abs(Diff_boot),2,max)
  return_patp_test$KS_b<-KS.b
  pval <- mean(KS.b >= KS)
  names(pval) <- "p-value"
  return_patp_test$pval<-pval
  return(return_patp_test)
}
patp_test_b <- function(data, tmat, id2, h, j, s, group, times, D_hat, 
                        Wt, n, weighted, LMAJ){
  
  groups <- unique(data[,group])
  groups <- sort(groups[!is.na(groups)])
  
  Pt <- list()
  for(g in groups){
    if(LMAJ==FALSE){
      c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), 
                  data=data[data[,group]==g,], method = "breslow")
      
      A0 <- msfit(object = c0, trans = tmat, variance=FALSE)
      if(weighted==TRUE){
        c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), 
                    weights=(1/clust.size), data=data[data[,group]==g,],
                    method = "breslow")
        A.wt <- basehaz(c0, centered=FALSE)
        A.wt$strata <- as.numeric(A.wt$strata)
        for(trn in sort(unique(A.wt$strata))){
          fun <- stepfun(A.wt[A.wt$strata==trn,"time"], 
                         c(0,A.wt[A.wt$strata==trn,"hazard"]))
          A0$Haz[A0$Haz$trans==trn,"Haz"] <- fun(A0$Haz[A0$Haz$trans==trn,"time"])
        }
      }
      P0 <- probtrans(A0, predt = s, variance=FALSE)[[h]][,c("time",
                                                             paste("pstate", j, sep=""))]
    } else {
      P0 <- LMAJ2(msdata=data[data[,group]==g,], tmat=tmat, 
                  id=id2, s=s, h=h, j=j, weighted=weighted)
    }
    if(length(Pt)==0){
      Pt[[1]] <- stepfun(P0$time, c(P0[1,paste("pstate", j, sep="")], 
                                    P0[,paste("pstate", j, sep="")]))
    } else {
      Pt[[2]] <- stepfun(P0$time, c(P0[1,paste("pstate", j, sep="")], 
                                    P0[,paste("pstate", j, sep="")]))
    }
  }          
  D_boot <- Pt[[1]](times) - Pt[[2]](times)
  return(sqrt(n)*Wt*(D_boot-D_hat))
}

#data: a data.frame in the long format required by the mstate package.
#tmat: a matrix of possible transitions between states of the process where different transitions are identified by a different integer. If a direct transition between two states is not possible it is indicated as NA. This matrix can be obtained via the mstate function transMat().
#cid: variable name that identifies the clusters.
#id: variable name that identifies the individual observations.
#h: the state h in Pr(X(t) = j| X(s) = h).
#j: the state j in Pr(X(t) = j| X(s) = h).
#s: the time s in Pr(X(t) = j| X(s) = h). The default value is 0.
#weighted: logical value. If TRUE, the estimator is weighted by the inverse of the cluster sizes. This is useful when cluster size is random and expected to be informative. The default value is FALSE.
#LMAJ: logical value. If TRUE, the landmark version of the estimator is returned. This is useful when s>0 and the Markov assumption is not plausible. The default value is FALSE.
#B: number of nonparametric cluster bootstrap replications. If B=0, no standard errors or confidence intervals/bands are returned. The default value is 100.
#cband: logical value. If TRUE, the limits of the 95% simultaneous confidence band are returned. The default value is FALSE.

#https://github.com/gbakoyannis/clustered-multistate/tree/master/R

ms_d_match_surv$cid<-
  as.numeric(unlist(ms_d_match_surv %>% #119356
                      dplyr::left_join(d_match_surv_msprep[,c("id","group_match")], by="id") %>% select(group_match)))#119356

set.seed(2125)
P12_365 <- patp(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", 
                h=1, j=2, s=365, B=1000, LMAJ=T, cband=T)
invisible(c("Entrega intervalos"))
set.seed(2125)
P23_365 <- patp(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", 
                h=2, j=3, s=365, B=1000, LMAJ=T, cband=T)
invisible(c("No entrega intervalos"))
set.seed(2125) 
P34_365 <- patp(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", 
                h=3, j=4, s=365, B=1000, LMAJ=T, cband=T)
invisible(c("Entrega sólo ceros"))
set.seed(2125)
P45_365 <- patp(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", 
                h=4, j=5, s=365, B=1000, LMAJ=T, cband=T)
invisible(c("Entrega error"))

set.seed(2125)
P12_1096 <- patp(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", 
                 h=1, j=2, s=(365*3)+1, B=1000, LMAJ=T, cband=T)
invisible(c("Entrega intervalos"))
set.seed(2125)
P23_1096 <- patp(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", 
                 h=2, j=3, s=(365*3)+1, B=1000, LMAJ=T, cband=T)
invisible(c("No entrega intervalos"))
set.seed(2125) 
P34_1096 <- patp(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", 
                 h=3, j=4, s=(365*3)+1, B=1000, LMAJ=T, cband=T)
invisible(c("Entrega sólo ceros"))
set.seed(2125)
P45_1096 <- patp(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", 
                 h=4, j=5, s=(365*3)+1, B=1000, LMAJ=T, cband=T)
invisible(c("Entrega error"))
```
</div>
  
<div style="border: 1px solid #ddd; padding: 5px; overflow-y: scroll; height:350px; overflow-x: scroll; width:100%">
```{r clus_mstate_group,eval=T, echo=T, paged.print=TRUE, error=T}
ms_d_match_surv_monthly<-
ms_d_match_surv%>% mutate(Tstart = round(Tstart/30.417, digit=0),Tstop = round(Tstop/30.417, digit=0))


library(matrixStats);library(mstate)

patp_test_month<- list()

mat_lmaj<-
  #original
  #data.frame(times=rep(c(3, 12, 12*3, 12*5),4), trans_from=rep(1:4,each=4))
  #enfasis en transiciones posteriores
  data.frame(times=rep(seq(0.1, 5*12, by=.1),2), trans_from=rep(3:4,each=600))
for (v in 1:nrow(mat_lmaj)){
    patp_test_month[[v]]<-tryCatch(patp_test(data=ms_d_match_surv_monthly, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
            h=mat_lmaj$trans_from[v], j=mat_lmaj$trans_from[v]+1, s=mat_lmaj$times[v], B=1, LMAJ=T),
            error = function(e) {message(paste("Error: ", e));return(NA)}
              )
    #return(paste0("State from: ",mat_lmaj$trans_from[v], "Months= ",mat_lmaj$times[v]))
}
#TIRA ERROR PORQUE NO ENCUENTRA OBSERVACIONES EN NINGUN PERIODO PARA LAS TRANSICIONES 3 Y 4
#LO CORRI CON SÓLO MESES SIN DIGITO DECIMAL Y TAMPOCO
tibble::as_tibble(patp_test_month, .name_repair = "unique") %>% t() %>% View()

P12_clus_0 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                        h=1, j=2, s=0, B=1000, LMAJ=T)
# 0 
P23_clus_0 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                        h=2, j=3, s=0, B=1000, LMAJ=T)
#Error
P34_clus_0 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                        h=3, j=4, s=0, B=1000, LMAJ=T)
#Error
P45_clus_0 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                        h=4, j=5, s=0, B=1000, LMAJ=T)
#Error

P12_clus_90 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                         h=1, j=2, s=90, B=1000, LMAJ=T)
#  0  
P23_clus_90 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                         h=2, j=3, s=90, B=1000, LMAJ=T)
#.816
P34_clus_90 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                         h=3, j=4, s=90, B=1000, LMAJ=T)
#Error
P45_clus_90 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                         h=4, j=5, s=90, B=1000, LMAJ=T)
#Error

P12_clus_365 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                          h=1, j=2, s=365, B=1000, LMAJ=T)
#  0 
P23_clus_365 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                          h=2, j=3, s=365, B=1000, LMAJ=T)
# 0.75
P34_clus_365 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                          h=3, j=4, s=365, B=1000, LMAJ=T)
#Error
P45_clus_365 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                          h=4, j=5, s=365, B=1000, LMAJ=T)
#Error

P12_clus_1095 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                           h=1, j=2, s=(365*3)+1, B=1000, LMAJ=T)
#0.001 
P23_clus_1095 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                           h=2, j=3, s=(365*3)+1, B=1000, LMAJ=T)
#0.009 
P34_clus_1095 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                           h=3, j=4, s=(365*3)+1, B=1000, LMAJ=T)
#Error
P45_clus_1095 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                           h=4, j=5, s=(365*3)+1, B=1000, LMAJ=T)
#Error


P12_clus_1826 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                           h=1, j=2, s=(365*5)+1, B=1000, LMAJ=T)
#0.111 
P23_clus_1826 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                           h=2, j=3, s=(365*5)+1, B=1000, LMAJ=T)
#0.12
P34_clus_1826 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                           h=3, j=4, s=(365*5)+1, B=1000, LMAJ=T)
#Error in rowprods(EY) : 
#Not compatible with requested type: [type=list; target=double].
P45_clus_1826 <- patp_test(data=ms_d_match_surv, tmat=trans_matrix, cid="cid", id="id", group="tipo_de_plan_res_1",
                           h=4, j=5, s=(365*5)+1, B=1000, LMAJ=T)
#Error in data.frame(time = sf0$time, Haz = -log(sf0$surv), norisk = norisk,  : 
#arguments imply differing number of rows: 15, 0
```
</div>
  
  ```{r lmaj_clus_plot, eval=T, echo=T, paged.print=TRUE, fig.height=10, fig.width=10, fig.cap="Figure 7a. Estimate of State Occupancies at Baseline, Five-states model", fig.align="center", error=T}

bind_rows(cbind.data.frame(P12,at=0,t=12),
          cbind.data.frame(P12_90,at=90,t=12),
          cbind.data.frame(P12_365,at=365,t=12),
          cbind.data.frame(P12_1096,at=1095,t=12)
) %>% 
  #dplyr::mutate(P12=ifelse(t==12,P23,P12)) %>% 
  
  dplyr::mutate(at=factor(at,levels = c(0,90,365,1095,1826), labels=c("0 days","90 days", "1 year","3 years", "5 years"))) %>% 
  #dplyr::mutate(l95ci=dplyr::case_when(se<.1 & Estimate==1~1,T~l95ci),
  #            u95ci=dplyr::case_when(se<.1 & Estimate==1~1,T~u95ci)) %>%
  ggplot(aes(time, P12))+#,² color= setting, linetype =setting, fill=setting
  geom_step(size=1, alpha=.65) + 
  geom_ribbon(aes(ymin=ll.band, ymax=ul.band), alpha=.5)+
  facet_wrap(t~at, ncol=5, scales="free_y") + #
  scale_y_continuous(labels = scales::percent_format(accuracy = .1), limits=c(0,.25))+
  #facet_wrap(state~., ncol=1, scales="free_y") + #type_of_program
  #scale_color_manual(name= "Type of\nprogram",values=c("gray65","gray35"), labels=c("Outpatient","Residential")) +
  #scale_fill_manual(name= "Type of\nprogram",values=c("gray65","gray35"), labels=c("Outpatient","Residential")) +
  #scale_linetype_manual(name= "Type of\nprogram",values=c(1,4), labels=c("Outpatient","Residential")) +
  scale_x_continuous(breaks=seq(0,max(c(msf0$Haz$time,msf1$Haz$time),na.rm=T),by=max(c(msf0$Haz$time,msf1$Haz$time),na.rm=T)/11),
                     label=0:11)+
  xlab("Years") + 
  ylab("") + 
  theme_minimal()
```

<br>