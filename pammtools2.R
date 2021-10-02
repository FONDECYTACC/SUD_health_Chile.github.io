
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("Investigating time-varying coefficient with timereg package")
#https://www.ncbi.nlm.nih.gov/labs/pmc/articles/PMC6015946/

#Zhang, Z., Reinikainen, J., Adeleke, K. A., Pieterse, M. E., & Groothuis-Oudshoorn, C. (2018). Time-varying covariates 
#and coefficients in Cox regression models. Annals of translational medicine, 6(7), 121. https://doi.org/10.21037/atm.2018.02.12

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#In this case the time-varying effect is tested by resampling method 

# fits Cox models with possibly time-varying effects
#fits the proportional hazards model, but allows for covariates to be included under the non-proportional hazards assumption as well,
#using an extension of the traditional Cox model

#recommended for instances where testing the significance of the NPH covariate effects are important, particularly if the estimate of the hazard rate is not imperative. The timereg package is well-designed, user-friendly,
#and has plenty of associated documentation, and thus it is a very valuable piece of software for implementing
#a variety of complex survival models. However, for survival modeling when the estimate of the hazard rate is
#needed, other more reliable functions from other packages are available for including covariates under both the
#PH and the NPH setting

library(timereg)



#here the test processes for each covariate with 1,00 randomly choosen 
#realizations under the null hypothesis of constant effects
timecox_pwp<- 
  timecox(Surv(Tstart,Tstop,status) ~ tipo_de_plan_res_1+  
            TD_1+ TD_2+ TD_3+ TD_4+ cluster(id) + strata(trans),
          data = ms_d_match_surv, n.sim=1000,weighted.test=1)
#The default study period in the timecox() model is the first and last observed (i.e. uncensored) failure, but this can be changed by
#the user through the “start.time” and “max.time” options.

# Multiplicative Hazard Model 
# 
# Test for nonparametric terms 
# 
# Test for non-significant effects 
# Supremum-test of significance p-value H_0: B(t)=0
# (Intercept)                                    Inf                   0
# tipo_de_plan_res_1                             Inf                   0
# TD_1                                           Inf                   0
# TD_2                                           Inf                   0
# TD_3                                           Inf                   0
# TD_4                                           Inf                   0
# strata(trans)trans=2                           Inf                   0
# strata(trans)trans=3                           Inf                   0
# strata(trans)trans=4                           Inf                   0
# 
# Test for time invariant effects 
# Kolmogorov-Smirnov test p-value H_0:constant effect
# (Intercept)                                   5.38                       0.000 #time-varying
# tipo_de_plan_res_1                            3.32                       0.041 #time-varying
# TD_1                                          9.01                       0.000 #time-varying
# TD_2                                          1.30                       0.981
# TD_3                                          1.93                       0.697
# TD_4                                          3.58                       0.019 #time-varying
# strata(trans)trans=2                          1.91                       0.466
# strata(trans)trans=3                          3.09                       0.044 #time-varying
# strata(trans)trans=4                          3.34                       0.015 #time-varying
# Cramer von Mises test p-value H_0:constant effect
# (Intercept)                                  50800                       0.000 #time-varying
# tipo_de_plan_res_1                           10300                       0.027 #time-varying
# TD_1                                        147000                       0.000 #time-varying
# TD_2                                           591                       0.992
# TD_3                                          2200                       0.669
# TD_4                                         20300                       0.002 #time-varying
# strata(trans)trans=2                          4230                       0.295
# strata(trans)trans=3                         12700                       0.039 #time-varying
# strata(trans)trans=4                         20100                       0.002 #time-varying
# 
# 
# 
# Call: 
#   timecox(formula = Surv(Tstart, Tstop, status) ~ tipo_de_plan_res_1 + 
#             TD_1 + TD_2 + TD_3 + TD_4 + cluster(id) + strata(trans), 
#           data = ms_d_match_surv, n.sim = 1000, weighted.test = 1)

#The variable age has no significant effect because the confidence interval intersects with the null effect reference line. 

#Libro Dynamic Regression Models for Survival Data, Martinussen & Scheike, 2007
# The test for time-varying effects suggest that the effect off xc might be time-varying while it is acceptable to
# assume constant effect of x2 and x3. We therefore proceed with the semiparametric model assuming fist constant 
#effect of x2 and then x2 and x3

#Estimated cumulative coefficients with 95% pointwise confidence intervals for intercept & terms
plot(timecox_pwp)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("no cambiaré el maxtime, pero sí llevaré a constantes algunas variables encontradas arriba")
timecox_pwp_const1<- 
  timecox(Surv(Tstart,Tstop,status) ~ tipo_de_plan_res_1+  
            TD_1+ const(TD_2)+ const(TD_3)+ TD_4+ cluster(id) + strata(trans),
          data = ms_d_match_surv, n.sim=1000)

summary(timecox_pwp_const1)
# Multiplicative Hazard Model 
# 
# Test for nonparametric terms 
# 
# Test for non-significant effects 
# Supremum-test of significance p-value H_0: B(t)=0
# (Intercept)                                    Inf                   0
# tipo_de_plan_res_1                             Inf                   0
# TD_1                                           Inf                   0
# TD_4                                           Inf                   0
# strata(trans)trans=2                           Inf                   0
# strata(trans)trans=3                           Inf                   0
# strata(trans)trans=4                           Inf                   0
# 
# Test for time invariant effects 
# Kolmogorov-Smirnov test p-value H_0:constant effect
# (Intercept)                                    367                       0.648
# tipo_de_plan_res_1                             135                       0.482
# TD_1                                           381                       0.011 # time-varying
# TD_4                                           187                       0.336
# strata(trans)trans=2                           475                       0.543
# strata(trans)trans=3                           365                       0.702
# strata(trans)trans=4                           593                       0.357
# Cramer von Mises test p-value H_0:constant effect
# (Intercept)                               1.74e+08                       0.138
# tipo_de_plan_res_1                        1.35e+07                       0.208
# TD_1                                      1.82e+08                       0.000 # time-varying
# TD_4                                      1.97e+07                       0.221
# strata(trans)trans=2                      6.60e+07                       0.555
# strata(trans)trans=3                      7.46e+07                       0.477
# strata(trans)trans=4                      1.69e+08                       0.315
# 
# Parametric terms :     
#   Coef.     SE Robust SE     z    P-val lower2.5% upper97.5%
#   const(TD_2) -0.143 0.0576    0.0498 -2.88 0.003970    -0.256    -0.0301
# const(TD_3) -0.194 0.0557    0.0464 -4.19 0.000028    -0.303    -0.0848
# 
# Call: 
#   timecox(formula = Surv(Tstart, Tstop, status) ~ tipo_de_plan_res_1 + 
#             TD_1 + const(TD_2) + const(TD_3) + TD_4 + cluster(id) + strata(trans), 
#           data = ms_d_match_surv, n.sim = 1000)

timecox_pwp_const2<- 
  timecox(Surv(Tstart,Tstop,status) ~ tipo_de_plan_res_1+  
            TD_1+ const(TD_2)+ const(TD_3)+ const(TD_4)+ cluster(id) + const(strata(trans)),
          data = ms_d_match_surv, n.sim=1000)

summary(timecox_pwp_const2)

# Multiplicative Hazard Model 
# 
# Test for nonparametric terms 
# 
# Test for non-significant effects 
# Supremum-test of significance p-value H_0: B(t)=0
# (Intercept)                               162.00                   0
# tipo_de_plan_res_1                          4.59                   0
# TD_1                                        8.70                   0
# 
# Test for time invariant effects 
# Kolmogorov-Smirnov test p-value H_0:constant effect
# (Intercept)                                  479                       0.185
# tipo_de_plan_res_1                           231                       0.932
# TD_1                                         634                       0.261
# Cramer von Mises test p-value H_0:constant effect
# (Intercept)                             4.12e+08                       0.039
# tipo_de_plan_res_1                      3.14e+07                       0.837
# TD_1                                    5.49e+08                       0.091
# 
# Parametric terms :     
#   Coef.     SE Robust SE      z    P-val lower2.5% upper97.5%
#   const(TD_2)                 -0.1070 0.0581    0.0506  -2.12 3.37e-02   -0.2210    0.00687
# const(TD_3)                 -0.1790 0.0565    0.0473  -3.79 1.54e-04   -0.2900   -0.06830
# const(TD_4)                  0.0644 0.0570    0.0504   1.28 2.01e-01   -0.0473    0.17600
# const(strata(trans))trans=2 -0.2290 0.0629    0.0551  -4.16 3.18e-05   -0.3520   -0.10600
# const(strata(trans))trans=3 -0.4590 0.0718    0.0659  -6.97 3.20e-12   -0.6000   -0.31800
# const(strata(trans))trans=4 -2.0200 0.0951    0.1190 -17.00 0.00e+00   -2.2100   -1.83000
# 
# Call: 
#   timecox(formula = Surv(Tstart, Tstop, status) ~ tipo_de_plan_res_1 + 
#             TD_1 + const(TD_2) + const(TD_3) + const(TD_4) + cluster(id) + 
#             const(strata(trans)), data = ms_d_match_surv, n.sim = 1000)

invisible("Tengo la duda de si seguir, porque el test supremo ya encuentra diferencias significativas")

plot(timecox_pwp_const2)
invisible("Se observa que los coeficientes acumulados no son tan fuertes al principio, pero depsués del año sí hasta")
invisible("un poco antes del día 2000 en el que se vuelve nulo")
invisible("Para TD 1 se ve el efecto protector desde el primer día, aunque pierde fuerza después de los 2000 días llegando a ser nulo")


#https://arxiv.org/pdf/1509.03253.pdf
timecox.alpha0 = timecox_pwp_const2$cum[,2] #intercept
timecox.covarNPH.cumul = timecox_pwp_const2$cum[,-(1:2)] #saca intercepto y tiempo
timecox.covarNPH.cumul.var = timecox_pwp_const2$var.cum
invisible("no obtengo nada de esto")
decumulated.ests = CsmoothB(timecox_pwp_const2$cum, c(91,364,366,1096,1825,1827), b = 1) #De-cumulates the cumulative estimates
timecox.hazardrate = exp(decumulated.ests[,2])
timecox.covarNPH = decumulated.ests[,-(1:2)]
cbind(decumulated.ests[,1],exp(decumulated.ests[,2:4]))

#In this manuscript, we approximated the 95% confidence interval bounds for exp{α0} and β(t) by calculating the 95% upper and
#lower confidence bounds for the cumulative estimates, then applying the CsmoothB() function to these functions.

#cbind.data.frame(time=timecox_pwp_const2$cum[,1],B=timecox_pwp_const2$cum[,3], sd=sqrt(timecox_pwp_const2$var.cum[,3]))
#sem<-sd(sqrt(timecox_pwp_const2$var.cum[,3]))/sqrt(length(timecox_pwp_const2$var.cum[,3]))


plot_cums <-  function (x , pointwise.ci=1, hw.ci=0, sim.ci=0, robust.ci=0, col=NULL,
                        specific.comps=FALSE,level=0.05, start.time = 0, 
                        stop.time = 0, add.to.plot=FALSE,main=NULL,mains=TRUE, xlab="Time",
                        ylab ="Cumulative coefficients",ylim=NULL,...) 
  # pointwise.ci=1 
  # hw.ci=0
  # sim.ci=0
  # robust.ci=0
  # col=NULL
  # specific.comps=FALSE
  # level=0.05
  # start.time = 0 
  # stop.time = 0
  # add.to.plot=FALSE
  # main=NULL
# mains=TRUE
# ylab ="Cumulative coefficients"
# ylim=NULL
{ ## {{{ 
  
  #x<-timecox_pwp_const2
  name_model<-deparse(substitute(x))#deparse(quote(x))
  object<-x; rm(x); 
  B<-object$cum; V<-object$var.cum; p<-dim(B)[[2]]; 
  if (robust.ci>=1) {V<-object$robvar.cum;}
  ### color for estimate, pointwise, robust-pointwise, sim.ci , hw.ci  
  cis <- c(1,pointwise.ci>=1,robust.ci>=1,sim.ci>=1,hw.ci>=1)
  if (is.null(col)) cols<-rep(1,5)  else { 
    cols <-  rep(1,5)
    cols[which(cis==TRUE)] <- col
  }
  
  if (sum(specific.comps)==FALSE) comp<-2:p else comp<-specific.comps+1
  if (stop.time==0) stop.time<-max(B[,1]);
  
  med<-B[,1]<=stop.time & B[,1]>=start.time
  B<-B[med,]; Bs<-B[1,];  B<-t(t(B)-Bs); B[,1]<-B[,1]+Bs[1];
  V<-V[med,]; Vs<-V[1,]; V<-t( t(V)-Vs); 
  Vrob<-object$robvar.cum; 
  Vrob<-Vrob[med,]; Vrobs<-Vrob[1,]; Vrob<-t( t(Vrob)-Vrobs); 
  
  c.alpha<- qnorm(1-level/2)
  i <- 0
  db<-list()
  for (v in comp) { 
    i <- i+1
    c.alpha<- qnorm(1-level/2)
    est<-B[,v];ul<-B[,v]+c.alpha*V[,v]^.5;nl<-B[,v]-c.alpha*V[,v]^.5;
    db[[match(v,comp)]]<-cbind.data.frame(B[,1],est,ul,nl)
    if (add.to.plot==FALSE) 
    {
      if (is.null(ylim))
        plot(B[,1],est,ylim=1.05*range(ul,nl),type="s",xlab=xlab,ylab=ylab,col=cols[1],...) 
      else
        plot(B[,1],est,ylim=ylim,type="s",xlab=xlab,ylab=ylab,col=cols[1],...) 
      if (!is.null(main)) { if (length(main)==1) main <- rep(main,length(comp)); mains <- FALSE; } 
      if (!is.null(main)) title(main=main[i]); 
      if (mains==TRUE) title(main=colnames(B)[v]); 
    }
    else lines(B[,1],est,type="s",col=cols[1]); 
    
    if (pointwise.ci>=1) {
      lines(B[,1],ul,lty=pointwise.ci,type="s",col=cols[2]);
      lines(B[,1],nl,lty=pointwise.ci,type="s",col=cols[2]); }
    if (robust.ci>=1) {
      lines(B[,1],ul,lty=robust.ci,type="s",col=cols[3]); 
      lines(B[,1],nl,lty=robust.ci,type="s",col=cols[3]); }
    if (hw.ci>=1) {
      if (level!=0.05) cat("Hall-Wellner bands only 95 % \n");
      tau<-length(B[,1])
      nl<-B[,v]-1.27*V[tau,v]^.5*(1+V[,v]/V[tau,v])
      ul<-B[,v]+1.27*V[tau,v]^.5*(1+V[,v]/V[tau,v])
      lines(B[,1],ul,lty=hw.ci,type="s",col=cols[5]); 
      lines(B[,1],nl,lty=hw.ci,type="s",col=cols[5]); 
      db[[match(v,comp)]][[1]]<-cbind.data.frame(ul,nl)
    }
    if (sim.ci>=1) {
      if (is.null(object$conf.band)==TRUE) 
        cat("Uniform simulation based bands only computed for n.sim> 0\n")
      if (level!=0.05) c.alpha<-percen(object$sim.testBeq0[,v-1],1-level)
      else c.alpha<-object$conf.band[v-1];
      nl<-B[,v]-c.alpha*Vrob[,v]^.5; ul<-B[,v]+c.alpha*Vrob[,v]^.5;
      lines(B[,1],ul,lty=sim.ci,type="s",col=cols[4]); 
      lines(B[,1],nl,lty=sim.ci,type="s",col=cols[4]); 
      db[[match(v,comp)]][[1]]<-cbind.data.frame(ul,nl)
    }
    abline(h=0)
  }
  names(db)<-gsub("^",paste0(name_model,"_"),attr(object$cum,"dimnames")[[2]][2:length(attr(object$cum,"dimnames")[[2]])])
  #names(db)<-attr(object$cum,"dimnames")[[2]][2:length(attr(object$cum,"dimnames")[[2]])]
  list2env(db, envir = .GlobalEnv)
  return(db)
} ## }}} 

plot_cums(timecox_pwp_const2)

timecox_pwp_const2_tipo_de_plan_res_1 %>% 
  dplyr::mutate(year=`B[, 1]`/365.25,variable="Residential\nModality") %>% 
  dplyr::filter(year<=5) %>% 
  dplyr::bind_rows(timecox_pwp_const2_TD_1 %>% 
                     dplyr::mutate(year=`B[, 1]`/365.25,variable="Treatment\nCompletion")%>% 
                     dplyr::filter(year<=5)) %>% 
  dplyr::mutate(variable=factor(variable)) %>% 
  ggplot()+
  geom_ribbon(aes(x=year, ymin = nl, ymax = ul, fill =variable), alpha=.5) +# "steelblue"
  geom_line(aes(x=year, y = est, color=variable))+ #"dodgerblue4"
  theme_sjplot2()+ 
  geom_hline(yintercept = 0)+
  scale_color_manual(name="Variable",values=c("dodgerblue4","black"))+
  scale_fill_manual(name="Variable",values=c("steelblue","grey70"))+
  labs(y="Cumulative coefficients",x="Years")+
  scale_y_continuous(breaks = seq(-900,+900,by = 90), limits=c(-900,900))+
  scale_x_continuous(breaks = seq(0,5,by = .5), limits=c(0,5))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 13))+
  theme(axis.text.y = element_text(size = 13))
