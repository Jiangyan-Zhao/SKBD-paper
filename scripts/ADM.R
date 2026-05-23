###################################################################################################################
#                         Adaptive Dose Modification for Phase I Clinical Trials
#
#                                   Yiyi Chu, Haitao Pan and Ying Yuan
#             
#
#  This file contains two functions for implementing the proposed adaptive dose modification (ADM)
#  design.
#
#   1. ADM.oc(): to generate operating characteristics of the ADM design, coupled with Bayesian Optimal Interval (BOIN)
#                design for dose finding. We recommend the BOIN design because of its simplicity and good performance.
#
#   2. ADM.conduct(): to decide whether adding a new dose is needed during the trial conduct.
#
#
###################################################################################################################


#########################################################################################################
##           Function to generate operating characteristics of the ADM design
##
## To use this function, library "BOIN" and "locfit" should be installed in R. 
##
## Arguments:
##   target:      target toxicity rate;
##   dose:        a vector containing the dosage under investigation. We recommend standardize the dosage to [0, 1]
##   p.true:      a vector containing the true toxicity probabilities of the doses under investigation;
##   ncohort:     total number of cohorts;
##   cohortsize:  sample size of the cohort;
##   lambda1:     lower bound of the acceptable toxicity region. The default value is lambda1=0.83 x target;
##   lambda2:     upper bound of the acceptable toxicity region. The default value is lambda2=1.17 x target;
##   C1:          probability cutoff for underdosing;
##   C2:          probability cutoff for overdosing;
##   weight:      the weight that is assigned for each datum point;
##   n.earlystop: Early stopping parameter in BOIN design. If the number of patients treated at the current dose
##                reaches n.earlystop, stop the trial and select the MTD based on the observed data. The default
##                value n.earlystop=100 essentially turns off this type of early stopping;
##   startdose:   the starting dose level for the trial;            
##   p.saf:       the parameter in BOIN design, which represents the highest toxicity probability that is deemed subtherapeutic 
##                (i.e. below the MTD) such that dose escalation should be undertaken. The default value is p.saf=0.6 x target;
##   p.tox:       the parameter in BOIN design, which represents the lowest toxicity probability that is deemed overly toxic such 
##                that deescalation is required. The default value is p.tox=1.4 x target;
##   cutoff.eli:  the cutoff to eliminate an overly toxic dose for safety. We recommend the default value of (cutoff.eli=0.95) 
##                for general use;
##   extrasafe:   set extrasafe=TRUE to impose a more stringent stopping rule;
##   offset:      a small positive number (between 0 and 0.5) to control how strict the stopping rule
##                is when extrasafe=TRUE. A larger value leads to a more strict stopping rule.
##                The default value offset=0.05 generally works well;
##   ntrial:      the number of simulated trials;
#########################################################################################################

ADM.oc<-function(target, dose, p.true, ncohort, cohortsize, lambda1="default", 
                 lambda2="default", C1=0.6, C2=0.6, weight=0.7, 
                 n.earlystop=100, startdose=1,p.saf="default", p.tox="default",
                 cutoff.eli=0.95, extrasafe=FALSE, offset=0.05, ntrial=1000){
  library(locfit)
  library(BOIN)
  BOINver = substr(packageVersion("BOIN"), 1, 3)
  if(BOINver<2.6) stop("you need to update 'BOIN' to the latest version")
  
  ## isontonic transformation using the pool adjacent violator algorithm (PAVA)
  pava <- function (x, wt = rep(1, length(x))) 
  {
    n <- length(x)
    if (n <= 1) 
      return(x)
    if (any(is.na(x)) || any(is.na(wt))) {
      stop("Missing values in 'x' or 'wt' not allowed")
    }
    lvlsets <- (1:n)
    repeat {
      viol <- (as.vector(diff(x)) < 0)
      if (!(any(viol))) 
        break
      i <- min((1:(n - 1))[viol])
      lvl1 <- lvlsets[i]
      lvl2 <- lvlsets[i + 1]
      ilvl <- (lvlsets == lvl1 | lvlsets == lvl2)
      x[ilvl] <- sum(x[ilvl] * wt[ilvl])/sum(wt[ilvl])
      lvlsets[ilvl] <- lvl1
    }
    x
  }
  
  # Assign "true" toxicity probability for an inserted dose (simulation-only)
  ins.p <- function(umi){ 
    if (d!=1 & umi==d-1){
      if (p[d-1]<target & target<p[d]){
        target
      }else{      
        (p[d-1]+p[d])/2
      }   
    }else if (umi==0){
      if (target<p[d]) {
        target
      }else{
        p[d]/2
      }
    }else if (umi==d){
      if (p[d]<target & target<p[d+1]){
        target
      }else{     
        (p[d]+p[d+1])/2
      }
    }else{
      if (target>p[d]){
        target
      }else{
        p[d]+0.1
      } 
    } 
  }
  
  if (lambda1=="default") lambda1<- 0.83*target
  if (lambda2=="default") lambda2<- 1.17*target
  
  ## if the user does not provide p.saf and p.tox, set them to the default values
  if(p.saf=="default") p.saf=0.6*target
  if(p.tox=="default") p.tox=1.4*target
  ## simple error checking
  if(target<0.05) {cat("Error: the target is too low! \n"); return();}
  if(target>0.6)  {cat("Error: the target is too high! \n"); return();}
  if((target-p.saf)<(0.1*target)) {cat("Error: the probability deemed safe cannot be higher than or too close to the target! \n"); return();}
  if((p.tox-target)<(0.1*target)) {cat("Error: the probability deemed toxic cannot be lower than or too close to the target! \n"); return();}
  if(offset>=0.5) {cat("Error: the offset is too large! \n"); return();}
  if(n.earlystop<=6) {cat("Warning: the value of n.earlystop is too low to ensure good operating characteristics. Recommend n.earlystop = 9 to 18 \n"); return();}
  
  set.seed(6)
  
  npts <- ncohort*cohortsize
  ndose<- length(dose)
  options(warn=-1)
  
  dselect <- rep(0, ntrial)  # store the selected dose level
  sdose <- rep(0, ntrial)    # store the selected dose value
  cohort.marker <- rep (0, ntrial) #store the number of cohorts after which trial insertion is likely to take place 
  ins.count<- rep(0, ntrial) # store the frequency of the insertion
  
  simdata <- NULL    # store the simulated output
  a <- 0.5; b <- 0.5 # hyperparameters for beta prior
  ## obtain dose escalation and deescalation boundaries by using the "get.boundary" function from "BOIN" package
  temp <- get.boundary(target, ncohort, cohortsize, n.earlystop, p.saf, p.tox, cutoff.eli, extrasafe)$full_boundary_tab
  b.e <- temp[2,];   # escalation boundary
  b.d <- temp[3,];   # deescalation boundary
  b.elim <- temp[4, ];  # elimination boundary
  
  ##----------------Simulate trials-----------------##
  for(sim in 1:ntrial){  
    # cat("sim = ", sim, "; ")
    test.dose<-dose        ## prespecified dose
    p<-p.true              ## true toxicity rates for given doses
    D<-length(p.true)      ## the number of given doses
    x<-rep(0,D)            ## the number of DLT at each dose level
    n<-rep(0,D)            ## the number of patients treated at each dose level
    pa<-rep(a, D); pb<-rep(b,D) 
    
    d<-startdose           ## starting dose level
    status<-0              ## indicate whether all patients have been treated for one trial
    
    elimi <- rep(0, D)     ## in BOIN design: indicate whether doses are eliminated
    earlystop<-0           ## in BOIN design: indicate whether the trial terminates early
    
    count<-0               ## count the number of insertions in each simulated trial
    
    while(status==0){
      ## generate toxicity outcomes
      x[d] <- x[d] + rbinom(1,cohortsize, p[d])
      n[d] <- n[d] + cohortsize
      pa[d]<- x[d]+a; pb[d]<-n[d]-x[d]+b
      if(n[d]>=n.earlystop) break;
      parea<-matrix(0,nrow=D,ncol=3)
      for (i in 1:D){
        parea[i,]<-c(pbeta(lambda1, pa[i], pb[i]),pbeta(lambda2, pa[i], pb[i])-pbeta(lambda1, pa[i], pb[i]), 1-pbeta(lambda2, pa[i], pb[i])) 
      }
      pareal<-rev(pava(rev(parea[,1]))); pareau<-pava(parea[,3])
      paream<-rep(1,D)-pareal-pareau
      
      ## determine if the dose insertion conditions are satisfied
      if (d==1){
        parea1<-c(pareal[d],paream[d],pareau[d])
        parea2<-c(pareal[d+1],paream[d+1],pareau[d+1])
        mi1 <- which(parea1==max(parea1))
        mi2 <- which(parea2==max(parea2)) # later dose 
        if (mi1==3 & parea1[3]>C2){
          # current 2nd upper satisfied->insert before
          mi <- 1; umi <- 0
        }else if (n[d+1] !=0 & mi1==1 & mi2==3 & parea2[3]>C2 & parea1[1]>C1){
          # both two condition satisfied-> insert at current
          mi <- 1; umi <- d
        }else{
          # no insert
          mi <- 0; 
        }
      } else if (d==D){
        parea1<-c(pareal[d-1],paream[d-1],pareau[d-1])
        parea2<-c(pareal[d],paream[d],pareau[d])
        mi1 <- which(parea1==max(parea1))
        mi2 <- which(parea2==max(parea2))
        mi <- ifelse( (mi2==1 & parea2[1]>C1) | 
                    (mi1==1 & mi2==3 & parea2[3]>C2 & parea1[1]>C1), 1, 0 )
        umi <- ifelse(mi2==1, 999, d-1)
      } else {
        parea1<-c(pareal[d-1],paream[d-1],pareau[d-1])
        parea2<-c(pareal[d],paream[d],pareau[d])
        parea3<-c(pareal[d+1],paream[d+1],pareau[d+1])
        mi1 <- which(parea1==max(parea1)) 
        mi2 <- which(parea2==max(parea2))
        mi3 <- which(parea3==max(parea3))
        if (mi1==1 & mi2==3 & parea2[3]>C2 & parea1[1]>C1){
          mi <- 1; umi <- d-1
        } else if (n[d+1]!=0 & mi2==1 & mi3==3 & parea2[1]>C1 & parea3[3]>C2){
          mi <- 1; umi <- d
        } else{
          mi <- 0
        }
      }
      ## implement the dose insertion if conditions are satisfied        
      if (mi==1 & sum(n) < npts & max(x)>0){
        if (umi==0){
          if (test.dose[d]==dose[1] & sum(n) <= (npts-2*cohortsize)){
            tmp <- sort(c(test.dose, test.dose[d]/2), index.return=T)
            test.dose <- tmp$x
            if (D==length(p.true)){cohort.marker[sim]<-sum(n)/cohortsize} ## record after how many cohorts the insertion takes place
            
            p <- c(p, ins.p(umi)); p<-p[tmp$ix] 
            D <- D+1
            
            x <- c(x, 0); n <- c(n, 0);   
            x <- x[tmp$ix]; n <- n[tmp$ix]; 
            pa <- c(pa, a); pb <- c(pb, b)
            pa <- pa[tmp$ix]; pb <- pb[tmp$ix]
            d <- 1; elimi<-c(elimi,0); elimi <-elimi[tmp$ix]
            count <- count + 1
            ## treat the next cohort of patients with the newly inserted dose
            
            x[d] <- x[d] + rbinom(1,cohortsize, p[d])
            n[d] <- n[d] + cohortsize
            pa[d]<- x[d]+a
            pb[d]<-n[d]-x[d]+b   
            
            bandwidth<-(0.5*abs(test.dose[d]-test.dose[d+1]))/((1-weight^(1/3))^(1/3))
            fit <- locfit(
              (x+0.05)~lp(test.dose,h=bandwidth),weights=n+0.5,deg=1,  family="binomial", link="logit",kern="tcub",maxk=5000
              )
            newdata<-seq(test.dose[d],test.dose[d+1],0.01)###
            dif.new<-abs(pava(predict(fit,newdata))-target)
            newdose<-newdata[min(which(dif.new==min(dif.new)))]
          } else {stop<-1; newdose<-NA}  
        } else if (d!=1 & umi==d-1){
          bandwidth<-(0.5*(test.dose[d]-test.dose[d-1]))/((1-weight^(1/3))^(1/3))
          fit <- locfit((x+0.05)~lp(test.dose, h=bandwidth),weights=n+0.5,deg=1,  family="binomial", link="logit",kern="tcub",maxk=5000)
          newdata<-seq(test.dose[d-1],test.dose[d],0.01)
          dif.new<-abs(pava(predict(fit,newdata))-target)
          newdose<-newdata[min(which(dif.new==min(dif.new)))]
        } else if (umi==d){
          bandwidth<-(0.5*(test.dose[d+1]-test.dose[d]))/((1-weight^(1/3))^(1/3))
          fit <- locfit((x+0.05)~lp(test.dose, h=bandwidth),weights=n+0.5,deg=1,  family="binomial", link="logit",kern="tcub",maxk=5000)
          newdata<-seq(test.dose[d],test.dose[d+1],0.01)
          dif.new<-abs(pava(predict(fit,newdata))-target)
          newdose<-newdata[min(which(dif.new==min(dif.new)))]
        } else {
          if (test.dose[d]==1.5*dose[ndose]){
            stop=1; break
          }else {
            bandwidth<-( 0.5* (1.5*dose[ndose]-test.dose[d]) )/((1-weight^(1/3))^(1/3))
            fit <- locfit((x+0.05)~lp(test.dose, h=bandwidth),weights=n+0.5,deg=1,  family="binomial", link="logit",kern="tcub",maxk=5000)
            newdata<-seq(test.dose[d],1.5*dose[ndose],0.01)
            dif.new<-abs(pava(predict(fit,newdata))-target)
            newdose<-newdata[min(which(dif.new==min(dif.new)))]
          }
        }
        ## rearrange the dose and toxicity rate vectors to reflect the correct ordering
        if ( (!is.na(newdose)) && (!(newdose %in% test.dose)) ){          
          tmp <- sort(c(test.dose, newdose), index.return=T)
          test.dose <- tmp$x
          if (D==length(p.true)){cohort.marker[sim]<-sum(n)/cohortsize} ## record after how many cohorts the insertion takes place
          
          p <- c(p, ins.p(umi)); p <- p[tmp$ix] 
          D <- D+1
          
          x <- c(x, 0); n <- c(n, 0) 
          x <- x[tmp$ix]; n <- n[tmp$ix]
          
          pa <- c(pa, a); pb <- c(pb, b)
          pa <- pa[tmp$ix]; pb <- pb[tmp$ix]
          elimi<-c(elimi,0); elimi <-elimi[tmp$ix]
          
          d <- which(newdose==test.dose)
          if (length(d)>1){
            d <- sample(d,1)
          }
          count<-count+1
          ## treat the next cohort of patients with the newly inserted dose
          if(sum(n)<npts){
            x[d] <- x[d] + rbinom(1,cohortsize, p[d])
            n[d] <- n[d] + cohortsize
            pa[d]<- x[d]+a
            pb[d]<-n[d]-x[d]+b                
          }
        } 
      }
      
      ## BOIN design: determine if the current dose should be eliminated
      if (!is.na(b.elim[n[d]])){
        if (x[d]>=b.elim[n[d]]){
          elimi[d:D]<-1
          if(d==1) {earlystop=1; break}
        }
        ## BOIN design: implement the extra safe rule by decreasing the elimination cutoff for the lowest dose
        if (extrasafe){
          if (d==1 && x[1]>3){
            if (1-pbeta(target,x[1]+1,n[1]-x[1]+1)>cutoff.eli-offset) {
              earlystop=1;break
            }
          }
        }
      }
      
      ## dose escalation/de-escalation based on BOIN design
      if (x[d]<=b.e[n[d]] && d!=D) {
        if (elimi[d+1]==0) d<-d+1
      }else if (x[d]>=b.d[n[d]] && d!=1) {
        d<-d-1
      }else{d=d}
      
      # end 
      total<-sum(n)
      if(total >= npts){status <- 1}  
    }#end while
    
    ## dose selection by BOIN design  
    if(earlystop==1) { 
      dselect[sim]=99
    }else  {
      dselect[sim]=select.mtd(target, n, x, cutoff.eli, extrasafe, offset)$MTD 
    }
    ## save the results
    ins.count[sim]<-count
    sdose[sim]<- ifelse(dselect[sim]==0, NA, test.dose[dselect[sim]])
    temp.simdata <- cbind(rep(sim, length(x)), test.dose, n, x, rep(dselect[sim], length(x)))
    simdata <- rbind(simdata, temp.simdata)
  }
  ## output results
  simdata <- data.frame(simdata)
  names(simdata) <- c("Simulation", "Dose", "N", "X", "Selection")
  selpercent<- rep(0, ndose) # selection percentage at prespecified doses
  ptspercent<- rep(0, ndose) # percentage of patients at prespecified doses
  for (i in 1:ndose) {
    selpercent[i]<-sum(sdose==dose[i],na.rm=TRUE)/ntrial*100
    ptspercent[i]<-(sum(simdata[which(simdata$Dose==dose[i]),3])/(ntrial*npts))*100
  }
  ins.select<- 100-sum(selpercent) # selection percentage at inserted dose
  ins.pts<- 100-sum(ptspercent)    # percentage of patients at inserted dose
  
  ins.dose<-NULL
  for (i in 1:ntrial){
    if (sdose[i] %in% dose) {ins.dose<-ins.dose}
    else {ins.dose<-c(ins.dose,sdose[i])}
  }
  ins.mean <- mean(ins.dose,na.rm=TRUE)
  ins.sd <- sd(ins.dose, na.rm=TRUE)
  ins.percent <-(ntrial-sum(ins.count==0))/ntrial*100 
  cohort.mean <- mean(cohort.marker[which(cohort.marker!=0)])
  insTimes.median <- median(ins.count)
  # cat ("selection percentage at each prespecified dose level (%):\n")
  # cat (formatC(selpercent,digits=2, format="f"),sep="  ","\n")
  # cat ("percentage of patients treated at prespecified dose level (%):\n")
  # cat (formatC(ptspercent,digits=2, format="f"),sep="  ","\n")
  # cat ("mean of the inserted dose (SD):",formatC(ins.mean,digits=2,format="f"),"(",formatC(ins.sd, digits=2, format="f"),") \n")
  # cat ("selection percentage of the inserted dose (%):",formatC(ins.select,digits=2, format="f"),"\n")
  # cat ("percentage of patients treated at the inserted doses (%):",formatC(ins.pts, digits=2, format="f"),"\n")
  # cat ("percentage of trials with dose insertion (%):",formatC(ins.percent,digits=1, format="f"),"\n")
  # cat ("The average of cohorts after which trial insertion is likely to take place:",formatC(cohort.mean,digits=1,format="f"),"\n")
  # cat ("Median number of insertion times:", formatC(insTimes.median,digits=1,format="f"),"\n")

  out <- list(
    # prespecified-dose summaries
    sel_pct_prespec = selpercent, 
    pts_pct_prespec = ptspercent,
    
    # insertion summaries
    insertion = list(
      sel_pct     = ins.select,
      pts_pct     = ins.pts,
      dose_mean   = ins.mean,
      dose_sd     = ins.sd,
      trial_pct   = ins.percent,
      cohort_mean = cohort.mean,
      n_median    = insTimes.median
    )
  )
}
