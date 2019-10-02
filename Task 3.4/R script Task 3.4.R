library(Rmisc)
library(ggplot2)
library(reshape2)
rm(list=ls(all=TRUE))
# Parameter setting
set.seed(124)
k=100 #Number of simulations

########################################################################################################
############################## Sardina pilchardus ######################################################
#------- PS_SPF Sardine GSA 6 --------
N.baseline=20518; 
N.optimal=39130 
n_baseline_trips=301
n_optimal_trips=301 
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=20; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.pil_6ps_spf=SumStats$values[1]-SumStats$values[2]
Dif.pil_6ps_spf.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.pil_6ps_spf.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- PS_SPF Sardine GSA 7 --------

N.baseline=1209; #N Baseline
N.optimal=1000 #N of fish, proposed sampling plan for the RSP 2019
n_baseline_trips=20
n_optimal_trips=20
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips

Sample.mean.cost=20; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)

Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.pil_7ps_spf=SumStats$values[1]-SumStats$values[2]
Dif.pil_7ps_spf.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.pil_7ps_spf.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- PS_SPF Sardine GSA 17_HRV --------

N.baseline=10764; #N Baseline
N.optimal=9940; #N of fish, proposed sampling plan for the RSP 2019
n_baseline_trips=64
n_optimal_trips=71
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips

Sample.mean.cost=20; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Pd=0.5

Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.pil_17HRVps_spf=SumStats$values[1]-SumStats$values[2]
Dif.pil_17HRVps_spf.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.pil_17HRVps_spf.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- PS_SPF Sardine GSA 17_SVN --------

N.baseline=913; #N Baseline
N.optimal=1920; #N of fish, proposed sampling plan for the RSP 2019
n_baseline_trips=5
n_optimal_trips=12
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips

Sample.mean.cost=20; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)

Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.pil_17SVNps_spf=SumStats$values[1]-SumStats$values[2]
Dif.pil_17SVNps_spf.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.pil_17SVNps_spf.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- PTM_SPF Sardine GSA 17_18_ITA --------

N.baseline=8658; #N Baseline
N.optimal=6300; #N of fish, proposed sampling plan for the RSP 2019
n_baseline_trips=42
n_optimal_trips=42
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips

Sample.mean.cost=20; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)

Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.pil_17_18ITAptm_spf=SumStats$values[1]-SumStats$values[2]
Dif.pil_17_18ITAptm_spf.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.pil_17_18ITAptm_spf.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#Compiling results
Species=c("PIL 6ps","PIL 7ps","PIL 17HRVps","PIL 17SVNps","PIL 17_18ITAptm")

Dif=as.numeric(c(Dif.pil_6ps_spf,Dif.pil_7ps_spf,Dif.pil_17HRVps_spf,Dif.pil_17SVNps_spf,Dif.pil_17_18ITAptm_spf))
Dif.sd=as.numeric(c(Dif.pil_6ps_spf.sd,Dif.pil_7ps_spf.sd,Dif.pil_17HRVps_spf.sd,Dif.pil_17SVNps_spf.sd,Dif.pil_17_18ITAptm_spf.sd))
Dif.pct=as.numeric(c(Dif.pil_6ps_spf.pct,Dif.pil_7ps_spf.pct,Dif.pil_17HRVps_spf.pct,Dif.pil_17SVNps_spf.pct,Dif.pil_17_18ITAptm_spf.pct))
Results=data.frame(Species,Dif,Dif.sd,Dif.pct)

ggplot(Results, aes(x=Species, y=Dif*-1,colour=Species))+
  geom_errorbar(aes(ymin=(Dif*-1)-1.96*Dif.sd, ymax=(Dif*-1)+1.96*Dif.sd),width=.1)+
  geom_point()+ylab("Cost Difference (€)")+xlab("Species, Gear & GSA")+
  geom_hline(aes(yintercept=0),color="black", linetype="dashed")+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=10,face="plain"))+theme(legend.position="none")


#ggsave("PIL.tiff",path = 'C:/Projects/STREAM_MARE/WP3_T34', width = 14.4, height = 8.32, units = "cm", dpi = 500)
ggsave("PIL.png",path = 'C:/Projects/STREAM_MARE/WP3_T34', width = 14.4, height = 8.32, units = "cm", dpi = 500)

##################################### Sardina pilchardus END ###########################################

###################################### Anchovy BEGIN ###################################################
#------- PS_SPF Anchovy GSA 1 --------
library(Rmisc)
library(ggplot2)
library(reshape2)
rm(list=ls(all=TRUE))
# Parameter setting
set.seed(124)
k=100 #Number of simulations

N.baseline=3900; #N Baseline
N.optimal=7680 #N of fish, proposed sampling plan for the RSP 2019
n_baseline_trips=116
n_optimal_trips=64
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips

Sample.mean.cost=20; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.ane_1ps_spf=SumStats$values[1]-SumStats$values[2]
Dif.ane_1ps_spf.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.ane_1ps_spf.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- PS_SPF Anchovy GSA 5 --------
N.baseline=1420; #N Baseline
N.optimal=1500 #N of fish, proposed sampling plan for the RSP 2019
n_baseline_trips=10
n_optimal_trips=10
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=20; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.ane_5ps_spf=SumStats$values[1]-SumStats$values[2]
Dif.ane_5ps_spf.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.ane_5ps_spf.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- PS_SPF Anchovy GSA 6 --------
N.baseline=25762; #N Baseline
N.optimal=51170 #N of fish, proposed sampling plan for the RSP 2019
n_baseline_trips=301
n_optimal_trips=301
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=20; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.ane_6ps_spf=SumStats$values[1]-SumStats$values[2]
Dif.ane_6ps_spf.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.ane_6ps_spf.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- PS_SPF Anchovy GSA 17_HRV --------
N.baseline=5233; #N Baseline
N.optimal=9940; #N of fish, proposed sampling plan for the RSP 2019
n_baseline_trips=64
n_optimal_trips=71
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=20; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.ane_17HRVps_spf=SumStats$values[1]-SumStats$values[2]
Dif.ane_17HRVps_spf.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.ane_17HRVps_spf.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- PS_SPF Anchovy GSA 17_SVN --------
N.baseline=799; #N Baseline
N.optimal=1920; #N of fish, proposed sampling plan for the RSP 2019
n_baseline_trips=5
n_optimal_trips=12
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips

Sample.mean.cost=20; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.ane_17SVNps_spf=SumStats$values[1]-SumStats$values[2]
Dif.ane_17SVNps_spf.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.ane_17SVNps_spf.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- PTM_SPF Anchovy GSA 17_18_ITA --------
N.baseline=11369; #N Baseline
N.optimal=6300; #N of fish, proposed sampling plan for the RSP 2019
n_baseline_trips=42
n_optimal_trips=42
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=20; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.ane_17_18ITAptm_spf=SumStats$values[1]-SumStats$values[2]
Dif.ane_17_18ITAptm_spf.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.ane_17_18ITAptm_spf.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#Compiling results
Species=c("ANE 1ps","ANE 5ps","ANE 6ps","ANE 17HRVps","ANE 17SVNps","ANE 17_18ITAptm")

Dif=as.numeric(c(Dif.ane_1ps_spf,Dif.ane_5ps_spf,Dif.ane_6ps_spf,Dif.ane_17HRVps_spf,Dif.ane_17SVNps_spf,Dif.ane_17_18ITAptm_spf))
Dif.sd=as.numeric(c(Dif.ane_1ps_spf.sd,Dif.ane_5ps_spf.sd,Dif.ane_6ps_spf.sd,Dif.ane_17HRVps_spf.sd,Dif.ane_17SVNps_spf.sd,Dif.ane_17_18ITAptm_spf.sd))
Dif.pct=as.numeric(c(Dif.ane_1ps_spf.pct,Dif.ane_5ps_spf.pct,Dif.ane_6ps_spf.pct,Dif.ane_17HRVps_spf.pct,Dif.ane_17SVNps_spf.pct,Dif.ane_17_18ITAptm_spf.pct))
Results=data.frame(Species,Dif,Dif.sd,Dif.pct)

ggplot(Results, aes(x=Species, y=Dif*-1,colour=Species))+
  geom_errorbar(aes(ymin=(Dif*-1)-1.96*Dif.sd, ymax=(Dif*-1)+1.96*Dif.sd),width=.1)+
  geom_point()+ylab("Cost Difference (€)")+xlab("Species, Gear & GSA")+
  geom_hline(aes(yintercept=0),color="black", linetype="dashed")+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=10,face="plain"))+theme(legend.position="none")

#ggsave("ANE.tiff",path = 'C:/Projects/STREAM_MARE/WP3_T34', width = 14.4, height = 8.32, units = "cm", dpi = 500)
ggsave("ANE.png",path = 'C:/Projects/STREAM_MARE/WP3_T34', width = 14.4, height = 8.32, units = "cm", dpi = 500)

#------- OTB Hake GSA 22 & 23 --------
library(Rmisc)
library(ggplot2)
library(reshape2)
rm(list=ls(all=TRUE))
# Parameter setting
set.seed(124)
k=100 #Number of simulations

N.baseline=16840+509; #N of samples 2016
N.optimal=2861+754 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=100+14
n_optimal_trips=17+20
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=300; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.hke22_23otb=SumStats$values[1]-SumStats$values[2]
Dif.hke22_23otb.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.hke22_23otb.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- NETS (Metier Lvl 4) Hake GSA 22 & 23 --------
N.baseline=275+28; #N of samples 2016
N.optimal=178+57 #N of fish, proposed sampling plan for the RSP 2019
n_baseline_trips=97+14
n_optimal_trips=65+53
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=300; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=200
Uc_trip_optimal=200
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.hke22_23nets=SumStats$values[1]-SumStats$values[2]
Dif.hke22_23nets.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.hke22_23nets.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- OTB Red mullet GSA 22 & 23 --------
N.baseline=9801+805; #N of samples 2016
N.optimal=1614+1111 #N of fish, proposed sampling plan for the RSP 2019
n_baseline_trips=100+14
n_optimal_trips=17+20
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.rmulet22_23otb=SumStats$values[1]-SumStats$values[2]
Dif.rmulet22_23otb.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.rmulet22_23otb.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- NETS (Metier Lvl 4) Red mullet GSA 23 --------
N.baseline=115; #N of samples 
N.optimal=471 #N of fish, proposed sampling plan for the RSP 2019
n_baseline_trips=14
n_optimal_trips=53
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=200
Uc_trip_optimal=200
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.rmulet23nets=SumStats$values[1]-SumStats$values[2]
Dif.rmulet23nets.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.rmulet23nets.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#Compiling results
Species=c("HKE 22otb","HKE 22nets","MUT 22-23otb","MUT 23nets")

Dif=as.numeric(c(Dif.hke22_23otb,Dif.hke22_23nets,Dif.rmulet22_23otb,Dif.rmulet23nets))
Dif.sd=as.numeric(c(Dif.hke22_23otb.sd,Dif.hke22_23nets.sd,Dif.rmulet22_23otb.sd,Dif.rmulet23nets.sd))
Dif.pct=as.numeric(c(Dif.hke22_23otb.pct,Dif.hke22_23nets.pct,Dif.rmulet22_23otb.pct,Dif.rmulet23nets.pct))
Results=data.frame(Species,Dif,Dif.sd,Dif.pct)

ggplot(Results, aes(x=Species, y=Dif*-1,colour=Species))+
  geom_errorbar(aes(ymin=(Dif*-1)-1.96*Dif.sd, ymax=(Dif*-1)+1.96*Dif.sd),width=.1)+
  geom_point()+ylab("Cost Difference (€)")+xlab("Species, Gear & GSA")+
  geom_hline(aes(yintercept=0),color="black", linetype="dashed")+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=10,face="plain"))+theme(legend.position="none")

#ggsave("HKE-MUT.tiff",path = 'C:/Projects/STREAM_MARE/WP3_T34', width = 14.4, height = 8.32, units = "cm", dpi = 500)
ggsave("HKE-MUT.png",path = 'C:/Projects/STREAM_MARE/WP3_T34', width = 14.4, height = 8.32, units = "cm", dpi = 500)

# GSA 25 Cyprus -------------------- Cyprus 

library(Rmisc)
library(ggplot2)
library(reshape2)
rm(list=ls(all=TRUE))
# Parameter setting
set.seed(124)
k=100 #Number of simulations

#------- OTB Red mullet GSA 25 --------
N.baseline=443; #N of samples 2016
N.optimal=1530 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=44
n_optimal_trips=18
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.rmulet25otb=SumStats$values[1]-SumStats$values[2]
Dif.rmulet25otb.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.rmulet25otb.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- NETS (Metier Lvl 4) Red mullet GSA 25 --------
N.baseline=1008; #N of samples 2016 
N.optimal=1188 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=12
n_optimal_trips=66
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=200
Uc_trip_optimal=200
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.rmulet25nets=SumStats$values[1]-SumStats$values[2]
Dif.rmulet25nets.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.rmulet25nets.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- OTM_MPD Red mullet GSA 29 --------
N.baseline=703; #N Baseline
N.optimal=1140 #N of fish, proposed sampling plan for the RSP 2019
n_baseline_trips=8
n_optimal_trips=19
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)  
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=35; #-- reduced by 30% (from 50)
DailyRate.mean=60; #-- reduced by 20% (from 75)
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.rmulet29otm_mpd=SumStats$values[1]-SumStats$values[2]
Dif.rmulet29otm_mpd.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.rmulet29otm_mpd.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#Compiling results
Species=c("MUT 25otb","MUT 25nets","MUT 29otm_mpd")

Dif=as.numeric(c(Dif.rmulet25otb,Dif.rmulet25nets,Dif.rmulet29otm_mpd))
Dif.sd=as.numeric(c(Dif.rmulet25otb.sd,Dif.rmulet25nets.sd,Dif.rmulet29otm_mpd.sd))
Dif.pct=as.numeric(c(Dif.rmulet25otb.pct,Dif.rmulet25nets.pct,Dif.rmulet29otm_mpd.pct))
Results=data.frame(Species,Dif,Dif.sd,Dif.pct)

ggplot(Results, aes(x=Species, y=Dif*-1,colour=Species))+
  geom_errorbar(aes(ymin=(Dif*-1)-1.96*Dif.sd, ymax=(Dif*-1)+1.96*Dif.sd),width=.1)+
  geom_point()+ylab("Cost Difference (€)")+xlab("Species, Gear & GSA")+
  geom_hline(aes(yintercept=0),color="black", linetype="dashed")+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=10,face="plain"))+theme(legend.position="none")

#ggsave("MUT.tiff",path = 'C:/Projects/STREAM_MARE/WP3_T34', width = 14.4, height = 8.32, units = "cm", dpi = 500)
ggsave("MUT.png",path = 'C:/Projects/STREAM_MARE/WP3_T34', width = 14.4, height = 8.32, units = "cm", dpi = 500)

# ----  ARISFOL GSAs 9, 10, 11 -----------------
library(Rmisc)
library(ggplot2)
library(reshape2)
rm(list=ls(all=TRUE))
# Parameter setting
set.seed(124)
k=100 #Number of simulations

# GSA 9 ARISFOL OTB_DWS

N.baseline=29; #N of samples 2016
N.optimal=1080 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=6
n_optimal_trips=12
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}


# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.afol9otb_dws=SumStats$values[1]-SumStats$values[2]
Dif.afol9otb_dws.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.afol9otb_dws.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]


# GSA 9 ARISFOL OTB_MDD

N.baseline=975; #N of samples 2016
N.optimal=1440 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=9
n_optimal_trips=16
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}


# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.afol9otb_mdd=SumStats$values[1]-SumStats$values[2]
Dif.afol9otb_mdd.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.afol9otb_mdd.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- ARISFOL GSA 10 OTB_DWS --------
N.baseline=2729; #N of samples 2016
N.optimal=5145 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=11
n_optimal_trips=21
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.afol10otb_dws=SumStats$values[1]-SumStats$values[2]
Dif.afol10otb_dws.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.afol10otb_dws.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- OTB MDD ARISFOL GSA 10 --------
N.baseline=2832; #N of samples 2016
N.optimal=2450 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=5
n_optimal_trips=10
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.afol10otb_mdd=SumStats$values[1]-SumStats$values[2]
Dif.afol10otb_mdd.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.afol10otb_mdd.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- OTB DWS ARISFOL GSA 11 --------
N.baseline=1450; #N of samples 2016
N.optimal=1365 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=8
n_optimal_trips=13
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.afol11otb_dws=SumStats$values[1]-SumStats$values[2]
Dif.afol11otb_dws.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.afol11otb_dws.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- OTB MDD ARISFOL GSA 11 --------
N.baseline=1431; #N of samples 2016
N.optimal=1365 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=8
n_optimal_trips=13
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.afol11otb_mdd=SumStats$values[1]-SumStats$values[2]
Dif.afol11otb_mdd.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.afol11otb_mdd.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#Compiling results
Species=c("ARS 9otb_dws","ARS 9otb_mdd","ARS 10otb_dws","ARS 10otb_mdd","ARS 11otb_dws","ARS 11otb_mdd")

Dif=as.numeric(c(Dif.afol9otb_dws,Dif.afol9otb_mdd,Dif.afol10otb_dws,Dif.afol10otb_mdd,Dif.afol11otb_dws, Dif.afol11otb_mdd))
Dif.sd=as.numeric(c(Dif.afol9otb_dws.sd,Dif.afol9otb_mdd.sd,Dif.afol10otb_dws.sd,Dif.afol10otb_mdd.sd,Dif.afol11otb_dws.sd, Dif.afol11otb_mdd.sd))
Dif.pct=as.numeric(c(Dif.afol9otb_dws.pct,Dif.afol9otb_mdd.pct,Dif.afol10otb_dws.pct,Dif.afol10otb_mdd.pct,Dif.afol11otb_dws.pct, Dif.afol11otb_mdd.pct))
Results=data.frame(Species,Dif,Dif.sd,Dif.pct)

ggplot(Results, aes(x=Species, y=Dif*-1,colour=Species))+
  geom_errorbar(aes(ymin=(Dif*-1)-1.96*Dif.sd, ymax=(Dif*-1)+1.96*Dif.sd),width=.1)+
  geom_point()+ylab("Cost Difference (€)")+xlab("Species, Gear & GSA")+
  geom_hline(aes(yintercept=0),color="black", linetype="dashed")+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=10,face="plain"))+theme(legend.position="none")

#ggsave("ARS.tiff",path = 'C:/Projects/STREAM_MARE/WP3_T34', width = 14.4, height = 8.32, units = "cm", dpi = 500)
ggsave("ARS.png",path = 'C:/Projects/STREAM_MARE/WP3_T34', width = 14.4, height = 8.32, units = "cm", dpi = 500)

# ----  ARITANT GSAs 9, 10, 11 -----------------

library(Rmisc)
library(ggplot2)
library(reshape2)
rm(list=ls(all=TRUE))
# Parameter setting
set.seed(124)
k=100 #Number of simulations

# GSA 9 ARITANT OTB_DWS

N.baseline=2645; #N of samples 2016
N.optimal=2640 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=6
n_optimal_trips=12
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.Aant9otb_dws=SumStats$values[1]-SumStats$values[2]
Dif.Aant9otb_dws.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.Aant9otb_dws.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

# GSA 9 ARITANT OTB_MDD

N.baseline=1980; #N of samples 2016
N.optimal=1980 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=9
n_optimal_trips=9
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.Aant9otb_mdd=SumStats$values[1]-SumStats$values[2]
Dif.Aant9otb_mdd.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.Aant9otb_mdd.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

# GSA 10 ARITANT OTB_MDD

N.baseline=226; #N of samples 2016
N.optimal=1550 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=5
n_optimal_trips=10
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.Aant10otb_mdd=SumStats$values[1]-SumStats$values[2]
Dif.Aant10otb_mdd.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.Aant10otb_mdd.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- ARITANT GSA 10 OTB_DWS --------
N.baseline=2729; #N of samples 2016
N.optimal=3255 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=11
n_optimal_trips=21
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.Aant10otb_dws=SumStats$values[1]-SumStats$values[2]
Dif.Aant10otb_dws.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.Aant10otb_dws.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- OTB DWS ARITANT GSA 11 --------
N.baseline=2167; #N of samples 2016
N.optimal=2730 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=8
n_optimal_trips=13
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.Aant11otb_dws=SumStats$values[1]-SumStats$values[2]
Dif.Aant11otb_dws.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.Aant11otb_dws.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#------- OTB MDD ARITANT GSA 11 --------
N.baseline=2188; #N of samples 2016
N.optimal=2730 #N of samples, proposed sampling plan for the RSP 2019
n_baseline_trips=8
n_optimal_trips=13
p_trip_change=0 # % adjustment to the number of optimal trips, valid values between 0 and 1
# 0=no adjusment; negative value=reducing the N of trips; positive value=increasing the N of trips
# example: p_trip_change=0 20% reduction of the optimal N of trips 
n_optimal_trips=n_optimal_trips+(n_optimal_trips*p_trip_change)
Nu.baseline=N.baseline/n_baseline_trips;
Nu.optimal=N.optimal/n_optimal_trips
Sample.mean.cost=50; 
DailyRate.mean=75; 
CV.sample.cost=0.1;
CV.daily.rate=0.2
Uc=rnorm(k,Sample.mean.cost,CV.sample.cost*Sample.mean.cost) 
Dr=rnorm(k,DailyRate.mean,CV.daily.rate*DailyRate.mean)
Uc_trip_baseline=400
Uc_trip_optimal=400
Uc_trip<-c(Uc_trip_baseline, Uc_trip_optimal)
Nu_trip<-c(n_baseline_trips, n_optimal_trips)
Pd=0.5

# Function Creation
Tc=function(Uc,Nu,Pd,Dr,Uc_trip,Nu_trip){
  (Uc*Nu+Uc_trip*Nu_trip)+Dr*(40+Nu*Pd)
}

# Simulations
Tcosts=matrix(data=NA,nrow=k,ncol=2)
colnames(Tcosts)=c("TcOld","TcNew")
i=1

for (Nu in c(Nu.baseline,Nu.optimal)){
  Tcosts[,i]=Tc(Uc,Nu,Pd,Dr,Uc_trip[i],Nu_trip[i])
  i=i+1
}

# Results' processing & presentation
Tcosts=data.frame(Tcosts);TC=stack(Tcosts)
TC$Sample_effort <- factor(TC$ind)
SumStats=summarySE(TC,measurevar="values",groupvars="Sample_effort")
SumStats$cv=SumStats$sd/SumStats$values
Dif.Aant11otb_mdd=SumStats$values[1]-SumStats$values[2]
Dif.Aant11otb_mdd.sd=SumStats$sd[1]-SumStats$sd[2]
Dif.Aant11otb_mdd.pct=(SumStats$values[1]-SumStats$values[2])*100/SumStats$values[2]

#Compiling results
Species=c("ARA 9otb_dws","ARA 9otb_mdd","ARA 10otb_dws","ARA 10otb_mdd","ARA 11otb_dws","ARA 11otb_mdd")

Dif=as.numeric(c(Dif.Aant9otb_dws,Dif.Aant9otb_mdd,Dif.Aant10otb_dws,Dif.Aant10otb_mdd,Dif.Aant11otb_dws, Dif.Aant11otb_mdd))
Dif.sd=as.numeric(c(Dif.Aant9otb_dws.sd,Dif.Aant9otb_mdd.sd,Dif.Aant10otb_dws.sd,Dif.Aant10otb_mdd.sd,Dif.Aant11otb_dws.sd, Dif.Aant11otb_mdd.sd))
Dif.pct=as.numeric(c(Dif.Aant9otb_dws.pct,Dif.Aant9otb_mdd.pct,Dif.Aant10otb_dws.pct,Dif.Aant10otb_mdd.pct,Dif.Aant11otb_dws.pct, Dif.Aant11otb_mdd.pct))

Results=data.frame(Species,Dif,Dif.sd,Dif.pct)

ggplot(Results, aes(x=Species, y=Dif*-1,colour=Species))+
  geom_errorbar(aes(ymin=(Dif*-1)-1.96*Dif.sd, ymax=(Dif*-1)+1.96*Dif.sd),width=.1)+
  geom_point()+ylab("Cost Difference (€)")+xlab("Species, Gear & GSA")+
  geom_hline(aes(yintercept=0),color="black", linetype="dashed")+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=10,face="plain"))+theme(legend.position="none")

#ggsave("ARA.tiff",path = 'C:/Projects/STREAM_MARE/WP3_T34', width = 14.4, height = 8.32, units = "cm", dpi = 500)
ggsave("ARA.png",path = 'C:/Projects/STREAM_MARE/WP3_T34', width = 14.4, height = 8.32, units = "cm", dpi = 500)