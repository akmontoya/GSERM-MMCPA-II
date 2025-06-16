
#set your directory to where you have stored the course csv files
setwd("c:/mmcpa")


#==============================


#read the teams data
teams<-read.table("teams.csv", sep=",",header=TRUE)
head(teams)

#the total effect of dysfunctional behavior on team performance
summary(lm(perform~dysfunc,data=teams))

#mediation analysis using PROCESS
process(data=teams,y="perform",x="dysfunc",m="negtone",model=4,total=1,
   boot=10000,seed=3341)

#moderation analysis using PROCESS
process(data=teams,y="perform",x="negtone",w="negexp",plot=1,jn=1,model=1)

#visualizing the model
x<-c(-.450,-.035,.522,-.450,-.035,.522,-.450,-.035,.522)
w<-c(-.531,-.531,-.531,-.060,-.060,-.060,.600,.600,.600)
y<-c(-.000,.004,.009,.121,.007,-.146,.291,.012,-.362)
wmarker<-c(15,15,15,16,16,16,17,17,17)
plot(y=y,x=x,cex=1.2,pch=wmarker,
xlab="Negativity of work climate",
ylab="Team performance",col=c(2,2,2,1,1,1,4,4,4))
legend.txt<-c("Low expressive",
"Moderate expressive", "High expressive")
legend("bottomleft", legend = legend.txt,cex=1,
lty=c(1,3,6),lwd=c(2,3,2),pch=c(15,16,17),
col=c("red","black","blue"))
lines(x[w==-.531],y[w==-.531],lwd=2,col="red")
lines(x[w==-.060],y[w==-.060],lwd=3,lty=3,col="black")
lines(x[w==.600],y[w==.600],lwd=2,lty=6,col="blue")

#conditional process analysis using PROCESS
process (data=teams,x="dysfunc",m="negtone",y="perform",w="negexp",plot=1,
   model=14,seed=61326)

#visualizing the second stage of the model
x<-c(-.450,-.035,.522,-.450,-.035,.522,-.450,-.035,.522)
w<-c(-.531,-.531,-.531,-.060,-.060,-.060,.600,.600,.600)
y<-c(.084,.017,-.073,.184,.016,-.210,.325,.015,-.400)
wmarker<-c(15,15,15,16,16,16,17,17,17)
plot(y=y,x=x,cex=1.2,pch=wmarker,
xlab="Negativity of work climate",
ylab="Team performance",col=c(2,2,2,1,1,1,4,4,4))
legend.txt<-c("Low expressive","Moderate expressive", "High expressive")
legend("bottomleft", legend = legend.txt,cex=1,lty=c(1,3,6),lwd=c(2,3,2),
pch=c(15,16,17),col=c("red","black","blue"))
lines(x[w==-.531],y[w==-.531],lwd=2,col="red")
lines(x[w==-.060],y[w==-.060],lwd=3,lty=3,col="black")
lines(x[w==.600],y[w==.600],lwd=2,lty=6,col="blue")

#visualizing the direct and indirect effects
x<-c(0,1,0,1,0,1)
w<-c(-0.531,-0.531,-0.060,-0.060,0.600,0.600)
y<-c(0.366,-0.100,0.366,-0.251,0.366,-0.462)
plot(y=y,x=w,pch=15,col="white",
xlab="Nonverbal negative expressivity",
ylab="Effect of dysfunctional team behavior")
legend.txt<-c("Direct effect","Indirect effect")
legend("bottomleft",legend=legend.txt,lty=c(1,3),lwd=c(4,3),
col=c("black","red"))
lines(w[x==0],y[x==0],lwd=4,lty=1,col="black")
lines(w[x==1],y[x==1],lwd=4,lty=3,col="red")
abline(0,0,lwd=0.5,lty=2)


#==============================
#Module 1

#read the mentor data
mentor<-read.table("mentor.csv", sep=",",header=TRUE)
head(mentor)

#Estimate the parallel mediation model with PROCESS
process(data=mentor,y="conflict",x="mentor",m=c("resource", "workload"),
   total=1,model=4,seed=92612)

#Estimate the conditional process model with PROCESS
process(data=mentor,y="conflict",x="mentor",m=c("resource","workload"),w="wforient",
   plot=1,model=7,seed=8231)

#visualize the model of resources
x<-c(3.1,3.7,4.3,3.1,3.7,4.3,3.1,3.7,4.3)
w<-c(2.3,2.3,2.3,3.1,3.1,3.1,3.8,3.8,3.8)
y<-c(2.866,2.858,2.850,3.259,3.376,3.493,3.603,3.830,4.056)
wmarker<-c(15,15,15,16,16,16,17,17,17)
plot(y=y,x=x,cex=1.2,pch=wmarker,
xlab="Extent of formal mentoring (X)",
ylab="Access to job-related resources",col=c("purple","purple","purple",1,1,1,4,4,4))
legend.txt<-c("Family-focused","Blended identity", "Work-focused")
legend("topleft", legend = legend.txt,cex=1,lty=c(1,3,6),lwd=c(2,3,2),
pch=c(15,16,17),col=c("purple","black","blue"))
lines(x[w==2.3],y[w==2.3],lwd=2,col="purple")
lines(x[w==3.1],y[w==3.1],lwd=3,lty=3,col="black")
lines(x[w==3.8],y[w==3.8],lwd=2,lty=6,col="blue")

#visualize the model of workload
x<-c(3.1,3.7,4.3,3.1,3.7,4.3,3.1,3.7,4.3)
w<-c(2.3,2.3,2.3,3.1,3.1,3.1,3.8,3.8,3.8)
y<-c(3.332,3.725,4.118,3.033,3.291,3.549,2.771,2.911,3.052)
wmarker<-c(15,15,15,16,16,16,17,17,17)
plot(y=y,x=x,cex=1.2,pch=wmarker,
xlab="Extent of formal mentoring (X)",
ylab="Workload",col=c("purple","purple","purple",1,1,1,4,4,4))
legend.txt<-c("Family-focused","Blended identity", "Work-focused")
legend("topleft", legend = legend.txt,cex=1,lty=c(1,3,6),lwd=c(2,3,2),
pch=c(15,16,17),col=c("purple","black","blue"))
lines(x[w==2.3],y[w==2.3],lwd=2,col="purple")
lines(x[w==3.1],y[w==3.1],lwd=3,lty=3,col="black")
lines(x[w==3.8],y[w==3.8],lwd=2,lty=6,col="blue")


#==============================
#Module 2

#read the binladen data
binladen<-read.table("binladen.csv", sep=",",header=TRUE)
head(binladen)

#estimating the model using PROCESS
process(data=binladen,y="mcivil",x="binladen",m=c("stereo","rthreat"),
  cov=c("sex","age","ideo"),model=6,total=1,seed=6234)

#customizing the assignment of covariates to equations
process(data=binladen,y="mcivil",x="binladen",m=c("stereo","rthreat"),
   cov=c("sex","age","ideo"),model=6,seed=6234,cmatrix=c(0,1,1,1,1,1,0,0,1));


#==============================
#Module 3

#read the binladen data
binladen<-read.table("binladen.csv", sep=",",header=TRUE)
head(binladen)

#estimating the model using PROCESS
process(data=binladen,y="mcivil",x="binladen",w="age",m=c("stereo","rthreat"),
    cov=c("sex","ideo"),model=85,plot=1,seed=63234)

#estimating the model using PROCESS model 83 instead
process(data=binladen,y="mcivil",x="binladen",m=c("stereo","rthreat"),
  w="age",cov=c("sex","ideo"),model=83,plot=1,seed=63234)

#estimating the model using PROCESS model 83 instead, brining age back as a covariate
process(data=binladen,y="mcivil",x="binladen",m=c("stereo","rthreat"),w="age",cov=c("age",
  "sex","ideo"),model=83,plot=1,seed=63234)


#==============================
#Module 4

#read the lawyer2 data
lawyer2<-read.table("lawyer2.csv", sep=",",header=TRUE)
head(lawyer2)


#show group means and do single factor ANOVA
aggregate(lawyer2$eval,list(lawyer2$protest),mean)
protest.f<-factor(lawyer2$protest)
summary(aov(eval~protest.f,data=lawyer2))

#frequency table for condition
table(lawyer2$protest)

#create dummy codes for groups, save to new data frame called lawyer2.dummy
d1<-as.numeric(lawyer2$protest==1);d2<-as.numeric(lawyer2$protest==2)
lawyer2.dummy<-data.frame(lawyer2,d1,d2)

#Regress evaluation on experimental condition
summary(lm(eval~d1+d2,data=lawyer2.dummy))


#read the lawyer2 data
lawyer2<-read.table("lawyer2.csv", sep=",",header=TRUE)
head(lawyer2)

#Create dummy codes with no protest group as reference
#and save to a new lawyer2.dummy data frame
d1<-as.numeric(lawyer2$protest==1)
d2<-as.numeric(lawyer2$protest==2)
lawyer2.dummy<-data.frame(lawyer2,d1,d2)

#estimate the relative total effects (c1 and c2)
summary(lm(eval~d1+d2,data=lawyer2.dummy))

#estimate a1 and a2
summary(lm(approp~d1+d2,data=lawyer2.dummy))

#estimate b and the relative direct effects (c'1 and c'2)
summary(lm(eval~approp+d1+d2,data=lawyer2.dummy))

#Do the mediation analysis with PROCESS
process(data=lawyer2,y="eval",m="approp",x="protest",mcx=1,model=4,total=1,seed=61235)

#create Helmert codes and save as data frame lawyer2.helmert
d1<-(lawyer2$protest==0)*(-2/3)+(lawyer2$protest>0)*(1/3)
d2<-(lawyer2$protest==1)*(-1/2)+(lawyer2$protest==2)*(1/2)
lawyer2.helmert<-data.frame(lawyer2,d1,d2)

#estimate the relative total effects (c1 and c2)
summary(lm(eval~d1+d2,data=lawyer2.helmert))

#estimate a1 and a2
summary(lm(approp~d1+d2,data=lawyer2.helmert))

#estimate b and the relative direct effects (c'1 and c'2)
summary(lm(eval~approp+d1+d2,data=lawyer2.helmert))

#Do the mediation analysis with PROCESS
process(data=lawyer2,y="eval",m="approp",x="protest",mcx=3,model=4,total=1,seed=61235)

#==============================
#Module 5

lawyer2<-read.table("lawyer2.csv", sep=",",header=TRUE)
head(lawyer2)

#create indicator codes and estimate unconditional effect model
d1<-as.numeric(lawyer2$protest==1)
d2<-as.numeric(lawyer2$protest==2)
lawyer2.dummy<-data.frame(lawyer2,d1,d2)
summary(lm(eval~d1+d2+sexism,data=lawyer2.dummy))

#estimate moderation model
summary(lm(eval~d1+d2+sexism+d1*sexism+d2*sexism,data=lawyer2.dummy))

#visualize the model
x<-c(0,1,2,0,1,2,0,1,2)
w<-c(4,4,4,5,5,5,6,6,6)
d1<-(x==1);
d2<-(x==2);
y<-7.706-4.129*d1-3.491*d2-0.472*w+0.901*d1*w+0.778*d2*w
plot(y=y,x=w,pch=15,col="white",xlab="Perceived pervasiveness of sex
discrimination (W)",ylab="Evaluation of the attorney (Y)")
legend.txt<-c("No protest","Individual protest","Collective protest")
legend("topleft",legend=legend.txt,lty=c(1,1,3),lwd=c(4,1,4),
col=c("black","red","blue"))
lines(w[x==0],y[x==0],lwd=4,lty=1,col="black")
lines(w[x==1],y[x==1],lwd=1,lty=1,col="red")
lines(w[x==2],y[x==2],lwd=4,lty=3,col="blue")

#Test of interaction between X and W
model1<-lm(eval~d1+d2+sexism,data=lawyer2.dummy)
model2<-lm(eval~d1+d2+sexism+d1*sexism+d2*sexism,data=lawyer2.dummy)
anova(model1,model2)

#estimate and probe using PROCESS instead
process(data=lawyer2,y="eval",w="sexism",x="protest",model=1,mcx=1,plot=1)

#visualize using output from the PROCESS plot option
x<-c(0,1,2,0,1,2,0,1,2)
w<-c(4.25,4.25,4.25,5.12,5.12,5.12,5.896,5.896,5.896)
y<-c(5.698,5.400,5.513,5.287,5.773,5.779,4.920,6.105,6.016)
plot(y=y,x=w,pch=15,col="white",xlab="Perceived pervasiveness of sex
discrimination (W)",ylab="Evaluation of the attorney (Y)")
legend.txt<-c("No protest","Individual protest","Collective protest")
legend("topleft",legend=legend.txt,lty=c(1,1,3),lwd=c(4,1,4),
col=c("black","red","blue"))
lines(w[x==0],y[x==0],lwd=4,lty=1,col="black")
lines(w[x==1],y[x==1],lwd=1,lty=1,col="red")
lines(w[x==2],y[x==2],lwd=4,lty=3,col="blue")

#get percentiles of the distribution of sexism
quantile(lawyer2$sexism, c(.16,.50,.84))

#probe by conditioning on moderator value of 4.25
sexismp<-lawyer2.dummy$sexism-4.25
sexpd1<-sexismp*lawyer2.dummy$d1;sexpd2<-sexismp*lawyer2.dummy$d2
lawyer2.dummy<-data.frame(lawyer2.dummy,sexismp,sexpd1,sexpd2)
model1<-lm(eval~sexismp+sexpd1+sexpd2,data=lawyer2.dummy)
summary(model1)
model2<-lm(eval~sexismp+d1+d2+sexpd1+sexpd2,data=lawyer2.dummy)
summary(model2)
anova(model1,model2)

#Moderation using PROCESS with a multicategorical moderator
process(data=lawyer2,y="eval",x="sexism",w="protest",model=1,mcw=1,plot=1)

#==============================
#Module 6

#read the lawyer2 data
lawyer2<-read.table("lawyer2.csv", sep=",",header=TRUE)
head(lawyer2)

#Do the conditional process analysis with PROCESS
process(data=lawyer2,y="eval",x="protest",m="approp",w="sexism",
  model=8,plot=1,mcx=1,seed=193456)

#Visualize the first stage model
x<-c(0,1,2,0,1,2,0,1,2)
w<-c(4.25,4.25,4.25,5.12,5.12,5.12,5.896,5.896,5.896)
m<-c(4.318,4.699,5.333,3.858,5.089,5.512,3.448,5.438,5.671)
plot(y=m,x=w,pch=15,col="white",ylim=c(3,6.5),
xlab="Perceived pervasiveness of sex discrimination (W)",
ylab="Appropriateness of the response (M)")
legend.txt<-c("No protest","Individual protest","Collective protest")
legend("topleft",legend=legend.txt,lty=c(1,1,3),lwd=c(4,1,4),
col=c("black","red","blue"))
lines(w[x==0],m[x==0],lwd=4,lty=1,col="black")
lines(w[x==1],m[x==1],lwd=1,lty=1,col="red")
lines(w[x==2],m[x==2],lwd=4,lty=3,col="blue")

#Visualize the model of Y
x<-c(0,1,2,0,1,2,0,1,2)
w<-c(4.25,4.25,4.25,5.12,5.12,5.12,5.896,5.896,5.896)
y<-c(5.899,5.461,5.342,5.657,5.691,5.542,5.441,5.896,5.721)
plot(y=y,x=w,pch=15,col="white",ylim=c(1,6.5),
xlab="Perceived pervasiveness of sex discrimination (W)",
ylab="Evaluation of the attorney (Y)")
legend.txt<-c("No protest","Individual protest","Collective protest")
legend("bottomleft",legend=legend.txt,lty=c(1,1,3),lwd=c(4,1,4),
col=c("black","red","blue"))
lines(w[x==0],y[x==0],lwd=4,lty=1,col="black")
lines(w[x==1],y[x==1],lwd=1,lty=1,col="red")
lines(w[x==2],y[x==2],lwd=4,lty=3,col="blue")


#==============================
#Module 7

#read the math data
math<-read.table("math.csv", sep=",",header=TRUE)
head(math)

#Is there are stereotype threat effect?
summary(lm(mathprob~threat,data=math))

#Correlations between math self-concept and performance
cor(math)

#Control for math self-concept
summary(lm(mathprob~threat+explms+implms,data=math))

#Do the moderation analysis
summary(lm(mathprob~threat+explms+implms+threat*explms+threat*implms,data=math))

#estimate the moderation model using PROCESS
process(data=math,y="mathprob",x="threat",w="explms",z="implms",model=2,plot=1);

#visualize the model
oldp<-par(mfrow=c(3,1),mar=c(3,4,0,0),oma=c(2,2,2,2),mgp=c(5,0.5,0))
w<-c(13,16,17,13,16,17)
x<-c(0,0,0,1,1,1)
yzlow<-c(6.039,8.242,8.977,5.802,7.173,7.630)
yzmod<-c(6.516,8.719,9.454,6.917,8.288,8.745)
yzhigh<-c(7.023,9.226,9.960,8.101,9.472,9.929)
wt<-x
x<-w
w<-wt
legend.txt<-c("No threat (X=0)","Threat (X=1)")
for (i in 1:3){
if (i==1)
  {y<-yzlow
  legend2.txt<-c("Implicit Math Self-Concept (Z) = -0.882")}
if (i==2)
  {y<-yzmod
  legend2.txt<-c("Implicit Math Self-Concept (Z) = -0.570")}
if (i==3)
  {y<-yzhigh
  legend2.txt<-c("Implicit Math Self-Concept (Z) = -0.239")}
plot(y=y,x=x,col="white",ylim=c(5,10),cex=1.5,xlim=c(13,17),tcl=-0.5)
lines(x[w==0],y[w==0],lwd=2,lty=1,col="blue")
lines(x[w==1],y[w==1],lwd=2,lty=2,col="red")
legend("topleft", legend=legend.txt,lwd=2,lty=c(1,2),
col=c("blue","red"))
legend("bottomright",legend=legend2.txt)
box}
mtext("Explicit Math Self-Concept (W)",side=1,outer=TRUE)
mtext("Math Performance",side=2,outer=TRUE)
par(oldp)


#Test the set of products
model1<-(lm(mathprob~threat+explms+implms,data=math))
model2<-(lm(mathprob~threat+explms+implms+threat*explms+threat*implms,data=math))
anova(model1,model2)


#Test stereotype threat effect in "fragile" math self-concept women
explms_c<-math$explms-17;implms_c<-math$implms-(-0.882);
summary(lm(mathprob~threat+explms_c+implms_c+threat*explms_c+threat*implms_c,data=math))

#Compare two conditional effects of X using PROCESS
process(data=math,y="mathprob",x="threat",w="explms",z="implms",model=2,contrast=c(17,-0.8816,13,-0.2392))


#---
#read the math data
math<-read.table("math.csv", sep=",",header=TRUE)
head(math)

#estimate moderated moderation model
summary(lm(mathprob~threat+explms+implms+threat*explms+threat*implms+explms*implms+threat*explms*implms,data=math))

#estimate the moderated moderation model using PROCESS
process(data=math,y="mathprob",x="threat",w="explms",z="implms",model=3,plot=1,jn=1)

#visualize the model
oldp<-par(mfrow=c(3,1),mar=c(3,4,0,0),oma=c(2,2,2,2),mgp=c(5,0.5,0))
w<-c(13,16,17,13,16,17)
x<-c(0,0,0,1,1,1)
yzlow<-c(5.921,8.326,9.128,6.484,7.176,7.407)
yzmod<-c(6.530,8.746,9.485,6.649,8.051,8.518)
yzhigh<-c(7.177,9.193,9.865,6.824,8.979,9.698)
wt<-x
x<-w
w<-wt
legend.txt<-c("No threat (X=0)","Threat (X=1)")
for (i in 1:3){
if (i==1)
  {y<-yzlow
  legend2.txt<-c("Implicit Math Self-Concept (Z) = -0.882")}
if (i==2)
  {y<-yzmod
  legend2.txt<-c("Implicit Math Self-Concept (Z) = -0.570")}
if (i==3)
  {y<-yzhigh
  legend2.txt<-c("Implicit Math Self-Concept (Z) = -0.239")}
plot(y=y,x=x,col="white",ylim=c(5,10),cex=1.5,xlim=c(13,17),tcl=-0.5)
lines(x[w==0],y[w==0],lwd=2,lty=1,col="blue")
lines(x[w==1],y[w==1],lwd=2,lty=2,col="red")
legend("topleft", legend=legend.txt,lwd=2,lty=c(1,2),
col=c("blue","red"))
legend("bottomright",legend=legend2.txt)
box}
mtext("Explicit Math Self-Concept (W)",side=1,outer=TRUE)
mtext("Math Performance",side=2,outer=TRUE)
par(oldp)


#estimate conditional two-way threat by explicit interaction when implicit=high
implms_p<-math$implms-(-0.239)
summary(lm(mathprob~threat+explms+implms_p+threat*explms+threat*implms_p+explms*implms_p+threat*explms*implms_p,data=math))

#estimate stereotype threat effect among women with "fragile" math self-concept
explms_p<-math$explms-17;implms_p<-math$implms-(-0.882)
summary(lm(mathprob~threat+explms_p+implms_p+threat*explms_p+threat*implms_p+explms_p*implms_p+threat*explms_p*implms_p,data=math))


#==============================
#Module 8

#No R code for this module

#==============================
#Module 9

#read the gaza data
gaza<-read.table("gaza.csv", sep=",",header=TRUE)
head(gaza)

#Do the conditional process analysis with PROCESS
process(data=gaza,y="depress",x="trauma",m="frqual",w="age",z="sex",moments=1,plot=1,
  model=9,seed=145)

#Visualize the model of friendship quality
oldp<-par(mfrow=c(2,1),mar=c(3,4,0,0),oma=c(2,2,2,2),
mgp=c(5,0.5,0))
x<-c(3.099,7.255,11.419,3.099,7.255,11.419,3.099,7.255,11.419)
w<-c(10.210,10.210,10.210,11.346,11.346,11.346,12.482,12.482,12.482)
yz0<-c(4.080,3.786,3.491,4.254,3.824,3.393,4.428,3.862,3.295)
yz1<-c(3.544,3.756,3.968,3.718,3.793,3.870,3.892,3.832,3.772)
legend.txt<-c("10.210","11.346","12.482")
for (i in 1:2){
if (i==1)
  {y<-yz0
  legend2.txt<-c("Female, Z=0")}
if (i==2)
  {y<-yz1
  legend2.txt<-c("Male, Z=1")}
plot(y=y,x=x,col="white",ylim=c(1,5),cex=1.5,xlim=c(2,12),tcl=-0.5)
lines(x[w==10.210],y[w==10.210],lwd=2,lty=3,col="blue")
lines(x[w==11.346],y[w==11.346],lwd=2,lty=2,col="red")
lines(x[w==12.482],y[w==12.482],lwd=2,lty=1,col="black")
legend("bottomleft", legend=legend.txt,lwd=2,lty=c(3,2,1),
col=c("blue","red","black"))
legend("bottomright",legend=legend2.txt)
box}
mtext("Trauma (X)",side=1,outer=TRUE)
mtext("Friendship Quality (M)",side=2,outer=TRUE)
par<-oldp



#==============================
#Module 10

#read the injuries data
injuries<-read.table("injuries.csv", sep=",",header=TRUE)
head(injuries)

#Do the conditional process analysis with PROCESS
process(data=injuries,y="injury",x="exhaust",m="safety",w="tenure",z="sex",cov="injuryb",
   moments=1,plot=1,model=21,seed=456)

#Visualize the first stage part of the model
x<-c(2.369,3.362,4.355,2.369,3.362,4.355,2.369,3.362,4.355)
w<-c(2.240,2.240,2.240,5.513,5.513,5.513,8.786,8.786,8.786)
m<-c(4.015,4.501,4.987,3.797,4.086,4.376,3.580,3.672,3.764)
wmarker<-c(15,15,15,16,16,16,17,17,17)
plot(y=m,x=x,cex=1.2,pch=wmarker,
xlab="Exhaustion (X)",
ylab="Safety protocol workarounds (M)",col=c(4,4,4,3,3,3,2,2,2))
legend.txt<-c("Low experience",
"Moderate experience", "High experience")
legend("topleft", legend = legend.txt,cex=1,
lty=c(3,1,6),lwd=c(2,3,2),pch=c(15,16,17),
col=c("blue","green","red"))
lines(x[w==2.240],m[w==2.240],lwd=3,lty=3,col="blue")
lines(x[w==5.513],m[w==5.513],lwd=3,col="green")
lines(x[w==8.786],m[w==8.786],lwd=2,lty=6,col="red")


#Visualize the second stage part of the model
z<-c(0,0,0,1,1,1)
m<-c(3.023,4.102,5.181,3.023,4.102,5.181)
y<-c(1.629,2.081,2.533,1.991,1.996,2.000)
plot(y=y,x=m,pch=15,col="white",
xlab="Use of safety protocol workarounds (M)",
ylab="Injuries (Y)")
legend.txt<-c("Female (Z=0)",
"Male (Z=1)")
legend("topleft",legend=legend.txt,
lty=c(1,3),lwd=c(3,2),col=c("blue","darkgreen"))
lines(m[z==0],y[z==0],lwd=3,lty=1,col="blue")
lines(m[z==1],y[z==1],lwd=2,lty=3,col="darkgreen")


#Reprogram model 21 to get the other indices of conditional moderated mediation
process(data=injuries,y="injury",x="exhaust",m="safety",z="tenure",w="sex",cov="injuryb",moments=1,
   seed=456,model=21,wmatrix=c(0,0,1),zmatrix=c(1,0,0))




