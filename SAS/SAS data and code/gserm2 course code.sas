

* ==============================;
* Review modules;
* First execute teams.sas to create the teams data file;

*the total effect of dysfunctional behavior on team performance;
proc reg data=teams;model perform=dysfunc;run;

*mediation analysis using PROCESS;
%process (data=teams,y=perform,x=dysfunc,m=negtone,model=4,total=1,boot=10000,seed=3341);

*moderation analysis using PROCESS;
%process (data=teams,y=perform,x=negtone,w=negexp,plot=1,jn=1,model=1);

*visualizing the model;
data;
input negtone negexp perform;
datalines;
     -.4500     -.5308     -.0001
     -.0350     -.5308      .0038
      .5224     -.5308      .0091
     -.4500     -.0600      .1212
     -.0350     -.0600      .0074
      .5224     -.0600     -.1456
     -.4500      .6000      .2913
     -.0350      .6000      .0124
      .5224      .6000     -.3623
run;
proc sgplot;reg x=negtone y=perform/group=negexp;run;

*conditional process analysis using PROCESS;
%process (data=teams,x=dysfunc,m=negtone,y=perform,w=negexp,plot=1,model=14,seed=61326);

*visualizing the second stage of the model;
data;
input negtone negexp perform;
cards;
     -.4500     -.5308      .0836
     -.0350     -.5308      .0166
      .5224     -.5308     -.0733
     -.4500     -.0600      .1841
     -.0350     -.0600      .0161
      .5224     -.0600     -.2095
     -.4500      .6000      .3250
     -.0350      .6000      .0154
      .5224      .6000     -.4004
run;
proc sgplot;reg x=negtone y=perform/group=negexp;run;


*==============================;
*Module 1;
* First execute mentor.sas to create the mentor data file;


*Estimate the parallel mediation model with PROCESS;
%process (data=mentor,y=conflict,x=mentor,m=resource workload,total=1,model=4,seed=92612);


*Estimate the conditional process model with PROCESS;
%process (data=mentor,y=conflict,x=mentor,m=resource workload,w=wforient,plot=1,model=7,seed=8231)

*visualize the model of resources;
data;
input mentor wforient resource;
cards;
     3.1000     2.3000     2.8664
     3.7000     2.3000     2.8582
     4.3000     2.3000     2.8501
     3.1000     3.1000     3.2592
     3.7000     3.1000     3.3763
     4.3000     3.1000     3.4933
     3.1000     3.8000     3.6029
     3.7000     3.8000     3.8296
     4.3000     3.8000     4.0562
run;
proc sgplot;reg x=mentor y=resource/group=wforient;run;


*visualize the model of workload;
data;
input mentor wforient workload;
cards;
     3.1000     2.3000     3.3321
     3.7000     2.3000     3.7251
     4.3000     2.3000     4.1182
     3.1000     3.1000     3.0328
     3.7000     3.1000     3.2911
     4.3000     3.1000     3.5494
     3.1000     3.8000     2.7710
     3.7000     3.8000     2.9113
     4.3000     3.8000     3.0516
run;
proc sgplot;reg x=mentor y=workload/group=wforient;run;



*==============================;
*Module 2;
* First execute binladen.sas to create the binladen data file;

*estimating the model using PROCESS;
%process (data=binladen,y=mcivil,x=binladen,m=stereo rthreat,cov=sex age ideo,model=6,total=1,seed=6234);

*customizing the assignment of covariates to equations;
%process (data=binladen,y=mcivil,x=binladen,m=stereo rthreat,
   cov=sex age ideo,model=6,seed=6234,cmatrix=0 1 1 1 1 1 0 0 1);



*==============================;
*Module 3;
* First execute binladen.sas to create the binladen data file;

*estimating the model using PROCESS;
%process (data=binladen,y=mcivil,x=binladen,m=stereo rthreat,w=age,
    cov=sex ideo,model=85,plot=1,seed=63234);

*estimating the model using PROCESS model 83 instead;
%process (data=binladen,y=mcivil,x=binladen,m=stereo rthreat,w=age,
    cov=sex ideo,model=83,plot=1,seed=63234);

*estimating the model using PROCESS model 83 instead, brining age back as a covariate;
%process (data=binladen,y=mcivil,x=binladen,m=stereo rthreat,w=age,cov=age sex ideo,model=83,
  plot=1,seed=63234);

*==============================;
*Module 4;
* First execute lawyer2.sas to create the lawyer2 data file;

*show group means and do single factor ANOVA;
proc anova data=lawyer2;
class protest;model eval=protest;means protest;run;


*frequency table for condition;
proc freq data=lawyer2;tables protest;run;


*Create dummies and regress evaluation on experimental condition;
*(no missing data on protest so this is ok);
data lawyer2;set lawyer2;
d1 = (protest=1);d2 = (protest=2);run;
proc reg data=lawyer2;model eval = d1 d2;run;


*estimate the relative total effects (c1 and c2);
*same code as above;
data lawyer2;set lawyer2;
d1 = (protest=1);d2 = (protest=2);run;
proc reg data=lawyer2;model eval = d1 d2;run;

*estimate a1 and a2;
proc reg data=lawyer2;model approp=d1 d2;run;


*estimate b and the relative direct effects (c'1 and c'2);
proc reg data=lawyer2;model eval=approp d1 d2;run;


*Do the mediation analysis with PROCESS;
%process (data=lawyer2,y=eval,m=approp,x=protest,mcx=1,model=4,total=1,seed=61235);

*create Helmert codes and estimate relative toal effects (c1 and c2);
data lawyer2;set lawyer2;if (protest = 0) then d1 = -2/3;
  if (protest > 0) then d1 = 1/3;if (protest = 0) then d2 = 0;
  if (protest = 1) then d2 = -1/2;if (protest = 2) then d2 = 1/2;
run;
proc reg data=lawyer2;model eval=d1 d2;run;

*estimate a1 and a2;
proc reg data=lawyer2;model approp=d1 d2;run;


*estimate b and the relative direct effects (c'1 and c'2);
proc reg data=lawyer2;model eval=approp d1 d2;run;

*Do the mediation analysis with PROCESS;
%process (data=lawyer2,y=eval,m=approp,x=protest,mcx=3,model=4,total=1,seed=61235);

*==============================;
*Module 5;
* First execute lawyer2.sas to create the lawyer2 data file;

*create indicator codes and estimate unconditional effect model;
data lawyer2;set lawyer2;
d1=(protest=1);d2=(protest=2);run;
proc reg;model eval = d1 d2 sexism;run;


*estimate moderation model;
data lawyer2;set lawyer2;
d1=(protest=1);d2=(protest=2);d1w=d1*sexism;d2w=d2*sexism;run;
proc reg data=lawyer2;model eval = d1 d2 sexism d1w d2w;run;


*visualize the model;
data;
input d1 d2 sexism protest;
yhat = 7.706-4.129*d1-3.491*d2-0.472*sexism+0.901*d1*sexism+0.778*d2*sexism;
cards;
0 0 4 0
0 0 5 0
0 0 6 0 
1 0 4 1
1 0 5 1
1 0 6 1
0 1 4 2
0 1 5 2
0 1 6 2
run;
proc sgplot;reg x=sexism y=yhat/group=protest;run;


*Test of interaction between X and W;
proc reg data=lawyer2;
model eval = d1 d2 d1w d2w sexism;
test d1w=0,d2w=0;
run;


*estimate and probe using PROCESS instead;
%process (data=lawyer2,y=eval,w=sexism,x=protest,model=1,mcx=1,plot=1);


*visualize using output from the PROCESS plot option;
data;
input protest sexism esteval;
datalines;
      .0000     4.2500     5.6981
     1.0000     4.2500     5.3996
     2.0000     4.2500     5.5131
      .0000     5.1200     5.2871
     1.0000     5.1200     5.7726
     2.0000     5.1200     5.7787
      .0000     5.8960     4.9204
     1.0000     5.8960     6.1053
     2.0000     5.8960     6.0156
run;
proc sgplot;reg x=sexism y=esteval/group=protest;run;

*get percentiles of the distribution of sexism;
proc univariate data=lawyer2;var sexism;
output out=outpdata pctlpts=16 50 84
pctlpre=p;
proc print data=outpdata;run;

*multicategorical moderator instead;
data lawyer2;set lawyer2;d1=(protest=1);d2=(protest=2);d1x=d1*sexism;d2x=d2*sexism;run;
proc reg data=lawyer2;model eval = d1 d2 sexism d1x d2x;run;


*probe by conditioning on moderator value of 4.25;
data lawyer2;set lawyer2;
d1=(protest=1);d2=(protest=2);sexism_c=sexism-4.25;d1w_c=d1*sexism_c;d2w_c=d2*sexism_c;run;
proc reg data=lawyer2;model eval = d1 d2 d1w_c d2w_c sexism_c;test d1=0,d2=0;run;



*Moderation using PROCESS with a multicategorical moderator;
%process (data=lawyer2,y=eval,x=sexism,w=protest,model=1,mcw=1,plot=1);


*==============================;
*Module 6;
* First execute lawyer2.sas to create the lawyer2 data file;

*Do the conditional process analysis with PROCESS;
%process (data=lawyer2,y=eval,x=protest,m=approp,w=sexism,model=8,plot=1,mcx=1,seed=193456);


*Visualize the first stage model;
data;
input protest sexism approp;
datalines;
      .0000     4.2500     4.3184
     1.0000     4.2500     4.6990
     2.0000     4.2500     5.3332
      .0000     5.1200     3.8582
     1.0000     5.1200     5.0894
     2.0000     5.1200     5.5115
      .0000     5.8960     3.4477
     1.0000     5.8960     5.4377
     2.0000     5.8960     5.6705
run;
proc sgplot;reg x=sexism y=approp/group=protest;run;


*Visualize the model of Y;
data;
input protest sexism esteval;
datalines;
      .0000     4.2500     5.8991
     1.0000     4.2500     5.4610
     2.0000     4.2500     5.3418
      .0000     5.1200     5.6568
     1.0000     5.1200     5.6908
     2.0000     5.1200     5.5421
      .0000     5.8960     5.4407
     1.0000     5.8960     5.8957
     2.0000     5.8960     5.7207
run;
proc sgplot;reg x=sexism y=esteval/group=protest;run;


*==============================;
*Module 7;
* First execute lawyer2.sas to create the math data file;

*Is there are stereotype threat effect?;
proc reg data=math;model mathprob=threat;run;

*Correlations between math self-concept and performance;
proc corr data=math;var threat explms implms mathprob;run;

*Control for math self-concept;
proc reg data=math;model mathprob=threat explms implms;run;

*Do the moderation analysis;
data math;set math;thrtexpl=threat*explms;thrtimpl=threat*implms;run;
proc reg data=math;model mathprob=threat explms implms thrtexpl thrtimpl;run;

*estimate the moderation model using PROCESS;
%process (data=math,y=mathprob,x=threat,w=explms,z=implms,model=2,plot=1);

*visualize the model;
data;input threat explms implms mathprob;
datalines;
      .0000    13.0000     -.8816     6.0393
     1.0000    13.0000     -.8816     5.8023
      .0000    13.0000     -.5700     6.5163
     1.0000    13.0000     -.5700     6.9174
      .0000    13.0000     -.2392     7.0226
     1.0000    13.0000     -.2392     8.1013
      .0000    16.0000     -.8816     8.2425
     1.0000    16.0000     -.8816     7.1733
      .0000    16.0000     -.5700     8.7195
     1.0000    16.0000     -.5700     8.2884
      .0000    16.0000     -.2392     9.2259
     1.0000    16.0000     -.2392     9.4723
      .0000    17.0000     -.8816     8.9769
     1.0000    17.0000     -.8816     7.6302
      .0000    17.0000     -.5700     9.4539
     1.0000    17.0000     -.5700     8.7454
      .0000    17.0000     -.2392     9.9603
     1.0000    17.0000     -.2392     9.9293
run;
proc sgpanel;
panelby implms / columns=1;
series x=explms y=mathprob/group=threat lineattrs =(color=black);run;

*Test the set of products;
data math;set math;thrtexpl=threat*explms;thrtimpl=threat*implms;run;
proc reg data=math;model mathprob = threat explms implms thrtexpl thrtimpl;
test thrtexpl=0,thrtimpl=0;run;


*Test stereotype threat effect in "fragile" math self-concept women;
data math;set math;explms_c=explms-17;implms_c=implms-(-0.882);
thrtexpl=threat*explms_c;thrtimpl=threat*implms_c;run;
proc reg data=math;model mathprob=threat explms_c implms_c thrtexpl thrtimpl;run;

*Compare two conditional effects of X using PROCESS;
%process (data=math,y=mathprob,x=threat,w=explms,z=implms,model=2,contrast=17 -0.8816 13 -0.2392);

*---;


*estimate moderated moderation model;
data math;set math;thrtexpl=threat*explms;thrtimpl=threat*implms;
explimpl=explms*implms;condexim=threat*explms*implms;run;
proc reg data=math;model mathprob=threat explms implms thrtexpl thrtimpl explimpl condexim;run;

*estimate the moderated moderation model using PROCESS;
%process (data=math,y=mathprob,x=threat,w=explms,z=implms,model=3,plot=1,jn=1);

*visualize the model;
data;input threat explms implms mathprob;
datalines;
      .0000    13.0000     -.8816     5.9209
     1.0000    13.0000     -.8816     6.4840
      .0000    13.0000     -.5700     6.5302
     1.0000    13.0000     -.5700     6.6488
      .0000    13.0000     -.2392     7.1770
     1.0000    13.0000     -.2392     6.8238
      .0000    16.0000     -.8816     8.3260
     1.0000    16.0000     -.8816     7.1760
      .0000    16.0000     -.5700     8.7464
     1.0000    16.0000     -.5700     8.0508
      .0000    16.0000     -.2392     9.1926
     1.0000    16.0000     -.2392     8.9794
      .0000    17.0000     -.8816     9.1277
     1.0000    17.0000     -.8816     7.4067
      .0000    17.0000     -.5700     9.4851
     1.0000    17.0000     -.5700     8.5181
      .0000    17.0000     -.2392     9.8645
     1.0000    17.0000     -.2392     9.6979
run;
proc sgpanel;
panelby implms / columns=1;
series x=explms y=mathprob/group=threat lineattrs =(color=black);run;

*estimate conditional two-way threat by explicit interaction when implicit=high;
data math;set math;implms_p = implms-(-0.239);thrtim_p = threat*implms_p;expimp_p = explms*implms_p;
thrtexpl = threat*explms;thexim_p=threat*explms*implms_p;run;
proc reg data=math;model mathprob = threat explms implms_p thrtexpl thrtim_p expimp_p thexim_p;run;

*estimate stereotype threat effect among women with "fragile" math self-concept;
data math;set math;explms_p = explms-17;implms_p = implms-(-.882);
thrtim_p=threat*implms_p;expimp_p=explms_p*implms_p;thrtexpl=threat*explms_p;thexim_p=threat*explms_p*implms_p;run;
proc reg data=math;model mathprob=threat explms_p implms_p thrtexpl thrtim_p expimp_p thexim_p;run;

*==============================;
*Module 8;

*No R code for this module

*==============================;
*Module 9;
* First execute gaza.sas to create the gaza data file;

*Do the conditional process analysis with PROCESS;
%process (data=gaza,y=depress,x=trauma,m=frqual,w=age,z=sex,moments=1,plot=1,model=9,seed=145);

*Visualize the model of friendship quality;
data gazaplot;
input trauma age sex frqual;
datalines;
     3.0987    10.2103      .0000     4.0801
     7.2548    10.2103      .0000     3.7857
    11.4109    10.2103      .0000     3.4914
     3.0987    10.2103     1.0000     3.5436
     7.2548    10.2103     1.0000     3.7557
    11.4109    10.2103     1.0000     3.9678
     3.0987    11.3462      .0000     4.2540
     7.2548    11.3462      .0000     3.8237
    11.4109    11.3462      .0000     3.3934
     3.0987    11.3462     1.0000     3.7175
     7.2548    11.3462     1.0000     3.7937
    11.4109    11.3462     1.0000     3.8698
     3.0987    12.4820      .0000     4.4280
     7.2548    12.4820      .0000     3.8617
    11.4109    12.4820      .0000     3.2954
     3.0987    12.4820     1.0000     3.8915
     7.2548    12.4820     1.0000     3.8316
    11.4109    12.4820     1.0000     3.7718
run;
proc sgpanel;
panelby sex / columns=1;
series x=trauma y=frqual/group=age lineattrs =(color=black);run;


*==============================;
*Module 9;
* First execute injuries.sas to create the injuries data file;

*Do the conditional process analysis with PROCESS;
%process (data=injuries,y=injury,x=exhaust,m=safety,w=tenure,z=sex,cov=injuryb,moments=1,plot=1,
   model=21,seed=456);

*Visualize the first stage part of the model;
data;
input exhaust tenure safety;
datalines;
     2.3693     2.2404     4.0147
     3.3623     2.2404     4.5010
     4.3553     2.2404     4.9873
     2.3693     5.5133     3.7972
     3.3623     5.5133     4.0863
     4.3553     5.5133     4.3755
     2.3693     8.7863     3.5798
     3.3623     8.7863     3.6717
     4.3553     8.7863     3.7637
run;
proc sgplot;reg x=exhaust y=safety/group=tenure;run;



*Visualize the second stage part of the model;
data;
input safety sex injury;
datalines;
      3.0227      .0000     1.6285
      4.1020      .0000     2.0809
      5.1813      .0000     2.5334
      3.0227     1.0000     1.9912
      4.1020     1.0000     1.9955
      5.1813     1.0000     1.9999
run;
proc sgplot;reg x=safety y=injury/group=sex;run;


*Reprogram model 21 to get the other indices of conditional moderated mediation;
%process (data=injuries,y=injury,x=exhaust,m=safety,z=tenure,w=sex,cov=injuryb,moments=1,seed=456,
   model=21,wmatrix=0 0 1,zmatrix=1 0 0);





