* Encoding: UTF-8.

/************* Module 1 (Review) *************/.
* First open the teams data file.

* The total effect of dysfunctional behavior on team performance.
regression/dep=perform/method=enter dysfunc.

* Do the mediation analysis with PROCESS (after executing process.sps).
process y=perform/x=dysfunc/m=negtone/model=4/total=1/boot=10000/seed=3341.

* Do the moderation analysis with PROCESS (after executing process.sps).
process y=perform/x=negtone/w=negexp/plot=1/jn=1/model=1.

* Visualizing the model.
DATA LIST FREE/
   negtone    negexp     perform    .
BEGIN DATA.
     -.4500     -.5308     -.0001
     -.0350     -.5308      .0038
      .5224     -.5308      .0091
     -.4500     -.0600      .1212
     -.0350     -.0600      .0074
      .5224     -.0600     -.1456
     -.4500      .6000      .2913
     -.0350      .6000      .0124
      .5224      .6000     -.3623
END DATA.
GRAPH/SCATTERPLOT=
 negtone  WITH     perform  BY       negexp   .
  
* Do the conditional process analysis with PROCESS (after executing process.sps).
process x=dysfunc/m=negtone/y=perform/w=negexp/plot=1/model=14/seed=61326.


* Visualizing the second stage of the model.
DATA LIST FREE/
   negtone    negexp     perform    .
BEGIN DATA.
     -.4500     -.5308      .0836
     -.0350     -.5308      .0166
      .5224     -.5308     -.0733
     -.4500     -.0600      .1841
     -.0350     -.0600      .0161
      .5224     -.0600     -.2095
     -.4500      .6000      .3250
     -.0350      .6000      .0154
      .5224      .6000     -.4004
END DATA.
GRAPH/SCATTERPLOT=
 negtone  WITH     perform  BY       negexp   .

/* ============================== */.
* Module 1
* First open the mentor data file.

* Estimate the serial mediation model with PROCESS.
process y=conflict/x=mentor/m=resource workload/total=1/model=4/seed=92612.

* Estimate the conditional process model with PROCESS.
process y=conflict/x=mentor/m=resource workload/w=wforient/plot=1/model=7/seed=8231.

* Visualize the model of resources.
DATA LIST FREE/
   mentor     wforient   resource   .
BEGIN DATA.
     3.1000     2.3000     2.8664
     3.7000     2.3000     2.8582
     4.3000     2.3000     2.8501
     3.1000     3.1000     3.2592
     3.7000     3.1000     3.3763
     4.3000     3.1000     3.4933
     3.1000     3.8000     3.6029
     3.7000     3.8000     3.8296
     4.3000     3.8000     4.0562
END DATA.
GRAPH/SCATTERPLOT=
 mentor   WITH     resource BY       wforient .

* Visualize the model of workload.
DATA LIST FREE/
   mentor     wforient   workload   .
BEGIN DATA.
     3.1000     2.3000     3.3321
     3.7000     2.3000     3.7251
     4.3000     2.3000     4.1182
     3.1000     3.1000     3.0328
     3.7000     3.1000     3.2911
     4.3000     3.1000     3.5494
     3.1000     3.8000     2.7710
     3.7000     3.8000     2.9113
     4.3000     3.8000     3.0516
END DATA.
GRAPH/SCATTERPLOT=
 mentor   WITH     workload BY       wforient .

/* ============================== */.
* Module 2
* First open the binladen data file.

* estimating the model using PROCESS.
process y=mcivil/x=binladen/m=stereo rthreat/cov=sex age ideo/model=6/total=1/seed=6234.

* customizing the assignment of covariates in PROCESS.
process y=mcivil/x=binladen/m=stereo rthreat/cov=sex age ideo/model=6/seed=6234/cmatrix=0,1,1,1,1,1,0,0,1.

/* ============================== */.
* Module 3
* First open the binladen data file.

* estimating the model using PROCESS (after first executing process.sps).
process y=mcivil/x=binladen/m=stereo rthreat/w=age/cov=sex ideo/model=85/plot=1/seed=63234.

* Visualize the stereo model.
DATA LIST FREE/
   binladen   age        stereo     .
BEGIN DATA.
      .0000     3.0000     2.7092
     1.0000     3.0000     2.9991
      .0000     4.8000     2.8588
     1.0000     4.8000     2.9985
      .0000     6.7000     3.0168
     1.0000     6.7000     2.9980
END DATA.
GRAPH/SCATTERPLOT=
 age      WITH     stereo   BY       binladen .

* Visualize the rthreat model.
DATA LIST FREE/
   binladen   age        rthreat    .
BEGIN DATA.
      .0000     3.0000     2.4404
     1.0000     3.0000     2.5926
      .0000     4.8000     2.5657
     1.0000     4.8000     2.6067
      .0000     6.7000     2.6980
     1.0000     6.7000     2.6216
END DATA.
GRAPH/SCATTERPLOT=
 age      WITH     rthreat  BY       binladen .

* Visualize the mcivil model.
DATA LIST FREE/
   binladen   age        mcivil     .
BEGIN DATA.
      .0000     3.0000     2.6307
     1.0000     3.0000     2.6553
      .0000     4.8000     2.6336
     1.0000     4.8000     2.6043
      .0000     6.7000     2.6367
     1.0000     6.7000     2.5505
END DATA.
GRAPH/SCATTERPLOT=
 age      WITH     mcivil   BY       binladen .

* Estimating the model using PROCESS model 83 instead.
process y=mcivil/x=binladen/m=stereo rthreat/w=age/cov=sex ideo/model=83/plot=1/seed=63234.

* Estimating the model using PROCESS model 83 instead, brining age back as a covariate.
process y=mcivil/x=binladen/m=stereo rthreat/w=age/cov=age sex ideo/model=83/plot=1/seed=63234.

/* ============================== */.
* Module 4
* First open the lawyer2 data file.

* Show group means and do single factor ANOVA.
means tables = eval by protest/statistics anova.

* frequency table for condition.
frequencies variables = protest.

* Regress evaluation on experimental condition.
compute d1 = (protest=1).
compute d2 = (protest=2).
regression/dep = eval/method = enter d1 d2.

* Estimate relative total effects of condition (same as code above).
compute d1 = (protest=1).
compute d2 = (protest=2).
regression/dep = eval/method = enter d1 d2.

* Estimate a1 and a2.
regression/dep = approp/method = enter d1 d2.

* Estimate b and the relative direct effects (c'1 and c'2).
regression/dep = eval/method = enter approp d1 d2.

* Do the mediation analysis with PROCESS (after first executing process.sps).
process y=eval/m=approp/x=protest/mcx=1/model=4/total=1/seed=61235.

* Create Helmert codes and estimate the relative total effects (c1 and c2).
if (protest = 0) d1 = -2/3.
if (protest > 0) d1 = 1/3.
if (protest = 0) d2 = 0.
if (protest = 1) d2 = -1/2.
if (protest = 2) d2 = 1/2.
regression/dep = eval/method = enter d1 d2.

* Estimate a1 and a2.
regression/dep = approp/method = enter d1 d2.

* Estimate b and the relative direct effects (c'1 and c'2).
regression/dep = eval/method = enter approp d1 d2.

* Do the mediation analysis with PROCESS (after running process.sps).
process y=eval/m=approp/x=protest/mcx=3/model=4/total=1/seed=61235.

/* ============================== */.
* Module 5
* First open the lawyer2 data file.

* Create indicator codes and estimate unconditional effect model.
compute d1 = (protest = 1).
compute d2 = (protest = 2).
regression/dep = eval/method = enter d1 d2 sexism.


* Create products and estimate the moderation model.
compute d1w = d1*sexism.
compute d2w = d2*sexism.
regression/dep = eval/method = enter d1 d2 sexism d1w d2w.

* Visualize the model.
data list free/d1 d2 w protest.
begin data.
0 0 4 0
0 0 5 0
0 0 6 0
1 0 4 1
1 0 5 1
1 0 6 1
0 1 4 2
0 1 5 2
0 1 6 2
end data.
compute yhat = 7.706-4.129*d1-3.491*d2-0.472*w+
               0.901*d1*w+0.778*d2*w.
graph/scatterplot = w with yhat by protest.

* Omnibus test of interaction between X and W.
regression/statistics defaults change/dep = eval/method = enter d1 d2 sexism/method = enter d1w d2w.


* Estimate and probe using PROCESS instead (after executing process.sps).
process y=eval/w=sexism/x=protest/model=1/mcx=1/plot=1.

* Visualize using output from the PROCESS plot option.
DATA LIST FREE/
   protest    sexism     eval       .
BEGIN DATA.
      .0000     4.2500     5.6981
     1.0000     4.2500     5.3996
     2.0000     4.2500     5.5131
      .0000     5.1200     5.2871
     1.0000     5.1200     5.7726
     2.0000     5.1200     5.7787
      .0000     5.8960     4.9204
     1.0000     5.8960     6.1053
     2.0000     5.8960     6.0156
END DATA.
GRAPH/SCATTERPLOT=
 sexism   WITH     eval     BY       protest  .

* Get percentiles of the distribution of sexism.
frequencies variables = sexism
/statistics mean stddev
/percentiles = 16 50 84.

* Probe by conditioning on moderator value of 4.25.
compute sexism_c = sexism-4.25.
compute d1w_c = d1*sexism_c.
compute d2w_c = d2*sexism_c.
regression/statistics defaults change/dep = eval/method = enter sexism_c d1w_c d2w_c/method = enter d1 d2.

* Moderation using PROCESS with a multicategorical moderator (after running process.sps).
process y=eval/x=sexism/w=protest/model=1/mcw=1/plot=1.

/* ============================== */.
* Module 6
* First open the lawyer2 data file.

* Do the conditional process analysis with PROCESS (after running process.sps).
process y=eval/x=protest/m=approp/w=sexism/model=8/plot=1/mcx=1/seed=193456.

* Visualize the first stage model.
DATA LIST FREE/
   protest    sexism     approp     .
BEGIN DATA.
      .0000     4.2500     4.3184
     1.0000     4.2500     4.6990
     2.0000     4.2500     5.3332
      .0000     5.1200     3.8582
     1.0000     5.1200     5.0894
     2.0000     5.1200     5.5115
      .0000     5.8960     3.4477
     1.0000     5.8960     5.4377
     2.0000     5.8960     5.6705
END DATA.
GRAPH/SCATTERPLOT=
 sexism   WITH     approp   BY  protest  .


* Visualize the model of Y.
DATA LIST FREE/
   protest    sexism     eval       .
BEGIN DATA.
      .0000     4.2500     5.8991
     1.0000     4.2500     5.4610
     2.0000     4.2500     5.3418
      .0000     5.1200     5.6568
     1.0000     5.1200     5.6908
     2.0000     5.1200     5.5421
      .0000     5.8960     5.4407
     1.0000     5.8960     5.8957
     2.0000     5.8960     5.7207
END DATA.
GRAPH/SCATTERPLOT=
 sexism   WITH     eval     BY       protest  .

/* ============================== */.
* Module 7
* First open the math data file.

* Is there are stereotype threat effect?.
regression/dep=mathprob/method=enter threat.

* Correlations between math self-concept and performance.
correlations variables = threat explms implms mathprob.


* Control for math self-concept.
regression/dep = mathprob/method = enter threat explms implms.

* Do the moderation analysis.
compute thrtexpl = threat*explms.
compute thrtimpl = threat*implms.
regression/dep = mathprob/method = enter threat explms implms thrtexpl thrtimpl.


* Estimate the moderation model using PROCESS (after first running process.sps).
process y=mathprob/x=threat/w=explms/z=implms/model=2/plot=1.

* Visualize the model.
DATA LIST FREE/
   threat     explms     implms     mathprob   .
BEGIN DATA.
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
END DATA.
GRAPH/SCATTERPLOT=
 explms   WITH     mathprob BY       threat   /PANEL   ROWVAR=  implms.


* Test the set of products.
compute thrtexpl = threat*explms.
compute thrtimpl = threat*implms.
regression/statistics defaults change/dep = mathprob/method = enter threat explms implms
   /method = enter thrtexpl thrtimpl.



* Test stereotype threat effect in "fragile" math self-concept women.
compute explms_c=explms-17.
compute implms_c=implms-(-0.882).
compute thrtexpl = threat*explms_c.
compute thrtimpl = threat*implms_c.
regression/dep = mathprob/method = enter threat explms_c implms_c thrtexpl thrtimpl.

* Compare two conditional effects of X on Y using PROCESS (after first executing process.sps).
process y=mathprob/x=threat/w=explms/z=implms/model=2/contrast=17,-0.8816;13,-0.2392.


* Estimate moderated moderation model.
compute thrtexpl = threat*explms.
compute thrtimpl = threat*implms.
compute explimpl = explms*implms.
compute condexim = threat*explms*implms.
regression/dep = mathprob/method = enter threat explms implms thrtexpl thrtimpl explimpl condexim.

* Estimate the moderated moderation model using PROCESS (after first executing process.sps).
process y=mathprob/x=threat/w=explms/z=implms/model=3/plot=1/jn=1.

* visualize the model.
DATA LIST FREE/
   threat     explms     implms     mathprob   .
BEGIN DATA.
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
END DATA.
GRAPH/SCATTERPLOT=
 explms   WITH   mathprob BY  threat   /PANEL   ROWVAR=  implms.  


* Estimate conditional two-way threat by explicit interaction when implicit=high.
compute implms_p = implms-(-0.239).
compute thrtim_p = threat*implms_p.
compute expimp_p = explms*implms_p.
compute thrtexpl = threat*explms.
compute thexim_p=threat*explms*implms_p.
regression/dep = mathprob/method = enter threat explms implms_p thrtexpl thrtim_p expimp_p thexim_p.

* Estimate stereotype threat effect among women with "fragile" math self-concept
compute explms_p = explms-17.
compute implms_p = implms-(-0.882).
compute thrtim_p = threat*implms_p.
compute expimp_p = explms_p*implms_p.
compute thrtexpl = threat*explms_p.
compute thexim_p=threat*explms_p*implms_p.
regression/dep = mathprob/method = enter threat explms_p implms_p thrtexpl thrtim_p expimp_p thexim_p.


/* ============================== */.
* Module 8
* No R code for this module.

/* ============================== */.
* Module 9
* First open the gaza data file.

* Do the conditional process analysis with PROCESS (after first executing process.sps).
process y=depress/x=trauma/m=frqual/w=age/z=sex/moments=1/plot=1/model=9/seed=145.

* Visualize the model of friendship quality.
DATA LIST FREE/
   trauma     age        sex        frqual     .
BEGIN DATA.
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
END DATA.
GRAPH/SCATTERPLOT=
 trauma   WITH     frqual   BY       age      
    /PANEL   ROWVAR=  sex. 

/* ============================== */.
* Module 10
* First open the injuries data file.

* Do the conditional process analysis with PROCESS (after first executing process.sps).
process y=injury/x=exhaust/m=safety/w=tenure/z=sex/cov=injuryb/moments=1/plot=1/model=21/seed=465.

* Visualize the first stage model of safety.
DATA LIST FREE/
    exhaust   tenure     safety     .
BEGIN DATA.
     2.3693     2.2404     4.0147
     3.3623     2.2404     4.5010
     4.3553     2.2404     4.9873
     2.3693     5.5133     3.7972
     3.3623     5.5133     4.0863
     4.3553     5.5133     4.3755
     2.3693     8.7863     3.5798
     3.3623     8.7863     3.6717
     4.3553     8.7863     3.7637
END DATA.
GRAPH/SCATTERPLOT=
 exhaust      WITH     safety   BY       tenure .


* Visualize the second stage model of injury.
DATA LIST FREE/
   safety     sex        injury     .
BEGIN DATA.
     3.0227      .0000     1.6285
     4.1020      .0000     2.0809
     5.1813      .0000     2.5334
     3.0227     1.0000     1.9912
     4.1020     1.0000     1.9955
     5.1813     1.0000     1.9999
END DATA.
GRAPH/SCATTERPLOT=
 safety   WITH     injury   BY       sex      .


* Reprogram model 21 to get the other indices of conditional moderated mediation.
process y=injury/x=exhaust/m=safety/z=tenure/w=sex/cov=injuryb/moments=1/model=21/seed=456/
   wmatrix=0,0,1/zmatrix=1,0,0.









