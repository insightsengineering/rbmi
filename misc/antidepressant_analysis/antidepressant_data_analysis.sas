 
* Read antidepressant trial;

*please change in location of where you have the dataset;
libname current "/home/bceuser/nocia/sasuser.v94"; 

data ad_data;
  set current.chapter15_example;
run;


* MMRM anlaysis;

* A simple MMRM model with baseline*visit as covariates with a single variance-covariance matrix;
Title1 "A simple MMRM model with a single variance-covariance matrix";
PROC MIXED DATA=ad_data ;
CLASS VISIT PATIENT THERAPY ;
MODEL change = basval*VISIT THERAPY*VISIT / DDFM=KR ; 
REPEATED VISIT / TYPE=UN SUBJECT=PATIENT R ;
LSMEANS THERAPY*VISIT /Diffs;
ESTIMATE  "Main result"  THERAPY*VISIT 0 0 0 0 0 0 0 0 1 -1;
RUN;

*Title1 "A simple MMRM model with baseline*visit as covariates with separate variance-covariance matrix";
*PROC MIXED DATA=ad_data ;
*CLASS VISIT PATIENT THERAPY ;
*MODEL change = basval*VISIT THERAPY*VISIT / DDFM=KR ; 
*REPEATED VISIT / TYPE=UN SUBJECT=PATIENT R GROUP=THERAPY;
*LSMEANS THERAPY*VISIT /Diffs;
*ESTIMATE  "Main result"  THERAPY*VISIT 0 0 0 0 0 0 0 0 1 -1;
*RUN;

* LOAD 5 MACROS. Please change location according to where you have them ;

%include "/home/bceuser/nocia/sasuser.v94/Part1A_33.sas";
%include "/home/bceuser/nocia/sasuser.v94/Part1B_47.sas";
%include "/home/bceuser/nocia/sasuser.v94/part2A_40.sas";
%include "/home/bceuser/nocia/sasuser.v94/part2B_31.sas";
%include "/home/bceuser/nocia/sasuser.v94/part3_55.sas";
%include "/home/bceuser/nocia/sasuser.v94/plotter_7.sas";

options fullstimer;

%part1A(Jobname=PSI
,Data=ad_data 
,Subject=PATIENT
,Response=change
,Time=VISIT
,Treat=THERAPY
,CovbyTime=basval
);

* 1000 draws from the posterior distribution with a thining of 100 iterations;
%part1B(Jobname=PSI
,Ndraws=10000
,thin=50
,Seed=98765
);

* Branch here into a MAR analysis (the default method);
* Build the predicted means;
%part2A(Jobname=PSI_MAR
,inname=PSI)

* Simulate the missing data;
%part2b(Jobname=PSI_MAR)

* Analyse the data and combine the results across the imputd data sets;
%part3(Jobname=PSI_MAR
,Label=MAR separate covariance for PE and Analysis models);

Title1 "PSI example MADRS data set using MAR";
proc print data=PSI_MAR_Out(drop=row) NOOBS;
run;


*********** Now use the CIR method;
%part2A(Jobname=PSI_CIR
,inname=PSI
,method=CIR
,ref=PLACEBO)

%part2b(Jobname=PSI_CIR)

%part3(Jobname=PSI_CIR
,Label=CIR to Placebo separate covariance);

Title1 "PSI example data set using CIR with Placebo as reference";
proc print data=PSI_CIR_Out(drop=row) NOOBS;
run;

*********** Now use the CR method;
%part2A(Jobname=PSI_CR
,inname=PSI
,method=CR
,ref=PLACEBO)

%part2b(Jobname=PSI_CR)

%part3(Jobname=PSI_CR
,Label=CR to Placebo separate covariance);

Title1 "PSI example data set using CR with Placebo as reference";
proc print data=PSI_CR_Out(drop=row) NOOBS;
run;

*********** Now use the JTR method;
%part2A(Jobname=PSI_JTR
,inname=PSI
,method=J2R
,ref=PLACEBO)

%part2b(Jobname=PSI_JTR)

%part3(Jobname=PSI_JTR
,Label=JTR to Placebo separate covariance);

Title1 "PSI example data set using JTR with Placebo as reference";
proc print data=PSI_JTR_Out(drop=row) NOOBS;
run;



* Now display all of these together;
data temp;
set  PSI_MAR_Out PSI_CIR_Out PSI_CR_Out PSI_JTR_Out;	
rank=_N_;
run;

proc sort data=temp;
by row rank;
run;

options linesize=200;
title1 "Summary Table";
proc print data=temp(drop=row rank) noobs;
run;


