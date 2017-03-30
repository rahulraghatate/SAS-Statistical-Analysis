Libname P507 "I:\P507\Datasets\";

proc import datafile="I:\P507\Datasets\univs_hettest.csv"
     out=univs
     dbms=csv;
     getnames=yes;
run;

proc reg data=univs plots=none;
model intuitfees = satact facsal_revise_cons13/stb vif spec;
OUTPUT P=pred1 R=resids OUT=temp;
run;

data univs_hettest;
set temp;
if resids=. THEN DELETE;/*Getting rid of obs that are listwise deleted, as this makes the Goldfeld-Quant procedure easier to manage*/
residsq=resids**2;
satactsq = satact**2;
facsalsq=facsal_revise_cons13**2;
satactfacsal= satact*facsal_revise_cons13;
run;

proc sort data=univs_hettest;
by facsal_revise_cons13;
run;

DATA lowfacsal;
SET univs_hettest;
OBSNUM = _N_;/*Creates a new variable that is just a count of the ordering of the observations*/
IF OBSNUM<=412;/*Original regression used 1235 observations. 
412 is approximately one-third. This tells SAS to take the bottom 412 obs and put in a new dataset called lowfacsal*/
RUN;
DATA highfacsal;
SET univs_hettest;
OBSNUM = _N_;
IF OBSNUM>=824;/*This tells SAS to take the top 412 observations and put in a new dataset called highfacsal*/
RUN;

/*Run two regressions for high and low facsal groups*/
TITLE "Goldfeld-Quandt Test";
proc reg data=lowfacsal plots=none;
model intuitfees = satact facsal_revise_cons13;
run;
/*Note: RSS equals 14520458009*/
proc reg data=highfacsal plots=none;
model intuitfees = satact facsal_revise_cons13;
run;
/*Note: RSS equals 37748398466*/
/* F = 37748398466/14520458009 = 2.60 with 412-3(409);412-3(409) degrees of freedom*/

TITLE "B-P and White's Test Regression";
PROC REG PLOTS=none data=univs_hettest;
 MODEL residsq = satact facsal_revise_cons13;/*This is the B-P Auxiliary Regression*/
 MODEL residsq = satact facsal_revise_cons13 satactsq facsalsq satactfacsal;/*White Test Auxiliary Regression*/
RUN;

proc model data=univs_hettest plots=none;
parms b0 b1 b2;
intuitfees=b0+b1*satact+b2*facsal_revise_cons13;
fit intuitfees/ white breusch=(1 satact facsal_revise_cons13);
run;
