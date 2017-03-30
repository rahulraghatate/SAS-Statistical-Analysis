TITLE "P507 In-Class Heteroscedasticity Exercise";

* P507 IN-CLASS EXERCISE FOR HETEROSCEDASTICITY TESTS AND WLS REGRESSIONS;
* WITH INVENTORY DATA SET.  THIS EXAMPLE USES THE ASSUMPTION THAT THE VARIANCE;
* OF THE ERROR TERMS IS PROPORTIONAL TO SQUARE OF SALES. ;

* INPUT DATA INTO SAS TEMPORARY DATASET NAMED ORIGDATA;

data origdata;
input year invent sales gdppc;
datalines;
50 31.1 18.6 4.47
51 39.3 21.7 4.73
52 41.1 22.5 4.83
53 43.9 24.8 4.98
54 41.6 23.3 4.79
55 45.1 26.5 5.03
56 50.6 27.7 5.05
57 51.9 28.7 5.06
58 50.2 27.2 4.95
59 52.9 30.3 5.15
60 53.8 30.9 5.16
61 54.9 30.9 5.16
62 58.2 33.4 5.19
63 60.0 35.0 5.40
64 63.4 37.3 5.54
65 68.2 41.0 5.74
66 78.0 44.9 6.01
67 84.7 46.5 6.40
68 90.6 50.2 6.62
69 98.2 53.5 6.73
70 101.6 52.8 6.64
71 102.6 55.9 6.74
72 108.2 63.0 7.06
73 124.6 72.9 7.37
74 157.8 84.8 7.21
75 159.9 86.4 7.07
76 175.2 98.8 7.40
77 189.2 113.2 7.70
78 210.4 126.9 7.95
79 240.9 143.9 8.11
80 264.1 154.4 7.97
81 282.1 168.1 8.16
82 264.6 159.2 7.84
83 260.4 170.6 8.00
;
run;

* RUN THE MAIN REGRESSION AND CAPTURE RESIDUALS IN DATASET REGOUT1;

TITLE "Main Regression";

proc reg;
  model invent = sales gdppc;
  output R=resid1 out=regout1;
run;

* DATA TRANSFORMATIONS FOR WHITE'S TEST AND THE WLS CORRECTION PROCEDURE.;
* TRANSFORMS ARE MADE FOR THE AD HOC ASSUMPTION THAT SIGMA I SQUARED  ;
* IS PROPORTIONAL TO SALES SQUARED.               ;

data white1;
  set regout1;
  salessqd = sales**2;
  gdppcsqd = gdppc**2;
  salesgdp = sales * gdppc;
  residsq=resid1**2;
run;

TITLE "First Whites Test";

proc reg plots=none data=white1;
  model residsq = sales gdppc salessqd gdppcsqd salesgdp;
run;

TITLE "First Goldfeld-Quandt Tests";

* SORT DATA BY SALES AND CARRY OUT GOLDFELD-QUANDT REGRESSIONS. NOTE THAT; 
* 8 OBSERVATIONS HAVE BEEN ELIMINATED BY DETERMINING CUT-OFF VALUES VIA; 
* INSPECTION OF THE PRINTED DATA FOLLOWING PROC SORT;

proc sort data=origdata;
  by sales;
run;

proc print;
run;

* SUBSET DATA FOR LOWEST 13 OBSERVATIONS & CARRY OUT GOLDFELD-QUANDT REGRESSION;

TITLE2 "GQ Test Low Subsample";
data gq1;
  set origdata;
  if sales > 34 then delete;
run;
proc reg plots=none;
  model invent = sales gdppc;
run;

* SUBSET DATA FOR HIGHEST 13 OBSERVATIONS & CARRY OUT GOLDFELD-QUANDT REGRESSION;

TITLE2 "GQ Test High Subsample";
data gq2;
  set origdata;
  if sales < 54 then delete;
run;
proc reg plots=none;
  model invent = sales gdppc;
run;

* TRANSFORM THE DATA BASED ON THE ASSUMPTION THAT SALES-SQUARED IS THE SOURCE.;
* RUN THE WEIGHTED LEAST SQUARES (WLS) REGRESSION. NOTE THAT THIS REGRESSION RUNS; 
* WITH AN INTERCEPT, AND THAT YOU MUST SWITCH AROUND THE INTERCEPT AND THE SLOPE ;
* COEFFICIENT AFTER THE WLS REGRESSION TO OBTAIN THE FINAL EQUATION.;

data wls;
  set origdata;
  sales2 = 1 / sales;
  invent2 = invent / sales;
  gdppc2= gdppc / sales;
run;

TITLE "WLS Regression";

proc reg data=wls;
  model invent2 = sales2 gdppc2;
  output R=resid2 P=pred2 out=regout2;
run;

* REPEAT WHITE'S TEST ON TRANSFORMED DATA;

TITLE "Second Whites Test";
data whites2;
set regout2;
sales2sq=sales2**2;
gdppc2sq=gdppc2**2;
sales2gdp2=sales2*gdppc2;
resid2sq=resid2**2;
pred2sq=pred2**2;
run;

proc reg data=whites2 plots=none;
model resid2sq = sales2 gdppc2 sales2sq gdppc2sq sales2gdp2;
run;

*CALCULATE THE NEW R-SQUARED VALUE: USE WLS PARAMETERS AND FINAL REGRESSION MODEL TO; 
*PREDICT NEW VALUE OF INVENT (inventp), AND THEN USE PROC CORR TO DERIVE CORRELATION;
*WITH ORIGINAL INVENT VARIABLE.  SQUARE CORRELATION COEFFICIENT TO DERIVE R-SQUARED.;
*Adjusted R2 = 1 - ((1-R-Squared)(n-1))/n-k-1);


TITLE "PROC CORR RESULTS FOR FINAL R-SQUARED VALUE CALCULATION";

DATA rsqr;
  set wls;
  inventp = -19.4002 + 1.5219*sales + 5.2078*gdppc;
RUN;

PROC CORR DATA=rsqr;
  VAR invent inventp;
RUN;

TITLE "PROC MODEL WLS";

* USE PROC MODEL TO REPLICATE ORIGINAL WHITE'S TEST, WLS REGRESSION, ;
* AND FINAL WHITE'S TEST;

* THIS IS THE PROC MODEL OLS REGRESSION TO GENERATE THE INITIAL WHITE'S TEST.;

PROC MODEL DATA=origdata PLOTS=NONE;
  PARMS b1 b2 b3;
  invent = b1 + b2*sales + b3*gdppc;
  fit invent / white;
RUN;

/* THIS IS THE PROC MODEL WLS REGRESSION AND GENERATES THE FINAL WHITE'S TEST.
   NOTE THAT PROC MODEL ALLOWS FOR DATA TRANSFORMATIONS WITHIN THE PROCEDURE.
   THE WEIGHTING VARIABLE FOR WLS IS ENTERED VIA THE INVERSE OF THE AD HOC ASSUMPTION. 
   SAS THEN MULTIPLES ALL VARIABLES BY THE SQUARE ROOT OF THE WEIGHTING VARIABLE TO 
   GENERATE WLS WEIGHTS. AS SALES-SQUARED IS SUSPECTED AS THE VARIABLE FOR THE AD HOC 
   ASSUMPTION, "saleinv2 = 1/(sales**2)" IS USED TO CREATE THE WEIGHTING VARIABLE. YOU CAN    
   CREATE THE WEIGHTING VARIABLE AS A TRANSFORMATION WITHIN THE PROC MODEL PROCEDURE.*/

PROC MODEL DATA=origdata PLOTS=NONE;
  PARMS b1 b2 b3;
  salesinv2 = 1/(sales**2);
  invent = b1 + b2*sales + b3*gdppc;
  fit invent / white fsrsq;
  weight salesinv2;
RUN;

/** REPEAT GOLDFELD-QUANDT TEST FOR TRANSFORMED, WLS MODEL. DETERMINE CUTOFF VALUES;
* BY INSPECTION, OR CAN USE PREVIOUS CUTOFF.;

TITLE "Second Goldfeld-Quandt Test";
proc sort;
  by sales2;
run;

* CARRY OUT GOLDFELD-QUANDT REGRESSION FOR LOWEST 13 OBSERVATIONS.;

data gq3;
  set wls;
  if sales2>.018 then delete;
run;

proc reg data=gq3 plots=none;
  model invent2 = sales2 gdppc2;
run;

* CARRY OUT GOLDFELD-QUANDT REGRESSION FOR HIGHEST 13 OBSERVATIONS.;

data gq4;
  set wls;
  if sales2<.029 then delete;
run;

proc reg data=gq4 plots=none;
  model invent2 = sales2 gdppc2;
run;*/


QUIT;
