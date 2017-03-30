TITLE "P507 IMPLICITLY LINEAR REGRESSION EXAMPLE"ù;

LIBNAME P507 "I:P507\Datasets\";

/* Data Definitions:
  Obs:  4137

  1. sat                      combined SAT score
  2. tothrs                   total hours through fall semest
  3. colgpa                   GPA after fall semester
  4. athlete                  =1 if athlete
  5. verbmath                 verbal/math SAT score
  6. hsize                    size graduating class, 100s
  7. hsrank                   rank in graduating class
  8. hsperc                   high school percentile, from top
  9. female                   =1 if female
 10. white                    =1 if white
 11. black                    =1 if black
*/

DATA gpa2;
  SET P507.GPA;
  LNCOLGPA=LOG(COLGPA);
  LNHSIZE=LOG(HSIZE);
  LNHSRANK=LOG(HSRANK);
  SATSQ=SAT**2;
RUN;

PROC PRINT data=temp (firstobs=1 obs=10);
RUN;

/*Creating Data for Table of Summary Statistics*/
PROC UNIVARIATE NOPRINT outtable=Table;
  VAR COLGPA LNCOLGPA SAT TOTHRS HSIZE LNHSIZE HSRANK LNHSRANK FEMALE ATHLETE;
RUN;

/*Printing Data for Table of Summary Statistics*/
proc print data=Table label;
   var _VAR_ _MIN_ _MEAN_ _MAX_ _STD_;
   label _VAR_='Variable';
run;

/*Printing Histograms for each variable, plotted against normal distribution given mu and sigma*/
PROC UNIVARIATE NOPRINT data=gpa2;
  HISTOGRAM COLGPA LNCOLGPA SAT TOTHRS HSIZE LNHSIZE HSRANK LNHSRANK FEMALE ATHLETE/normal(noprint color=red);
RUN;

TITLE2 "INITIAL REGRESSION MODEL";
PROC REG PLOTS=NONE data=gpa2;
  MODEL COLGPA = SAT TOTHRS HSIZE HSRANK FEMALE/STB VIF;
  OUTPUT P=pred R=resid;
RUN;

PROC GPLOT;
	PLOT resid*pred/VREF=0;
	RUN;

TITLE2 "INITIAL REGRESSION MODEL";
PROC REG data=gpa2;
  MODEL COLGPA = SAT TOTHRS HSIZE HSRANK FEMALE/STB VIF;
RUN;


TITLE2 "ADDED VARIABLE REGRESSION MODEL";
PROC REG PLOTS=NONE data=gpa2;
  MODEL COLGPA = SAT TOTHRS HSIZE HSRANK FEMALE ATHLETE/STB VIF;
  test ATHLETE=0;
RUN;

TITLE2 "EXPONENTIAL GROWTH (LOG-LIN) REGRESSION MODEL";
*Interpret as 100*Bi% in Y given unit change in Xi;
PROC REG PLOTS=NONE data=gpa2;
  MODEL LNCOLGPA = SAT TOTHRS HSIZE HSRANK FEMALE ATHLETE/STB VIF;
RUN;

TITLE2 "DECLINING RETURNS (LIN-LOG) REGRESSION MODEL";
*Interpret as a 1% change in HSRANK causes COLGPA to change by B4/100;
PROC REG PLOTS=NONE data=gpa2;
  MODEL COLGPA = SAT TOTHRS HSIZE LNHSRANK FEMALE ATHLETE/STB VIF;
RUN;

TITLE2 "SAT SQUARED REGRESSION MODEL";
PROC REG PLOTS=NONE data=gpa2;
  MODEL COLGPA = SAT SATSQ TOTHRS HSIZE HSRANK FEMALE ATHLETE/STB VIF;
RUN;

QUIT;
