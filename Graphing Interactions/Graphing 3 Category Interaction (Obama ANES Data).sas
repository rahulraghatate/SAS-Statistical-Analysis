Libname ANES "I:\P507\Datasets\ANES";

proc import datafile="I:\P507\Datasets\ANES\ANES_Obama.csv"
     out=ANES_Obama
     dbms=csv;
     getnames=yes;
run;

/*Looking at the variable names and labels, and at descriptive statistics*/
proc contents data=ANES_Obama;
run;

proc means;
var ftpo_pres relig_pray;
run;

proc freq;
tables partisanship;
run;

/*Reverse Coding relig_pray so that higher values mean pray more often and lower values mean pray less often*/
data ANES_Obama;
set ANES_Obama;
relig_pray_rev=6-relig_pray;/*Max Value for relig_pray is 5, so subtract from 6 (max value +1)*/
relig_pray_revXdemocrat=relig_pray_rev*democrat;
relig_pray_revXindependent=relig_pray_rev*independent;
relig_pray_revXrepublican=relig_pray_rev*republican;
run;

/*Standard OLS Model with Interactions Added Manually*/
proc reg data=ANES_Obama plots=none;
model ftpo_pres = democrat independent relig_pray_rev relig_pray_revXdemocrat relig_pray_revXindependent;
run;

 proc format;
         value $partisanformat
               'Democrat' = '1.Democrat'
               'Republican' = '2.Republican'
				'Independent' = '3.Independent';
         run;
/*Interaction model using GLM*/
proc glm data=ANES_Obama order = formatted plots=none;
format partisanship $partisanformat.;
class partisanship;
    model ftpo_pres = partisanship|relig_pray_rev/solution;
	store Obama_int;/*this stores the results in a format that we can call on later for graphing purposes (I gave it name of Obama)*/
run;

 proc format;
         value $partisanformat
               'Democrat' = '3.Democrat'
               'Republican' = '2.Republican'
				'Independent' = '1.Independent';
         run;
/*Interaction model using GLM*/
proc glm data=ANES_Obama order = formatted plots=none;
format partisanship $partisanformat.;
class partisanship;
    model ftpo_pres = partisanship|relig_pray_rev/solution;
	store Obama_int;/*this stores the results in a format that we can call on later for graphing purposes (I gave it name of Obama_int)*/
run;

 proc format;
         value $partisanformat
               'Democrat' = '2.Democrat'
               'Republican' = '3.Republican'
				'Independent' = '1.Independent';
         run;
/*Interaction model using GLM*/
proc glm data=ANES_Obama order = formatted plots=none;
format partisanship $partisanformat.;
class partisanship;
    model ftpo_pres = partisanship|relig_pray_rev/solution;
	store Obama_int;/*this stores the results in a format that we can call on later for graphing purposes (I gave it name of Obama_int)*/
run;

proc plm resore=Obama_int;
effectplot slicefit (x=relig_pray_rev sliceby=partisanship)/clm;
run;

