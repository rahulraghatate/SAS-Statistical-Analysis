Libname P507 "I:\P507\Datasets\";

proc import datafile="I:\P507\Datasets\collegegrades.csv"
     out=grades
     dbms=csv;
     getnames=yes;
run;

/*Looking at the variable names and labels, and at descriptive statistics*/
proc contents data=grades;
run;

proc means data=grades;
var colgpa tothrs athlete hsize hsrank hsperc;
run;

/*Running an OLS Regression*/
proc reg data=grades plots=none;
    model colgpa = tothrs athlete hsize hsrank hsperc;
run;

/*Reverse Coding hsperc so that higher values mean better rank*/

DATA grades;
  SET grades;
  hsperc_rev=100-hsperc;
RUN;

proc univariate data=grades;
var hsperc hsperc_rev;
histogram;
run;

/*Re-Run Regression with reverse coded variable*/
proc reg data=grades plots=none;
    model colgpa = tothrs athlete hsize hsrank hsperc_rev;
run;

/*Manually Creating The Multiplicative Terms*/
DATA grades1;
  SET grades;
  athleteXhsperc_rev=athlete*hsperc_rev;
RUN;

/*Re-Running the Model with an interaction for athlete and hsperc*/
proc reg data=grades1 plots=none;
    model colgpa = tothrs athlete hsize hsrank hsperc_rev athleteXhsperc_rev;
	store olsgpaint;
run;
 proc format;
         value athleteformat
               0 = 'Non-Athlete'
               1 = 'Athlete';
         run;

/*Same Model, but now using proc glm instead of proc reg so that we don't have to manually create interaction term*/
proc glm data=grades1 order=formatted plots=none;/
format Athlete athleteformat.; 
class athlete;
    model colgpa =tothrs hsize hsrank athlete|hsperc_rev/solution;
	store gpaint;/*this stores the results in a format that we can call on later for graphing purposes (I gave it name of gpaint)*/
run;

/*Manually Creating Dataset for Predicted Values Graph*/
data gpasimols;
do tothrs = 52.8322456;
do hsize = 2.7997269;
do hsrank = 52.8300701;
do athlete = 0,1;
do hsperc_rev = 0.1 to 92.0 by 0.1;
do athleteXhsperc_rev=athlete*hsperc_rev;
output;
end;
end;
end;
end;
end;
end;
run;

/*Manually Creating Dataset for Predicted Values Graph*/
data gpasim;
do tothrs = 52.8322456;
do hsize = 2.7997269;
do hsrank = 52.8300701;
do athlete = 0,1;
do hsperc_rev = 0.1 to 92.0 by 0.1;
output;
end;
end;
end;
end;
end;
run;

/*This creates a dataset that has predicted values for each row in the simulation dataset*/
proc plm restore=gpaint;
score data=gpasim out=gpapreds predicted=pred lclm=lower uclm=upper;
run;

/*This creates a dataset that has predicted values for each row in the simulation dataset*/
proc plm restore=olsgpaint;
score data=gpasimols out=gpapredsols predicted=pred;
run;


/*Graphing the Interaction Term With Confidence Intervals*/
proc sgplot data=gpapredols;
series x=hsperc_rev y=pred / group=athlete;
yaxis label="Predicted College GPA";
xaxis label ="HS Percentile";
title "HS Graduating Class Percentile and College GPA, Athletes vs Non-Athletes";
run;

/*Graphing the Interaction Term Without Confidence Intervals*/
proc sgplot data=gpapreds;
series x=hsperc_rev y=pred / group=athlete;
yaxis label="Predicted College GPA";
run;


/*Using effectplot function in plm to generate identical output to above*/
proc plm restore=gpaint;
effectplot slicefit (x=hsperc_rev sliceby=athlete);
run;
