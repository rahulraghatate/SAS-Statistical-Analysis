
TITLE 'P507 Autocorrelation Example - U.S. Import Data';

DATA raw;
INPUT year gnp cons interest
        unemp infla invest income
        taxm expendm export import price;
tax = 100*taxm/price;
expend = 100*expendm/price;

* Note that except for tax and expend, all data are in 1982 prices;

CARDS;
1970  2416.2  1492.0   8.04     4.8    5.7  381.5  1668.1
       195.4   207.8  178.3   208.3   42.0
1971  2484.8  1538.8    7.39    5.8    4.4  419.3  1728.4
       202.7   224.8  179.2   218.9   44.4
1972  2608.5  1621.9    7.21    5.5    3.2  465.4  1797.4
       232.2   249.0  195.2   244.6   46.5
1973  2744.1  1689.6    7.44    4.8    6.2  520.8  1916.3
       263.7   269.3  242.3   273.8   49.5
1974  2729.3  1674.0    8.57    5.5   11.0  481.3  1896.6
       293.9   305.5  269.1   268.4   54.0
1975  2695.0  1711.9    8.83    8.3    9.1  383.3  1931.7
       294.9   364.2  259.7   240.8   59.3
1976  2826.7  1803.9    8.43    7.6    5.8  453.5  2001.0
       340.1   393.7  274.4   285.4   63.1
1977  2958.6  1883.8    8.02    6.9    6.5  521.3  2066.6
       384.1   430.1  281.6   317.1   67.3
1978  3115.2  1961.0    8.73    6.0    7.6  576.9  2167.4
       441.4   470.7  312.6   339.4   72.2
1979  3192.4  2004.4    9.63    5.8   11.3  575.2  2212.6
       505.0   521.1  356.8   353.2   78.6
1980  3187.1  2000.4   11.94    7.0   13.5  509.3  2214.3
       553.8   615.1  388.9   332.0   85.7
1981  3248.8  2024.2   14.17    7.5   10.3  545.5  2248.6
       639.5   703.3  392.7   343.4   94.0
1982  3166.0  2050.7   13.79    9.5    6.2  447.3  2261.5
       635.3   781.2  361.9   335.6  100.0
1983  3279.1  2146.0   12.04    9.5    3.2  504.0  2331.9
       659.9   835.9  348.1   368.1  103.9
1984  3501.4  2249.3   12.71    7.4    4.3  658.4  2469.8
       726.0   895.6  371.8   455.8  107.7
1985  3618.7  2354.8   11.37    7.1    3.6  637.0  2542.8
       788.7   985.6  367.2   471.4  110.9
1986  3717.9  2446.4    9.02    6.9    1.9  639.6  2635.3
       827.9  1034.8  397.1   526.9  113.8
1987  3845.3  2515.8    9.38    6.1    3.6  669.0  2670.7
       913.8  1071.9  451.8   570.3  117.4
1988  4016.9  2606.5    9.71    5.4    4.1  705.7  2800.5
       972.4  1114.2  534.7   610.6  121.3
1989  4117.7  2656.8    9.26    5.2    4.8  716.9  2869.00
      1052.9  1187.2  593.3   647.4  126.3
1990  4155.8  2682.2    9.32    5.4    5.4  690.3  2893.3
      1111.7  1273.0  630.3   667.8  131.5
;
RUN;

* Execute the original model with Durbin-Watson statistic;
data P507.autocorrexample;
set raw;
run;

PROC REG;
  MODEL  import =  cons unemp tax /dwprob;
  OUTPUT R=resid OUT=newdata;
RUN;

* Begin the Cochrane-Orcutt process by generating the lagged residuals;

DATA newdata2;
  SET newdata;
  residl = LAG1(resid);
RUN;

* Using the residuals and lagged residuals, generate a first-round estimate;
* of rho by using proc reg to derive the autocorrelation coefficient as the;
* slope coefficient of the resid variable.;

TITLE2 "First Round Estimate of Rho";
PROC REG plots=none;
  MODEL resid = residl/NOINT;
RUN;


* After running Proc Reg to get the autocorrelation coefficient, the next;
* steps provide the first-difference transforms for the Cochrane-Orcutt;
* procedure.  Note that the first round estimate of rho = .453985;

DATA chor;
   SET newdata;
   rho = .45399;
   import2 = import - rho*LAG1(import);
   cons2 = cons - rho*LAG1(cons);
   unemp2 = unemp - rho*LAG1(unemp);
   tax2 = tax - rho*LAG1(tax);
RUN;

* Now run the transformed regression to generate the WLS estimates from;
* the first round estimate of rho, ignoring the Prais-Winsten transforms.;

TITLE2 "First Round WLS Estimates";
PROC REG plots=none;
   MODEL import2 = cons2 unemp2 tax2 / dwprob;
RUN;

* To use the second round residuals to generate a new estimate of rho             ;
* calculate the second round residuals from the original variables using the      ;
* beta hats obtained from the above WLS regression.  These are:                   ;
* -159.29220/(1-.45399) = -291.73861 for the Intercept, 0.43364 for Consumption;
* -17.00126 for Unemployment, and -0.16067 for Taxes.                           ;

DATA newdata3;
  SET newdata;
  resid2 = import - (-291.73861) - (0.43364*cons) - (-17.00126*unemp) - (-0.16067*tax);
* Lag the second round residuals;
  resid2l = LAG1(resid2);
RUN;

* Now generate the second round estimate of rho by using Proc Reg again;

TITLE2 "Second Round Estimate of Rho";
PROC REG plots=none;
  MODEL resid2 = resid2l/NOINT;
RUN;

* The second round estimate of rho=0.55910. Now take this final estimate;
* and carry out the Weighted Least Squares transforms but without Prais-Winsten;


DATA chor2;
   SET newdata;
   rho = 0.55910;

 import3 = import - rho*lag1(import);
 cons3 = cons - rho*lag1(cons);
 unemp3 = unemp - rho*lag1(unemp);
 tax3 = tax - rho*lag1(tax);

RUN;

*  Execute the Weighted Least Squares regression (finally!);
* Remember to transform the intercept after estimating this by dividing by 1-rho;

PROC REG data=chor2;
  MODEL import3 = cons3 unemp3 tax3 /dwprob;
RUN;

*Calculate R-squared for final model;
DATA new;
  SET raw;
  imphat = -305.510 + 0.42287*cons -15.85432*unemp -0.11626*tax;
RUN;

TITLE "R for final model";
PROC CORR;
  var import imphat;
RUN;

*  Run the SAS PROC AUTOREG procedure for dealing with autocorrelation and compare the estimates;
*  the value of the autocorrelation coefficient, and statistics with those from the;
*  Cochrane-Orcutt procedure.;

TITLE2 "Yule-Walker WLS Estimates Using Proc AutoReg with Tax";
PROC AUTOREG DATA=raw;
      MODEL import = cons unemp tax/METHOD=YW NLAG=1 ITER;
RUN;

*Calculate R-squared for PROC AUTOREG;
DATA new1;
  SET raw;
  imphat2 = -276.9774 + 0.4364*cons -17.8475*unemp-0.1827*tax;
RUN;

TITLE "R for PROC AUTOREG final model";
PROC CORR;
  var import imphat2;
RUN;

QUIT;


