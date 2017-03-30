*P507 NEAR MULTICOLLINEARITY IN-CLASS EXAMPLE REGRESSIONS;

TITLE "P507 NEAR MULTICOLLINEARITY IN-CLASS EXAMPLE REGRESSIONS";
LIBNAME P507 "I:P507\Datasets\";

DATA P507.airpol;
INPUT cities$ so temp me pop wind precip days;
DATALINES;
Phoenix 10 70.3 213 582 6.0 7.05 36
Little_Rock 13 61.0 91 132 8.2 48.52 100
San_Francisco 12 56.7 453 716 8.7 20.66 67
Denver 17 51.9 454 515 9.0 12.95 86
Hartford 56 49.1 412 158 9.0 43.37 127
Wilmington 36 54.0 80 80 9.0 40.25 114
Washington 29 57.3 434 757 9.3 38.89 111
Jacksonville 14 68.4 136 529 8.8 54.47 116
Miami 10 75.5 207 335 9.0 59.80 128
Atlanta 24 61.5 368 497 9.1 48.34 115
Chicago 110 50.6 3344 3369 10.4 34.44 122
Indianapolis 28 52.3 361 746 9.7 38.74 121
Des_Moines 17 49.0 104 201 11.2 30.85 103
Wichita 8 56.6 125 277 12.7 30.58 82
Louisville 30 55.6 291 593 8.3 43.11 123
New_Orleans 9 68.3 204 361 8.4 56.77 113
Baltimore 47 55.0 625 905 9.6 41.31 111
Detroit 35 49.9 1064 1513 10.1 30.96 129
Minneapolis-St._Paul 29 43.5 669 744 10.6 25.94 137
Kansas_City 14 54.5 381 507 10.0 37.00 99
St_Louis 56 55.9 775 622 9.5 35.89 105
Omaha 14 51.5 181 347 10.9 30.18 98
Albuquerque 11 56.8 46 244 8.9 7.77 58
Albany 46 47.6 44 116 8.8 33.36 135
Buffalo 11 47.1 391 463 12.4 36.11 166
Cincinnati 23 54.0 462 453 7.1 39.04 132
Cleveland 65 49.7 1007 751 10.9 34.99 155
Columbus 26 51.5 266 540 8.6 37.01 134
Philadelphia 69 54.6 1692 1950 9.6 39.93 115
Pittsburg 61 50.4 347 520 9.4 36.22 147
Providence 94 50.0 343 179 10.6 42.75 125
Memphis 10 61.6 337 624 9.2 49.10 105
Nashville 18 59.4 275 448 7.9 46.00 119
Dallas 9 66.2 641 844 10.9 35.94 78
Houston 10 68.9 721 1233 10.8 48.19 103
Salt_Lake_City 28 51.0 137 176 8.7 15.17 89
Norfolk 31 59.3 96 308 10.6 44.68 116
Richmond 26 57.8 197 299 7.6 42.59 115
Seattle 29 51.1 379 531 9.4 38.79 164
Charleston 31 55.2 35 71 6.5 40.75 148
Milwaukee 16 45.7 569 717 11.8 29.07 123
;
RUN;

PROC CORR;
  VAR so temp me pop wind precip days ;
RUN;

PROC REG plots=none;
  MODEL so = temp me pop wind precip days/stb tol vif collin;
RUN;

PROC REG plots=none;
  MODEL so = temp me pop wind precip days/stb tol vif collinoint;
RUN;

*Drop out me and then pop to see effects;
PROC REG plots=none;
  MODEL so = temp pop wind precip days/stb collinoint tol vif;
  MODEL so = temp me wind precip days/stb collinoint tol vif;
RUN;

*Auxiliary Regressions;
TITLE2 "Auxiliary Regressions";
PROC REG plots=none;
  model me   = temp pop wind precip days/stb;
  model pop  = temp me wind precip days/stb;
RUN;

DATA TEMP;
  SET P507.airpol;
  mepc=me/pop;
RUN;

TITLE2 "Final Regressions";
PROC REG plots=none;
  MODEL so = temp mepc pop wind precip days/stb collinoint vif;
run;

PROC REG plots=none;
  MODEL so = temp mepc wind precip days/stb collinoint vif;
run;

QUIT;
