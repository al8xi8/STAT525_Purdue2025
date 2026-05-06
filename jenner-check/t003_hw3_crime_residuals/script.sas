/**************************************************************************
 * STAT 525 - Fall 2025
 * Homework:    Homework 3
 * Student:     Alexandra Chang
 *
 * (Bundled for Jenner — Box-Cox PROC TRANSREG block omitted; the rest is
 *  the script as written.)
 **************************************************************************/

/**************************************************************************
 * Problem 4: Analysis of hs_diploma_pct and crime_rate
 **************************************************************************/

data crime;
    input crime_rate hs_diploma_pct;
    datalines;
   8487    74
   8179    82
   8362    81
   8220    81
   6246    87
   9100    66
   6561    68
   5873    81
   7993    74
   7932    82
   6491    75
   6816    82
   9639    78
   4595    84
   5037    82
   4427    79
   6226    78
  10768    73
   8335    77
  12311    65
  10104    77
  10503    76
   7562    79
   8593    79
   7133    78
  10205    84
  14016    78
   5959    81
   3764    89
   4297    85
   7562    77
   4844    74
   5777    80
   3599    84
   3219    88
  11187    75
   2105    77
   6650    78
  11371    61
   4517    91
   7348    83
   5696    77
   4995    85
   9248    70
   6860    88
   9776    80
   4280    82
  11154    82
   3442    82
   9674    70
   7309    64
   4530    79
   4017    83
   7122    77
   5689    76
   6109    80
   3343    84
   5029    82
   4330    81
   5425    74
   8769    81
   6880    76
   6538    78
   6521    78
   9423    79
   9697    83
   3805    79
   3134    83
   3433    81
   2979    84
   6836    64
   5804    67
   7986    75
  10994    73
  11322    77
   8937    64
   8807    75
  11087    80
  10355    83
   7858    85
   3632    91
   8040    88
   6981    83
   7582    76
;
run;

/* 4a: Descriptive statistics */
proc means data=crime mean std var min max median q1 q3;
    var crime_rate;
run;

/* 4b: Scatterplot */
proc sgplot data=crime;
    scatter x=hs_diploma_pct y=crime_rate;
    xaxis label="High School Diploma Percentage";
    yaxis label="Crime Rate";
    title "Scatterplot of Crime Rate vs. High School Diploma %";
run;

/* 4c: Linear Regression */
proc reg data=crime;
    model crime_rate = hs_diploma_pct;
    output out=resid_data r=residuals;
run;
quit;

/* 4d: Residual plots */
proc sgplot data=resid_data;
    scatter x=hs_diploma_pct y=residuals;
    refline 0 / axis=y lineattrs=(color=red);
    title "Residuals vs. HS Diploma Percentage";
run;

proc univariate data=resid_data;
    var residuals;
    histogram;
    qqplot;
    title "Distribution of Residuals";
run;


/**************************************************************************
 * Problem 5: Typo Impact Analysis
 **************************************************************************/

/* 5a: Introduce typo */
data crime_typo;
    set crime;
    if _N_ = 84 then crime_rate = 758;
run;

/* 5b: Regression with typo */
proc reg data=crime_typo;
    model crime_rate = hs_diploma_pct;
    output out=resid_typo r=residuals;
run;
quit;

/* 5b continued: Residual plots with typo */
proc sgplot data=resid_typo;
    scatter x=hs_diploma_pct y=residuals;
    refline 0 / axis=y;
    title "Residuals vs. HS Diploma Percentage (with Typo)";
run;

proc univariate data=resid_typo;
    var residuals;
    histogram;
    qqplot;
    title "Distribution of Residuals (with Typo)";
run;


/**************************************************************************
 * Problem 6: Sales Growth — sqrt transformation
 **************************************************************************/

data sales;
    input sales_growth time;
    datalines;
  98.0    0.0
  135.0    1.0
  162.0    2.0
  178.0    3.0
  221.0    4.0
  232.0    5.0
  283.0    6.0
  300.0    7.0
  374.0    8.0
  395.0    9.0
;
run;

data sales_transformed;
    set sales;
    sqrt_sales = sqrt(sales_growth);
run;

proc reg data=sales_transformed;
    model sqrt_sales = time;
    title "Regression: sqrt(Sales Growth) vs. Time";
run;
quit;
