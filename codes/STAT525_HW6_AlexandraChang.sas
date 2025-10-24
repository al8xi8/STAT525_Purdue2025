/**************************************************************************
 * Course:   STAT 525 – Fall 2025
 * Homework: Homework 6
 * Problems: 9.13, 9.14, 9.23, and 10.20 (Parts a–f)
 * Author:   Alexandra Chang
 **************************************************************************/


/**************************************************************************
 * Problem 9.13 – Lung Pressure Data
 **************************************************************************/

data lungpressure;
    input Y X1 X2 X3;
    datalines;
49  31  44  8
55  34  51  7
85  48  60  8
32  24  40  9
26  25  39  6
28  26  41  7
95  52  61  8
26  28  44  8
74  47  55  8
37  28  47  7
31  25  42  6
49  32  48  7
38  27  45  6
41  30  46  7
12  24  35  5
44  30  46  8
29  28  42  6
40  28  46  6
31  27  43  6
;
run;


/**************************************************************************
 * 9.13(a) – Boxplots and Histograms for X1, X2, X3
 **************************************************************************/

proc sgplot data=lungpressure;
    vbox X1;
    title "Boxplot of X1 (Emptying Rate)";
run;

proc sgplot data=lungpressure;
    vbox X2;
    title "Boxplot of X2 (Ejection Rate)";
run;

proc sgplot data=lungpressure;
    vbox X3;
    title "Boxplot of X3 (Blood Gas Variable)";
run;

proc univariate data=lungpressure normal;
    var X1 X2 X3;
    histogram X1 X2 X3 / normal;
    inset mean std skewness kurtosis / position=ne;
    title "Histograms of X1, X2, X3";
run;


/**************************************************************************
 * 9.13(b) – Scatterplot Matrix and Correlation Matrix
 **************************************************************************/

proc sgscatter data=lungpressure;
    matrix Y X1 X2 X3 / diagonal=(histogram);
    title "Scatterplot Matrix for Y, X1, X2, and X3";
run;

proc corr data=lungpressure plots=matrix(histogram);
    var Y X1 X2 X3;
    title "Correlation Matrix of Y and X Variables";
run;


/**************************************************************************
 * 9.13(c) – Multiple Regression (First-Order Model)
 **************************************************************************/

proc reg data=lungpressure;
    model Y = X1 X2 X3 / vif tol;
    title "Multiple Regression Model for Lung Pressure (Y = X1 + X2 + X3)";
run;
quit;


/**************************************************************************
 * Problem 9.14 – Hierarchical Subset Regression (First + Second Order)
 **************************************************************************/

/* Step 1: Compute means for centering */
proc means data=lungpressure noprint;
    var X1 X2 X3;
    output out=means mean=meanX1 meanX2 meanX3;
run;

/* Step 2: Center variables */
data lungpressure_centered;
    if _n_ = 1 then set means;
    set lungpressure;
    x1c = X1 - meanX1;
    x2c = X2 - meanX2;
    x3c = X3 - meanX3;
run;

/* Step 3: Create second-order terms */
data lungpressure2;
    set lungpressure_centered;
    x1sq = x1c*x1c;
    x2sq = x2c*x2c;
    x3sq = x3c*x3c;
    x1x2 = x1c*x2c;
    x1x3 = x1c*x3c;
    x2x3 = x2c*x3c;
run;

/* Step 4: All-subsets regression (Adjusted R² method) */
proc reg data=lungpressure2;
    model Y = x1c x2c x3c x1sq x2sq x3sq x1x2 x1x3 x2x3 /
              selection=adjrsq best=3;
    title "Best Three Hierarchical Subset Models by Adjusted R-Square";
run;
quit;


/**************************************************************************
 * Problem 9.23 – PRESS Statistic
 **************************************************************************/

proc reg data=lungpressure2 outest=sumstats press;
    model Y = x1c x2c x1x2;
    title "PRESS Statistic for Best Model (x1c, x2c, x1x2)";
run;

proc print data=sumstats;
    var _MODEL_ _DEPVAR_ _RMSE_ _PRESS_ Intercept x1c x2c x1x2;
    title "PRESS Summary Output";
run;
quit;


/**************************************************************************
 * Problem 10.20(a) – Residual and Fitted Plots
 **************************************************************************/

proc reg data=lungpressure2;
    model Y = x1c x2c x1x2;
    output out=residuals r=resid p=pred;
    title "Residuals and Fitted Values for Y = x1c + x2c + x1x2";
run;

proc sgscatter data=residuals;
    matrix resid pred x1c x2c x1x2;
    title "Residuals vs. Fitted and Predictor Variables";
run;

proc sgplot data=residuals;
    scatter x=pred y=resid;
    refline 0 / axis=y;
    title "Residuals vs. Fitted Values";
run;


/**************************************************************************
 * 10.20(b) – Normal Probability Plot and Tests
 **************************************************************************/

proc univariate data=residuals normal plot;
    var resid;
    qqplot resid / normal(mu=est sigma=est);
    inset mean std skewness kurtosis / position=ne;
    title "Normal Probability Plot and Tests for Residuals";
run;


/**************************************************************************
 * 10.20(c) – Variance Inflation Factors (Multicollinearity)
 **************************************************************************/

proc reg data=lungpressure2;
    model Y = x1c x2c x1x2 / vif tol;
    title "Variance Inflation Factors for X1c, X2c, and X1x2";
run;
quit;


/**************************************************************************
 * 10.20(d) – Studentized Deleted Residuals and Bonferroni Test
 **************************************************************************/

proc reg data=lungpressure2;
    model Y = x1c x2c x1x2 / r influence;
    output out=outres rstudent=rstud;
    title "Studentized Deleted Residuals and Influence Diagnostics";
run;

proc print data=outres;
    var Y rstud;
    title "Studentized Deleted Residuals";
run;
quit;


/**************************************************************************
 * 10.20(e) – Hat Matrix Diagonals (Leverage)
 **************************************************************************/

proc reg data=lungpressure2;
    model Y = x1c x2c x1x2 / influence;
    title "Hat Matrix Diagonals (Leverage) and Influence Diagnostics";
run;
quit;


/**************************************************************************
 * 10.20(f) – Influence Diagnostics (DFFITS, DFBETAS, Cook’s Distance)
 **************************************************************************/

proc reg data=lungpressure2;
    model Y = x1c x2c x1x2 / influence r;
    output out=influence_out
        cookd = D
        dffits = DFFITS
        dfbetas_intercept = DFB_Int
        dfbetas_x1c = DFB_X1C
        dfbetas_x2c = DFB_X2C
        dfbetas_x1x2 = DFB_X1X2;
    title "Influence Diagnostics: DFFITS, DFBETAS, and Cook's D";
run;

proc print data=influence_out;
    var Y DFFITS D DFB_Int DFB_X1C DFB_X2C DFB_X1X2;
    title "Influence Output for All Observations";
run;
quit;
