
/**************************************************************************
 * Course:     STAT 525 - Fall 2025
 * Homework:   Homework 2
 * Student:    Alexandra Chang
 **************************************************************************/

/**************************************************************************
 * Problem 1–3: Done by hand (not using SAS)
 **************************************************************************/

/**************************************************************************
 * Problem 4 – Z-test for hardness using proc reg
 **************************************************************************/
data hardness;
    input hardness time;
    datalines;
199 16
205 16
196 16
200 16
218 24
220 24
215 24
223 24
237 32
234 32
235 32
230 32
250 40
248 40
253 40
246 40
;
run;

proc reg data=hardness;
    model hardness = time / clb alpha=0.01;
run;

/**************************************************************************
 * Problem 5 – CI, PI, WH Band at time = 30
 **************************************************************************/
data hardness;
    input hardness time;
    datalines;
199 16
205 16
196 16
200 16
218 24
220 24
215 24
223 24
237 32
234 32
235 32
230 32
250 40
248 40
253 40
246 40
.   30
;
run;

proc reg data=hardness;
    model hardness = time / clm cli alpha=0.02;
    id time;
    output out=outdata p=yhat stdp=se_fit;
run;

data outdata_wh;
    set outdata;
    alpha = 0.02;
    fval = finv(1 - alpha, 2, 14);
    wh_lower = yhat - sqrt(2 * fval) * se_fit;
    wh_upper = yhat + sqrt(2 * fval) * se_fit;
run;

proc print data=outdata_wh;
    var time yhat wh_lower wh_upper;
run;

/**************************************************************************
 * Problem 6 – Residual plots and regression diagnostics
 **************************************************************************/
data hardness;
    input hardness time;
    datalines;
199 16
205 16
196 16
200 16
218 24
220 24
215 24
223 24
237 32
234 32
235 32
230 32
250 40
248 40
253 40
246 40
;
run;

proc reg data=hardness;
    model hardness = time;
    output out=outreg p=pred r=resid;
run;

proc means data=outreg mean noprint;
    var hardness;
    output out=meanout mean=Ybar;
run;

data outreg2;
    if _n_ = 1 then set meanout;
    set outreg;
    dev_pred = pred - Ybar;
run;

proc sgplot data=outreg2;
    title "Residuals vs. Time";
    scatter x=time y=resid;
run;

proc sgplot data=outreg2;
    title "Regression Deviation (Yhat - Ybar) vs. Time";
    scatter x=time y=dev_pred;
run;

proc sgplot data=hardness;
    title "Hardness vs. Time with Regression Line";
    reg x=time y=hardness;
run;

/**************************************************************************
 * Problem 7 – F statistic from R² = 0.18, n = 25
 **************************************************************************/
data prob7;
    r2 = 0.18;
    n = 25;
    f = (r2 / (1 - r2)) * (n - 2);
    output;
run;

proc print data=prob7; run;
