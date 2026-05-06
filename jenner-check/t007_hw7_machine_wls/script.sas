/**************************************************************************
 * Course:   STAT 525 - Fall 2025
 * Homework: Homework 7
 * Name:     Alexandra Chang
 **************************************************************************/

options nocenter linesize=80;

/**************************************************************************
 * Problem: Machine Speed vs Number of Defectives
 **************************************************************************/

/*-----------------------------------------------------------
  Input Data
-----------------------------------------------------------*/
data machinespeed;
    input y x;
    datalines;
28 200
75 400
37 300
53 400
22 200
58 300
40 300
96 400
46 200
52 400
30 200
69 300
;
run;

/*-----------------------------------------------------------
  (a) Fit OLS Regression and Compute Residuals
-----------------------------------------------------------*/
proc reg data=machinespeed;
    model y = x;
    output out=residdata r=residual p=predicted;
run;

proc print data=residdata;
    var x y predicted residual;
    title "Observed, Predicted, and Residual Values";
run;

/*-----------------------------------------------------------
  (b) Plot Residuals vs X
-----------------------------------------------------------*/
symbol v=circle i=none;
proc gplot data=residdata;
    plot residual*x / frame;
    title "Residual Plot for Machine Speed vs Number of Defectives";
    title2 "Checking for Unequal Error Variance (Ch. 11 Concept)";
run;

/*-----------------------------------------------------------
  (c) Plot Squared Residuals vs X
-----------------------------------------------------------*/
data resid_sqr;
    set residdata;
    sq_resid = residual**2;
run;

symbol v=circle i=none;
proc gplot data=resid_sqr;
    plot sq_resid*x / frame;
    title "Squared Residuals vs Machine Speed (X)";
    title2 "Examining Relation Between Error Variance and X (Ch. 11 Concept)";
run;

/*-----------------------------------------------------------
  (d) Estimate Variance Function σ̂²ᵢ = f(Xᵢ) and Compute Weights
-----------------------------------------------------------*/
proc reg data=resid_sqr;
    model sq_resid = x;
    output out=varfit p=vhat;
run; quit;

data wts;
    set varfit;
    if vhat < 1e-6 then vhat = 1e-6;
    w = 1 / vhat;
run;

proc print data=wts noobs;
    var x y residual sq_resid vhat w;
    title "Weights via (11.16b): wᵢ = 1 / σ̂²ᵢ";
run;

/*-----------------------------------------------------------
  (e) OLS vs WLS Comparison
-----------------------------------------------------------*/
ods output ParameterEstimates=ols_pe;
proc reg data=machinespeed;
    model y = x;
run; quit;

ods output ParameterEstimates=wls_pe;
proc reg data=wts;
    model y = x / clb;
    weight w;
    output out=wlsout r=res_w p=pred_w;
    title "WLS Fit using wᵢ = 1/σ̂²ᵢ (Eq. 11.16b)";
run; quit;

data ols_pe;  set ols_pe;  Method = "OLS";  keep Method Variable Estimate StdErr tValue Probt; run;
data wls_pe;  set wls_pe;  Method = "WLS";  keep Method Variable Estimate StdErr tValue Probt; run;

data compare; set ols_pe wls_pe; run;

proc print data=compare noobs;
    title "Comparison of OLS and WLS Estimates";
run;

/*-----------------------------------------------------------
  (f) Reduction in Standard Errors
-----------------------------------------------------------*/
data ols_results;
    input Method $ Intercept SE_Intercept Slope SE_Slope;
    datalines;
OLS -5.7500 16.7305 0.1875 0.05381
;
run;

data wls_results;
    input Method $ Intercept SE_Intercept Slope SE_Slope;
    datalines;
WLS -6.2332 13.1684 0.1891 0.05056
;
run;

data compare_SE; set ols_results wls_results; run;

proc sql;
    create table se_change as
    select
        a.SE_Intercept as SE_Intercept_OLS,
        b.SE_Intercept as SE_Intercept_WLS,
        100*(a.SE_Intercept - b.SE_Intercept)/a.SE_Intercept as Pct_Reduction_Intercept format=6.2,
        a.SE_Slope as SE_Slope_OLS,
        b.SE_Slope as SE_Slope_WLS,
        100*(a.SE_Slope - b.SE_Slope)/a.SE_Slope as Pct_Reduction_Slope format=6.2
    from ols_results a, wls_results b;
quit;

title "Comparison of Standard Errors (OLS vs WLS)";
proc print data=se_change noobs label;
    label SE_Intercept_OLS = "OLS SE(b₀)"
          SE_Intercept_WLS = "WLS SE(b₀)"
          Pct_Reduction_Intercept = "% Reduction in SE(b₀)"
          SE_Slope_OLS = "OLS SE(b₁)"
          SE_Slope_WLS = "WLS SE(b₁)"
          Pct_Reduction_Slope = "% Reduction in SE(b₁)";
run;

/*-----------------------------------------------------------
  (g) Iterated WLS
-----------------------------------------------------------*/
data iter1;
    set wlsout;
    sq_resid_wls = res_w**2;
run;

proc reg data=iter1;
    model sq_resid_wls = x;
    output out=iter_varfit p=vhat2;
run; quit;

data iter_wts;
    set iter_varfit;
    if vhat2 < 1e-6 then vhat2 = 1e-6;
    w2 = 1 / vhat2;
run;

proc reg data=iter_wts;
    model y = x;
    weight w2;
    title "Iterated WLS (2nd round) using new weights";
run; quit;

/*-----------------------------------------------------------
  (h) OLS on Transformed System (Eq. 11.23)
-----------------------------------------------------------*/
ods output ParameterEstimates=wls_pe;
proc reg data=wts;
    model y = x;
    weight w;
    title "WLS (reference)";
run; quit;

data trans;
    set wts;
    sw = sqrt(w);
    yw = sw * y;
    xw = sw * x;
    iw = sw;
run;

ods output ParameterEstimates=trans_pe;
proc reg data=trans;
    model yw = iw xw / noint;
    title "OLS on W^{1/2}-transformed system (Eq. 11.23)";
run; quit;

data wls_pe;   set wls_pe;   Method="WLS";        keep Method Variable Estimate; run;
data trans_pe; set trans_pe; Method="Trans-OLS";  keep Method Variable Estimate; run;

data compare_1116; set wls_pe trans_pe; run;

proc print data=compare_1116 noobs;
    title "Compare coefficients: WLS vs OLS on W^{1/2}-transformed system";
run;
