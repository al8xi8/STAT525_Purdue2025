/***********************************************************************
* Course:      STAT 525 - Fall 2025
* Homework:    Homework 4
* Student:     Alexandra Chang
***********************************************************************/

/* ------------------- Question 1 ------------------- */
/* Michaelis-Menten Linearization and Fit */
data enzyme;
    input concentration velocity;
    datalines;
0.02 76
0.02 47
0.06 97
0.06 107
0.11 123
0.11 139
0.22 159
0.22 152
0.56 191
0.56 201
1.10 207
1.10 200
;
run;

data enzyme2;
    set enzyme;
    vinv = 1 / velocity;
    cinv = 1 / concentration;
run;

proc reg data=enzyme2;
    model vinv = cinv;
    output out=reg_out p=vinv_pred r=resid;
    title "Regression: 1/V vs 1/C";
run;
quit;

data reg_out2;
    set reg_out;
    pred_velocity = 1 / vinv_pred;
run;

/* ------------------- Question 2 ------------------- */
/* Regression Through the Origin for Typo Cost */
data typos;
    input x y;
    datalines;
7   128
12  213
10  191
10  178
14  250
25  446
30  540
25  457
18  324
10  177
4   75
6   107
;
run;

proc reg data=typos;
    model y = x / noint;
    title "Regression Through the Origin: Typo Cost vs. Galleys";
run;
quit;

data predict;
    input x;
    datalines;
10
;
run;

data all;
    set typos predict;
run;

proc reg data=all;
    model y = x / noint clm cli alpha=0.02;
    output out=pred_out p=predicted lclm=lcl_mean uclm=ucl_mean
                      lcli=lcl_pred ucli=ucl_pred;
    title "98% Prediction Interval for X = 10 Galleys";
run;
quit;

/* ------------------- Question 6 ------------------- */
/* Brand Preference Data Regression */
data brand_pref;
    input liking moisture sweetness;
    datalines;
64  4 2
73  4 4
61  4 2
76  4 4
72  6 2
80  6 4
71  6 2
83  6 4
83  8 2
89  8 4
86  8 2
93  8 4
88 10 2
95 10 4
94 10 2
100 10 4
;
run;

proc reg data=brand_pref;
    model liking = moisture sweetness;
    title "Regression of Liking on Moisture and Sweetness";
run;
quit;