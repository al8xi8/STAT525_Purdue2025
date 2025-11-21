/**************************************************************************
 * Course:   STAT 525 - Fall 2025
 * Homework: Homework 9
 * Name:     Alexandra Chang
 **************************************************************************/


/**************************************************************************
 * ========================================================================
 * QUESTION 1 – Problem 19.5
 * Two-Factor Study (2×4)
 * ========================================================================
 **************************************************************************/

data twofactor;
    input A $ B $ y;
    datalines;
A1 B1 250
A1 B2 265
A1 B3 268
A1 B4 269
A2 B1 288
A2 B2 273
A2 B3 270
A2 B4 269
;
run;

/* (1a) Factor B marginal means */
proc means data=twofactor mean;
    class B;
    var y;
    title "(1a) Factor B Marginal Means – Problem 19.5(a)";
run;

/* (1b) Means plot + ANOVA */
proc means data=twofactor noprint;
    class A B;
    var y;
    output out=plotdata mean=mean_y;
run;

symbol1 v=circle i=join c=black;
symbol2 v=square i=join c=black;

proc gplot data=plotdata;
    plot mean_y*B = A / frame;
    title "(1b) Interaction Plot of Treatment Means – Problem 19.5(b)";
run; quit;

proc glm data=twofactor;
    class A B;
    model y = A B A*B;
    means A B A*B;
    title "(1b) Two-Factor ANOVA – Problem 19.5(b)";
run; quit;

/* (1c) Log transformation */
data logtf;
    set twofactor;
    logy = log(y);
run;

proc means data=logtf noprint;
    class A B;
    var logy;
    output out=plotlog mean=mean_log;
run;

proc gplot data=plotlog;
    plot mean_log*B = A / frame;
    title "(1c) Interaction Plot of log(y) – Problem 19.5(c)";
run; quit;

proc glm data=logtf;
    class A B;
    model logy = A B A*B;
    means A B A*B;
    title "(1c) Two-Factor ANOVA on log(y) – Problem 19.5(c)";
run; quit;


/**************************************************************************
 * ========================================================================
 * QUESTION 2 – Problems 19.14 and 19.15
 * Hay Fever Relief Data
 * ========================================================================
 **************************************************************************/

data hay;
    input y A B rep;
    datalines;
2.4 1 1 1
2.7 1 1 2
2.3 1 1 3
2.5 1 1 4
4.6 1 2 1
4.2 1 2 2
4.9 1 2 3
4.7 1 2 4
4.8 1 3 1
4.5 1 3 2
4.4 1 3 3
4.6 1 3 4
5.8 2 1 1
5.2 2 1 2
5.5 2 1 3
5.3 2 1 4
8.9 2 2 1
9.1 2 2 2
8.7 2 2 3
9.0 2 2 4
9.1 2 3 1
9.3 2 3 2
8.7 2 3 3
9.4 2 3 4
6.1 3 1 1
5.7 3 1 2
5.9 3 1 3
6.2 3 1 4
9.9 3 2 1
10.5 3 2 2
10.6 3 2 3
10.1 3 2 4
13.5 3 3 1
13.0 3 3 2
13.3 3 3 3
13.2 3 3 4
;
run;

/* (2a) Fit model and save fitted + residuals */
proc glm data=hay;
    class A B;
    model y = A B A*B;
    output out=fitout p=fitted r=resid;
    title "(2a) Fitted Values and Residuals – Model (19.23)";
run; quit;

proc print data=fitout;
    var y A B rep fitted resid;
    title "(2a) Displaying Fitted Values and Residuals";
run;

/* (2c) Residual plot */
symbol1 v=circle i=none c=black;

proc gplot data=fitout;
    plot resid * fitted / frame;
    title "(2c) Residuals vs Fitted Plot – Problem 19.15(c)";
run; quit;

/* (2d) Normal probability plot */
proc univariate data=fitout normal;
    var resid;
    qqplot resid / normal(mu=est sigma=est);
    title "(2d) Normal Probability Plot – Problem 19.15(d)";
run;

/* (2b) ANOVA table */
proc glm data=hay;
    class A B;
    model y = A B A*B;
    title "(2b) ANOVA Table – Problem 19.15(b)";
run; quit;

/* (2e) Kimball inequality */
data kimball;
    alpha = 0.05;
    k = 3;
    alpha_f = 1 - (1 - alpha)**k;
    put "Kimball family-wise alpha upper bound = " alpha_f;
run;


/**************************************************************************
 * ========================================================================
 * QUESTION 3 – Problem 19.23
 * (Continuation of Hay dataset modeling)
 * ========================================================================
 **************************************************************************/
/* Note: Already covered under Question 2 (model fitting, residuals, plots) */


/**************************************************************************
 * ========================================================================
 * QUESTION 4 – Problem 19.28
 * (Conceptual – no SAS code required)
 * ========================================================================
 **************************************************************************/
/*
   Problem 19.28 is theoretical.
   No SAS code is required.
*/


/**************************************************************************
 * ========================================================================
 * QUESTION 5 – Problem 5
 * Tile Strength — Tukey & Contrasts
 * ========================================================================
 **************************************************************************/

data tiles;
    input Temp Oven y;
    datalines;
5 1 5
5 2 10
5 3 7
5 4 4
5 5 3
10 1 3
10 2 8
10 3 12
10 4 2
10 5 8
15 1 9
15 2 13
15 3 15
15 4 4
15 5 10
20 1 7
20 2 12
20 3 9
20 4 6
20 5 13
;
run;

/*** (5a) Tukey multiple comparisons ***/
proc glm data=tiles;
    class Temp Oven;
    model y = Temp Oven;
    means Temp / tukey alpha=0.05;
    means Oven / tukey alpha=0.05;
    title "(5a) Tukey Pairwise Comparisons for Temp and Oven – Problem 5(a)";
run; quit;

/*** (5c) Step-function temperature contrasts ***/
proc glm data=tiles;
    class Temp Oven;
    model y = Temp Oven;

    contrast 'Jump at 12.5C' Temp -1 -1 1 1;
    contrast '5C vs 10C'     Temp 1 -1 0 0;
    contrast '15C vs 20C'    Temp 0 0 1 -1;

    title "(5c) Three Temperature Contrasts – Problem 5(c)";
run; quit;


/**************************************************************************
 * ========================================================================
 * QUESTION 6 – Problem 20.4
 * Tukey Test for Additivity
 * ========================================================================
 **************************************************************************/

data terminal;
    input y A B;
    datalines;
16.5 1 1
21.4 1 2
11.8 2 1
17.3 2 2
12.3 3 1
16.9 3 2
16.6 4 1
21.0 4 2
;
run;

proc glm data=terminal;
    class A B;
    model y = A B A*B;
    random A B;
    test H=A*B E=A;
    title "(6) Tukey Test for Additivity – Problem 20.4";
run; quit;
