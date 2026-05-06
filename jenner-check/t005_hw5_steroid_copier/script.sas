/**************************************************************************
 * Course:      STAT 525 – Fall 2025
 * Homework:    Homework 5 – Questions 5 & 6 (steroid + copier service time)
 * Student:     Alexandra Chang
 *
 * (Bundled for Jenner — Q1 needs an external `satisfaction` dataset, and
 *  Q2/Q3 are placeholders in the original; bundle keeps the two problems
 *  with self-contained DATALINES.)
 **************************************************************************/

/**************************************************************************
 * Question 5: Steroid example (KNNL Problem 8.6)
 **************************************************************************/

/* 5a. Quadratic regression */
data steroid;
    input y x;
    x2 = x*x;
    datalines;
27.1 23
22.1 19
21.9 25
10.7 12
1.4 8
18.8 12
14.7 11
5.7 8
18.6 17
20.4 18
9.2 9
23.4 21
10.5 10
19.7 25
11.8 9
24.6 17
3.4 9
22.8 23
21.1 13
24.0 14
21.8 16
23.5 17
19.4 21
25.6 24
12.8 13
20.8 14
20.6 18
;
run;

proc reg data=steroid;
    model y = x x2 / clb;
    title "5a. Quadratic Regression Model";
run;
quit;

/* 5b. Plot fit */
proc sgplot data=steroid;
    scatter x=x y=y;
    reg x=x y=y / degree=2;
    title "5b. Quadratic Fit Plot";
run;

/* 5c. Working-Hotelling 99% joint CI */
data new;
    input x;
    x2 = x*x;
    y = .;
    datalines;
10
15
20
;
run;

data all;
    set steroid new;
run;

proc reg data=all;
    model y = x x2 / alpha=0.01 clm;
    id x y x2;
    output out=ci_out p=pred stdp=stderr;
run;

data joint_CI;
    set ci_out;
    if y = .;
    fval = finv(0.99, 2, 24);
    WH = sqrt(2 * fval);
    lowerWH = pred - WH * stderr;
    upperWH = pred + WH * stderr;
    keep x pred stderr lowerWH upperWH;
run;

proc print data=joint_CI noobs;
    title "5c. Working-Hotelling 99% CIs for E(y)";
run;

/**************************************************************************
 * Question 6: Copier Service Time (KNNL Problem 8.15)
 **************************************************************************/

data copier;
    input y x1 x2;
    datalines;
20 2 1
60 4 0
46 3 0
41 2 0
12 1 0
137 10 0
68 5 1
89 5 1
4 1 1
32 2 1
144 9 1
156 10 0
93 6 0
36 3 0
72 4 1
100 8 0
105 7 0
131 8 0
127 10 0
57 4 0
66 5 0
101 7 1
109 7 1
74 5 0
134 9 1
112 7 0
18 2 0
73 5 0
111 7 0
96 6 0
123 8 1
90 5 1
20 2 1
28 2 1
3 1 0
57 4 0
86 5 0
132 9 0
112 7 0
27 1 0
131 9 1
34 2 1
27 2 0
61 4 0
77 5 0
;
run;

proc reg data=copier;
    model y = x1 x2 / clb;
    title "6b. Regression: Service Time vs Copiers and Type";
run;
quit;

/* 6e. Add interaction term and refit */
data copier2;
    set copier;
    x1x2 = x1 * x2;
run;

proc reg data=copier2;
    model y = x1 x2 x1x2 / clb;
    title "6e. Regression with Interaction Term";
run;
quit;
