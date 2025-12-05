/**************************************************************************
 * STAT 525 – Homework 10
 * Student: Alexandra Chang
 *
 * This file contains SAS code for:
 * 1. Problem 23.7(e): Interaction Test and Scheffé CIs (using original data with missing values)
 * 2. Problem 23.12(a): Bonferroni CIs for L1, L2 (μ22 removed, first dataset)
 * 3. Problem 23.12(b): Single-df Tests for μ12=μ13 and μ32=μ33 (μ22 removed, second dataset)
 * 4. Problem 24.14(d)-(e): Electronics Assembly - Model Fit and Residual Diagnostics
 * 5. Problem 24.12: AB Interaction Plots by Level of C
 **************************************************************************/

/* Reset options and titles */
options nocenter ls=80 pageno=1;
title;

/* --- Problem 23.7(e) --- */
/* Hayfever data with missing values */
data hayfever_original;
    input y A B rep;
    datalines;
    2.4 1 1 1
    2.7 1 1 2
    .   1 1 3       /* Y113 missing */
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
    .   2 2 1       /* Y221 missing */
    9.1 2 2 2
    8.7 2 2 3
    .   2 2 4       /* Y224 missing */
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

/* 1. Fit full two-factor model (with missing values) */
proc glm data=hayfever_original;
    class A B;
    model y = A B A*B;
    title "23.7(e) - Full Model: Includes A, B, and Interaction A*B (Original Data)";
run;

/* 1. Reduced model (no interaction) (with missing values) */
proc glm data=hayfever_original;
    class A B;
    model y = A B;
    title "23.7(e) - Reduced Model: Main Effects Only (Original Data)";
run;

/* 2. Fit full two-factor model and capture MSE and df_Error for Scheffé */
ods output OverallANOVA=anova;
proc glm data=hayfever_original;
    class A B;
    model y = A B A*B;
    title "23.7(e) - Full Model for Hay Fever Data - ANOVA Table (for ODS)";
run;
quit;
ods output close;

/* Pull MSE and dfE from the 'Error' row */
data _null_;
    set anova;
    if Source='Error' then do;
        call symputx('MSE', MeanSq);
        call symputx('dfE', DF);
    end;
run;

/* 3. Get the 9 cell means μ_ij (A×B means) from the original data */
proc means data=hayfever_original nway noprint;
    class A B;
    var y;
    output out=cellmeans mean=mu;
run;

/* Hard-coded values based on prompt's subsequent steps (for Scheffé demo) */
%let MSE_SCHEFFE = 0.0611852;
%let dfE_SCHEFFE = 27;

/* 4. Compute L1–L6 and Scheffé CIs */
data scheffe;
    set cellmeans end=last;

    retain mu11 mu12 mu13
           mu21 mu22 mu23
           mu31 mu32 mu33;

    array muij[3,3]
           mu11 mu12 mu13
           mu21 mu22 mu23
           mu31 mu32 mu33;

    muij[A,B] = mu;

    if last then do;
        /* contrasts */
        L1 = (mu12 + mu13)/2 - mu11;
        L2 = (mu22 + mu23)/2 - mu21;
        L3 = (mu32 + mu33)/2 - mu31;
        L4 = L2 - L1;
        L5 = L3 - L1;
        L6 = L3 - L2;

        /* standard errors, assuming n_ij = 4 for all cells involved */
        seL1 = sqrt(&MSE_SCHEFFE * 0.375);
        seL2 = seL1;
        seL3 = seL1;

        seL4 = sqrt(&MSE_SCHEFFE * 0.75);
        seL5 = seL4;
        seL6 = seL4;

        /* Scheffe multiplier: K = 9 cell means, alpha=0.10, df_num=K-1=8 */
        Scrit = sqrt( (9-1) * finv(0.90, 8, &dfE_SCHEFFE) );

        /* confidence intervals */
        L1_low = L1 - Scrit*seL1;  L1_high = L1 + Scrit*seL1;
        L2_low = L2 - Scrit*seL2;  L2_high = L2 + Scrit*seL2;
        L3_low = L3 - Scrit*seL3;  L3_high = L3 + Scrit*seL3;
        L4_low = L4 - Scrit*seL4;  L4_high = L4 + Scrit*seL4;
        L5_low = L5 - Scrit*seL5;  L5_high = L5 + Scrit*seL5;
        L6_low = L6 - Scrit*seL6;  L6_high = L6 + Scrit*seL6;

        output;
    end;
run;

/* 5. Print the Scheffé results for L1–L6 */
proc print data=scheffe noobs;
    var L1 L1_low L1_high
        L2 L2_low L2_high
        L3 L3_low L3_high
        L4 L4_low L4_high
        L5 L5_low L5_high
        L6 L6_low L6_high;
    title "23.7(e) - Scheffé 90% Confidence Intervals for L1–L6";
run;

/* --- Imputed Data (Used for context, but not a main step) --- */
data hayfever_full;
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

proc glm data=hayfever_full;
    class A B;
    model y = A B A*B;
    title "23.12 - Full Model (A, B, A*B) - Imputed Data (Context)";
run;

proc glm data=hayfever_full;
    class A B;
    model y = A B;
    title "23.12 - Reduced Model (Main Effects Only) - Imputed Data (Context)";
run;
quit;

/* --- Problem 23.12(a) - Bonferroni CIs for L1 and L2 --- */

data hay12_part_a;
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
/* removed A=2 B=2 */
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

/* Hard–code MSE and dfE for the partial data */
%let MSE_2312 = 0.0611852;
%let dfE_2312 = 24;

/* Get cell means */
proc means data=hay12_part_a nway noprint;
    class A B;
    var y;
    output out=cellmeans_2312 mean=mu;
run;

/* Compute contrasts and Bonferroni CIs */
data contrasts_2312;
    set cellmeans_2312 end=last;

    retain mu11 mu13 mu21 mu23 mu31 mu33;

    if A=1 and B=1 then mu11 = mu;
    if A=1 and B=3 then mu13 = mu;

    if A=2 and B=1 then mu21 = mu;
    if A=2 and B=3 then mu23 = mu;

    if A=3 and B=1 then mu31 = mu;
    if A=3 and B=3 then mu33 = mu;

    if last then do;
        /* Contrasts D = mu_i3 - mu_i1 */
        D1 = mu13 - mu11;
        D2 = mu23 - mu21;
        D3 = mu33 - mu31;

        /* Interaction contrasts L1, L2 */
        L1 = D1 - D2;
        L2 = D1 - D3;

        /* Standard errors: n_ij = 4 */
        seD = sqrt(&MSE_2312 * (1/4 + 1/4)); 
        seL = sqrt(2 * &MSE_2312 * 0.5);

        /* Bonferroni t critical: alpha=0.10, m=2 (for L1, L2) → per test alpha=0.05 */
        tcrit = tinv(1 - 0.05/2, &dfE_2312);

        /* CIs for L1 and L2 */
        L1_low = L1 - tcrit*seL;    L1_high = L1 + tcrit*seL;
        L2_low = L2 - tcrit*seL;    L2_high = L2 + tcrit*seL;

        output;
    end;
run;

/* Print the Bonferroni results for L1 and L2 */
proc print data=contrasts_2312 noobs;
    var L1 L1_low L1_high
        L2 L2_low L2_high;
    title "23.12(a) - Bonferroni 90% Family Confidence Intervals for L1 and L2";
run;

/* --- Problem 23.12(b) - Single df tests (SECOND DATASET) --- */

data hay12_part_b;
    input y A B rep;
    lines;
4.2 1 1 1
2.8 1 1 2
2.4 1 1 3
0.7 1 1 4
4.2 1 2 1
5.6 1 2 2
4.1 1 2 3
4.5 1 2 4
4.4 1 3 1
4.7 1 3 2
4.5 1 3 3
4.7 1 3 4
6.2 2 1 1
5.0 2 1 2
4.8 2 1 3
5.8 2 1 4
/* (A=2,B=2) cell omitted */
8.9 2 3 1
9.3 2 3 2
9.0 2 3 3
9.1 2 3 4
9.2 3 1 1
6.6 3 1 2
4.4 3 1 3
2.7 3 1 4
9.7 3 2 1
10.0 3 2 2
10.6 3 2 3
10.8 3 2 4
12.9 3 3 1
13.1 3 3 2
13.5 3 3 3
13.5 3 3 4
;
run;

/* Fit two–factor model and do single–df tests */
proc glm data=hay12_part_b;
    class A B;
    model y = A B A*B;

    /* A*B ordering from PROC GLM with 8 levels is typically:
       Level: 11  12  13  21  23  31  32  33
       Index:  1   2   3   4   5   6   7   8
    */

    /* Test 1: H0: μ12 = μ13  → contrast (1,2) − (1,3) */
    estimate "Test 1: mu12 = mu13"
             A*B 0 1 -1 0 0 0 0 0 / divisor=1;

    /* Test 2: H0: μ32 = μ33  → contrast (3,2) − (3,3) */
    estimate "Test 2: mu32 = mu33"
             A*B 0 0 0 0 0 0 1 -1 / divisor=1;

    lsmeans A*B; /* Show the means to confirm ordering */
run;
quit;

/* --- Problem 24.14(d)–(e) and 24.12 --- */

/* --- Step 1: Input Data ------------------------------------------------ */
data assembly;
    input time A B C rep;
    datalines;
1250 1 1 1 1
1175 1 1 1 2
1236 1 1 1 3
1239 1 1 1 4
1193 1 1 1 5
1021 1 1 2 1
1099 1 1 2 2
1069 1 1 2 3
 996 1 1 2 4
1070 1 1 2 5
1319 1 2 1 1
1251 1 2 1 2
1241 1 2 1 3
1295 1 2 1 4
1265 1 2 1 5
1119 1 2 2 1
1110 1 2 2 2
1123 1 2 2 3
1097 1 2 2 4
1163 1 2 2 5
1217 1 3 1 1
1190 1 3 1 2
1201 1 3 1 3
1232 1 3 1 4
1251 1 3 1 5
1033 1 3 2 1
1067 1 3 2 2
1057 1 3 2 3
1077 1 3 2 4
1022 1 3 2 5
1066 2 1 1 1
1076 2 1 1 2
1004 2 1 1 3
1002 2 1 1 4
1034 2 1 1 5
 864 2 1 2 1
 848 2 1 2 2
 881 2 1 2 3
 892 2 1 2 4
 868 2 1 2 5
1105 2 2 1 1
1043 2 2 1 2
1051 2 2 1 3
1128 2 2 1 4
1060 2 2 1 5
 927 2 2 2 1
 944 2 2 2 2
 957 2 2 2 3
 897 2 2 2 4
 933 2 2 2 5
1021 2 3 1 1
1020 2 3 1 2
1035 2 3 1 3
1000 2 3 1 4
1026 2 3 1 5
 841 2 3 2 1
 865 2 3 2 2
 817 2 3 2 3
 911 2 3 2 4
 868 2 3 2 5
;
run;


/* --- Step 2: Fit ANOVA Model and Save Fitted Values & Residuals --- */

proc glm data=assembly;
    class A B C;
    model time = A B C A*B A*C B*C A*B*C;
    output out=diag r=resid p=fitted;
    title "24.14 - Three-Factor ANOVA Model with Residuals and Fitted Values";
run;
quit;


/* --- Step 3: Residual vs Fitted Plot (Part d) --- */
proc print data=diag (obs=5);
    var time fitted resid A B C;
    title "24.14(d) - Residuals and Fitted Values (First 5 Obs)";
run;

proc sgplot data=diag;
    scatter x=fitted y=resid;
    refline 0 / axis=y;
    title "24.14(d) - Residuals vs Fitted Values Plot";
run;


/* --- Step 4: Normal Probability Plot & Residual–Normal Correlation (Part e) --- */

/* Compute correlation between ordered residuals & expected normal scores */
proc rank data=diag normal=blom out=normres;
    var resid;
    ranks zscore;          /* expected normal scores */
run;

proc corr data=normres;
    var resid zscore;
    title "24.14(e) - Correlation Between Ordered Residuals and Expected Normal Scores";
run;

/* Produce actual normal probability plot (Q-Q plot) */
proc univariate data=diag normal;
    var resid;
    qqplot resid / normal(mu=est sigma=est);
    title "24.14(e) - Normal Probability Plot of Residuals";
run;


/* --- Problem 24.12: AB Interaction Plots by Level of C --- */

/* Get cell means */
proc means data=assembly nway noprint;
    class A B C;
    var time;
    output out=means mean=mean_time;
run;

/* Plot AB (A on x-axis, B as separate lines), one panel per C */
proc sgpanel data=means;
    panelby C / columns=2;  /* One plot per experience level (C) */
    series x=A y=mean_time / group=B markers;
    colaxis label="Gender (A)";
    rowaxis label="Estimated Mean Assembly Time";
    title "24.12(b) - AB Interaction Plot by Level of Experience (C)";
run;

/* Analysis of Variance Table for Problem 24.12 (repeated model fit for context) */
proc glm data=assembly;
    class A B C;
    model time = A B C A*B A*C B*C A*B*C;
    title "24.12(b) - Analysis of Variance Table for Electronics Assembly Data";
run;
quit;
