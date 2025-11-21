/**************************************************************************
 * STAT 525 – Homework 8
 * Student: Alexandra Chang
 **************************************************************************/

/* Q3 — KNNL 17.12 */

options nocenter ls=74; 
goptions colors=(none);

data prob3;
infile 'u:\.www\datasets525\ch16pr11.txt';
input y machine;
run;

proc means;
var y;
by machine;
output out=new1 mean=mn;
run;

symbol1 v=circle i=join;
proc gplot;
plot mn*machine;
run;

proc glm data=prob3;
class machine;
model y = machine;
means machine / t clm;
means machine / lsd cldiff;
means machine / tukey;
means machine / bon alpha=.5;
means machine / tukey alpha=.10;
run;

proc glm data=prob3;
class machine;
model y = machine / clparm;
estimate 'L' machine .5 .5 -.5 -.5 0 0;
run;

proc glm data=prob3;
class machine;
model y = machine / clparm alpha=.0142857;

estimate 'D1' machine 1 -1 0 0 0 0;
estimate 'D2' machine 0 0 1 -1 0 0;
estimate 'D3' machine 0 0 0 0 1 -1;

estimate 'L1' machine .5 .5 -.5 -.5 0 0;
estimate 'L2' machine .5 .5 0 0 -.5 -.5;
estimate 'L3' machine .25 .25 -.5 -.5 .25 .25;
estimate 'L4' machine .25 .25 .25 .25 -.5 -.5;

run;

/* Q4(a) */

proc mixed data=filling;
class machine;
model fill = machine;
estimate 'Avg(1,2) - Avg(3,4)' machine 0.5 0.5 -0.5 -0.5 0 0 / cl alpha=0.05;
run;

/* Q4(b) */

%let alphaF = 0.10;
%let g      = 7;
%let aprime = %sysevalf(&alphaF/&g);

ods output Estimates=bon_ci;

proc mixed data=filling noclprint;
class machine;
model fill = machine;

estimate 'D1: mu1 - mu2' machine 1 -1 0 0 0 0 / cl alpha=&aprime;
estimate 'D2: mu3 - mu4' machine 0 0 1 -1 0 0 / cl alpha=&aprime;
estimate 'D3: mu5 - mu6' machine 0 0 0 0 1 -1 / cl alpha=&aprime;

estimate 'L1: avg(1,2) - avg(3,4)' machine 0.5 0.5 -0.5 -0.5 0 0 / cl alpha=&aprime;
estimate 'L2: avg(1,2) - avg(5,6)' machine 0.5 0.5 0 0 -0.5 -0.5 / cl alpha=&aprime;
estimate 'L3: avg(1,2,5,6) - avg(3,4)' machine 0.25 0.25 -0.5 -0.5 0.25 0.25 / cl alpha=&aprime;
estimate 'L4: avg(1,2,3,4) - avg(5,6)' machine 0.25 0.25 0.25 0.25 -0.5 -0.5 / cl alpha=&aprime;

run;

/* Q5 */

%let mse   = 0.03097;
%let alphaF = 0.05;
%let g      = 4;
%let half   = 0.08;

data plan;
alpha_prime = &alphaF/&g;
n = 60;
df = 6*(n-1);
tcrit = tinv(1 - alpha_prime/2, df);

K_L1 = 2;  
K_L2 = 2;  
K_L3 = 1;  
K_L4 = 0.75;

me_L1 = tcrit*sqrt(&mse*K_L1/n);
me_L2 = tcrit*sqrt(&mse*K_L2/n);
me_L3 = tcrit*sqrt(&mse*K_L3/n);
me_L4 = tcrit*sqrt(&mse*K_L4/n);

put 'df=' df 5. ' tcrit=' tcrit 6.3;
put 'Half-widths at n=60:';
put 'L1=' me_L1 6.3 '  L2=' me_L2 6.3;
put 'L3=' me_L3 6.3 '  L4=' me_L4 6.3;
run;

/* Q7(b) */

proc glm data=winding plots=diagnostics;
class speed;
model breaks = speed;
output out=resid p=predicted r=residual;
run;

proc sgplot data=resid;
scatter x=predicted y=residual / group=speed;
refline 0 / axis=y lineattrs=(color=red);
title "Residuals vs Fitted Values by Winding Speed";
run;

proc sgplot data=resid;
vbox residual / category=speed;
refline 0 / axis=y lineattrs=(color=red);
title "Boxplot of Residuals by Winding Speed";
run;

/* Q7(c) */

ods output HOVFTest=bf;

proc glm data=winding;
class speed;
model breaks = speed;
means speed / hovtest=bf;
run;

proc print data=bf noobs;
var Source DF FValue ProbF;
title "Brown-Forsythe Test for Equal Variances";
run;
