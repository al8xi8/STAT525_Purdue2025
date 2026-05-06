* Course:      STAT 525 - Fall 2025
* Homework:    Homework 1
* Student:     Alexandra Chang;

DATA a1;
    INPUT y x;
DATALINES;
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
RUN;

/* Regression, residuals, predictions */
PROC REG DATA=a1;
    MODEL y = x / CLB P R;
    OUTPUT OUT=a2 P=pred R=resid;
    ID x;
RUN;
