Under the grand model, outlying Y observations will have large corresponding absolute values of studentized deleted residuals. If there are no outliers, each studentized residual will follow a *t* distribution with n - p - 1 = 100 - 9 - 1 = 90 degrees of freedom. 
```{r}
stud_resids <- studres(grand_model)
stud_resids_df <- data.frame(stud_resid = stud_resids) %>%
  arrange(desc(abs(stud_resid)))

head(stud_resids_df, 10)
```
The 10 observations with the largest studentized deleted residuals by absolute value are shown above. We will compare the values in *stud_resid* to the following t-distribution.

```{r}
curve(dt(x, df=90), from=-5, to=5,
      main = 't-distribution with 90 dof',
      ylab = 'Density', 
      col = 'steelblue')
```

Observation 59 appears concerning. Its studentized deleted residual of 3.638507 is very close to the right tail of the t-distribution above. We will test the probability of observing the value or more extreme.
```{r}
pt(3.638507, 90, lower.tail = FALSE)
```
Because this probability is extremely low, we conclude that Observation 59 is indeed a Y-outlier. 

To spot influential observations, we will consider their DFFITs values.
```{r}
dffits_vals <- dffits(grand_model)
dffits_vals_df <- data.frame(dffits = dffits_vals) %>%
  arrange(desc(abs(dffits)))
head(dffits_vals_df, 10)
```





```{r}
selected_model_robust <- rlm(X2020 ~ X2018 + Is_Home + Sex + X2012, 
                             expenditures_reg_train)
```

```{r}
selected_model_robust_df <- data.frame(Y_hat = fitted(selected_model_robust),
                                       Residuals = resid(selected_model_robust)
)

ggplot(selected_model_robust_df, aes(x = Y_hat, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Y_hat", y = "Residuals", title = "Residuals vs Y_hat for Robust 
       Model")
```