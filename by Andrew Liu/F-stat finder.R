alpha <- 0.5
numerator_df <- 10
denominator_df <- 324

critical_f <- qf(1 - alpha, numerator_df, denominator_df)
print(paste("Critical F-value is:", critical_f))

qt(0.99375,114)

qtukey(0.9, 6, 114)

qt(0.9722,324)

qf(1 - 6.740246933518981e-10, 6, 81)

pf(12.4615, 6, 81, lower.tail = FALSE)
