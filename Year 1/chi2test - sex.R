observed_sex = c(175,213) #males, females
expected_sex_proportions = c(0.5, 0.5)
chisq.test(x=observed_sex, p=expected_sex_proportions)
