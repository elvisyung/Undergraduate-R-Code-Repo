observed_frequency = c(29,88,1,28,6,167,37,384,13,1) 
expected_frequency_proportion = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
chisq.test(x=observed_frequency, p=expected_frequency_proportion)
