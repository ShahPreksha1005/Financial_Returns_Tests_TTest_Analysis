# Load the 'writexl' package for writing Excel files
install.packages("writexl")
library(writexl)

# Set seed for reproducibility
set.seed(123)

# Create synthetic financial data for two groups with positive integers
group1_returns <- sample.int(100, 100, replace = TRUE)  # Group 1 returns
group2_returns <- sample.int(150, 100, replace = TRUE)  # Group 2 returns

# Create serial numbers
serial_numbers <- 1:100

# Create dates
dates <- seq(as.Date("2022-01-01"), by = "days", length.out = 100)

# Combine data into a data frame
finance_data <- data.frame(SerialNumber = serial_numbers, Date = dates, 
                           customers1_returns = group1_returns, 
                           customers2_returns = group2_returns)

# Save the data frame to an Excel file
write_xlsx(finance_data, path = "finance_data_with_attributes.xlsx")

# Display a summary of the data
summary(finance_data)

# Perform a two-sample t-test
t_test_result <- t.test(group1_returns, group2_returns, alternative 
                        = "two.sided", var.equal = TRUE)

# Print the t-test results
cat("Two-Sample t-test Results:\n")
print(t_test_result)

# Extracting the P-value and Statistic
test_statistic <- t_test_result$statistic
p_value <- t_test_result$p.value

# Print the results
cat("Test Statistic:", test_statistic, "\n")
cat("P-value:", p_value, "\n")

# Interpret the results
if (p_value < 0.05) {
  cat("At the 0.05 significance level, we reject the null hypothesis.\n")
} else {
  cat("We do not have enough evidence to reject the null hypothesis at the 
      0.05 significance level.\n")
}

# For Unequal Variance
test_result_2 = t.test(group1_returns,group2_returns,alternative=
                         "two.sided",var.equal=FALSE)
test_result_2

# Extracting the P-value and Statistic
test_statistic <- test_result_2$statistic
p_value <- test_result_2$p.value

# Print the results
cat("Test Statistic:", test_statistic, "\n")
cat("P-value:", p_value, "\n")

# Interpret the results
if (p_value < 0.05) {
  cat("At the 0.05 significance level, we reject the null hypothesis.\n")
} else {
  cat("We do not have enough evidence to reject the null hypothesis at the 
      0.05 significance level.\n")
}


# Paired t test
set.seed(123)
# Create synthetic financial data for two groups with positive integers
group3_returns <- sample.int(100, 100, replace = TRUE)  # Group 1 returns
group4_returns <- sample.int(150, 100, replace = TRUE)  # Group 2 returns

# Create serial numbers
serial_numbers <- 1:100

# Create dates
dates <- seq(as.Date("2022-01-01"), by = "days", length.out = 100)

# Combine data into a data frame
finance_data_2 <- data.frame(SerialNumber = serial_numbers, Date = dates, 
                             customers3_returns = group3_returns, 
                             customers4_returns = group4_returns)

# Save the data frame to an Excel file
write_xlsx(finance_data_2, path = "finance_data_with_attributes.xlsx")

# Display a summary of the data
summary(finance_data_2)

paired_t_test = t.test(group3_returns, group4_returns, paired=TRUE)
paired_t_test

# Extracting the P-value and Statistic
test_statistic <- paired_t_test$statistic
p_value <- paired_t_test$p.value

# Print the results
cat("Test Statistic:", test_statistic, "\n")
cat("P-value:", p_value, "\n")

# Interpret the results
if (p_value < 0.05) {
  cat("At the 0.05 significance level, we reject the null hypothesis.\n")}
else {
  cat("We do not have enough evidence to reject the null hypothesis at the 
      0.05 significance level.\n")}