library(ggplot2)
library(dplyr)
library(tidyr)

Male_0 <- 100
Female_0 <- 100

male <- c(Male_0)
female <- c(Female_0)

ma <- Male_0
fe <- Female_0

for (i in 1:200) {
  ma_inc <- -0.2 * ma + (ma + fe) / (ma * fe)
  fe_inc <- -0.1 * fe + (ma + fe) / (ma * fe)
  
  ma <- ma + ma_inc
  fe <- fe + fe_inc
  
  male <- c(male, ma)
  female <- c(female, fe)
}

# Put into a tidy dataframe
df <- data.frame(
  Iteration = 0:200,
  Male = male,
  Female = female
) %>%
  pivot_longer(cols = c("Male", "Female"),
               names_to = "Group",
               values_to = "Population")

# Plot
ggplot(df, aes(x = Iteration, y = Population, color = Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(title = "Male vs Female Population over Iterations",
       y = "Population Size")
