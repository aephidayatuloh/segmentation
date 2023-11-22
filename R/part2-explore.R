library(tidyverse)
library(scales)
library(ggcorrplot)


abt <- read_csv("data/customer_segmentation.csv")
abt |> 
  ggplot(aes(x = NoChild)) + 
  geom_bar() + 
  scale_x_continuous(breaks = seq(1, 10, by = 1)) + 
  scale_y_continuous(breaks = seq(0, 22000, by = 50), 
                     labels = number_format(big.mark = ",")) +
  labs(x = "No of Child") + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank()
  )

abt |> 
  count(NoChild) |> 
  mutate(pct = n/sum(n))

abt |> 
  ggplot(aes(x = tenure_months, y = "")) + 
  geom_violin(fill = "skyblue", alpha = 0.75) +
  geom_boxplot(width = 0.1) + 
  stat_summary(fun = mean, color = "red") + 
  scale_x_continuous(breaks = seq(0, 150, by = 10)) + 
  labs(x = "Tenure (month)") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

abt |> 
  ggplot(aes(x = youngest_kid_age_join, y = "")) + 
  geom_violin(fill = "skyblue", alpha = 0.75) +
  geom_boxplot(width = 0.1) + 
  stat_summary(fun = mean, color = "red") +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  labs(x = "Youngest Kid Age when Join (year)") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

abt |> 
  ggplot(aes(x = recency, y = "")) + 
  geom_violin(fill = "skyblue", alpha = 0.75) +
  geom_boxplot(width = 0.1) + 
  stat_summary(fun = mean, color = "red") +
  scale_x_continuous(breaks = seq(0, 375, by = 15)) +
  labs(x = "Recency (day)") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

abt |> 
  ggplot(aes(x = avg_monthly_frequency, y = "")) + 
  geom_violin(fill = "skyblue", alpha = 0.75) +
  geom_boxplot(width = 0.1) + 
  stat_summary(fun = mean, color = "red") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  labs(x = "Avg Monthly Visit") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

abt |> 
  ggplot(aes(x = avg_monetary, y = "")) + 
  geom_violin(fill = "skyblue", alpha = 0.75) +
  geom_boxplot(width = 0.1) + 
  stat_summary(fun = mean, color = "red") +
  scale_x_continuous(
    breaks = seq(0, 2500, by = 100), 
    labels = number_format(big.mark = ",")
  ) +
  labs(x = "Avg Monthly Monetary") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

abt |> 
  ggplot(aes(x = avg_interpurchase, y = "")) + 
  geom_violin(fill = "skyblue", alpha = 0.75) +
  geom_boxplot(width = 0.1) + 
  stat_summary(fun = mean, color = "red") +
  scale_x_continuous(breaks = seq(0, 250, by = 10)) +
  labs(x = "Avg Interpurchase") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

abt |> 
  ggplot(aes(x = freq_last3mo, y = "")) + 
  geom_violin(fill = "skyblue", alpha = 0.75) +
  geom_boxplot(width = 0.1) + 
  stat_summary(fun = mean, color = "red") +
  scale_x_continuous(breaks = seq(0, 50, by = 1)) +
  labs(x = "Freq Last 3 Months") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())


# Correlation -------------------------------------------------------------

pmat <- abt |> 
  select(-MemberID) |>
  cor_pmat()

abt |> 
  select(-MemberID) |>
  cor() |> 
  ggcorrplot(colors = c("firebrick", "white", "green"), 
             hc.order = TRUE)

abt |> 
  select(-MemberID) |>
  cor() |> 
  ggcorrplot(colors = c("firebrick", "white", "green"), 
             hc.order = TRUE, p.mat = pmat, 
             lab = TRUE, lab_size = 4, digits = 1, 
             ggtheme = theme_bw())


# Hypothesis Testing ------------------------------------------------------

abt |> 
  select(avg_monetary) |> 
  t.test(mu = 300, alternative = "greater")

abt |> 
  select(avg_monthly_frequency) |> 
  t.test(mu = 2, alternative = "greater")

abt |> 
  transmute(last3mo = if_else(freq_last3mo > 0, 1, 0)) |> 
  count(last3mo) |> 
  mutate(pct = n/sum(n))

abt |> 
  transmute(last3mo = if_else(freq_last3mo > 0, 1, 0)) |> 
  table() |> 
  prop.test(alternative = "less")


pairs(abt |> 
        select(-MemberID), 
      pch = 19, cex = 0.5, 
      lower.panel = NULL)
