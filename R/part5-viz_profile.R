source("R/fn_cluster_profile.R")

cs_cluster |> 
  count(.pred_cluster) |> 
  mutate(pct = n/sum(n)) |> 
  ggplot(aes(x = .pred_cluster, y = n, fill = .pred_cluster)) + 
  geom_col(alpha = 0.75, show.legend = FALSE) + 
  geom_text(aes(label = percent(pct, accuracy = 0.1)), 
            vjust = -0.25, size = 8) + 
  labs(y = "Jumlah Anggota Cluster") + 
  lims(y = c(0, 250)) +
  theme_bw()

p_NoChild <- cs_cluster |> 
  cluster_profiling(
    .variable = NoChild, 
    .cluster = .pred_cluster, 
    discrete = TRUE
  )

p_recency <- cs_cluster |> 
  cluster_profiling(
    .variable = recency, 
    .cluster = .pred_cluster, 
    discrete = FALSE, 
    pop.box.width = 0.001, 
    cluster.box.width = 0.2
  )

p_avg_monetary <- cs_cluster |> 
  cluster_profiling(
    .variable = avg_monetary, 
    .cluster = .pred_cluster, 
    discrete = FALSE, 
    pop.box.width = 0.0005, 
    cluster.box.width = 0.2
  )

p_avg_monthly_frequency <- cs_cluster |> 
  cluster_profiling(
    .variable = avg_monthly_frequency, 
    .cluster = .pred_cluster, 
    discrete = FALSE, 
    pop.box.width = 0.3, 
    cluster.box.width = 0.2
  )

p_avg_interpurchase <- cs_cluster |> 
  cluster_profiling(
    .variable = avg_interpurchase,
    pop.box.width = 0.005, 
    cluster.box.width = 0.2
  )

p_freq_last3mo <- cs_cluster |> 
  cluster_profiling(
    freq_last3mo, 
    pop.box.width = 0.01, 
    cluster.box.width = 0.2
  )

p_tenure_months <- cs_cluster |> 
  cluster_profiling(
    tenure_months, 
    pop.box.width = 0.005, 
    cluster.box.width = 0.2
  )

p_avg_consumption <- cs_cluster |> 
  cluster_profiling(
    avg_consumption, 
    pop.box.width = 0.005, 
    cluster.box.width = 0.2
  )

grid.arrange(
  p_NoChild, 
  p_recency, 
  p_tenure_months, 
  p_avg_monetary, 
  p_avg_monthly_frequency, 
  p_avg_interpurchase, 
  p_freq_last3mo, 
  p_avg_consumption, 
  ncol = 4
)


# Labeling ----------------------------------------------------------------

cs_cluster <- cs_cluster |> 
  mutate(
    cluster_label = case_when(.pred_cluster == "Cluster_1" ~ "Frequently", 
                              .pred_cluster == "Cluster_2" ~ "High-Value", 
                              .pred_cluster == "Cluster_3" ~ "Almost-Churn", 
                              .pred_cluster == "Cluster_4" ~ "High-Value")
  )
