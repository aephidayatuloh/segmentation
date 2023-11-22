library(tidymodels)
library(tidyclust)


# abt <- read_csv("data/customer_segmentation.csv")
# 
# abt_cluster <- abt |> 
#   select(-MemberID) 

optimk <- 4

kmeans_spec <- k_means(num_clusters = optimk) |> 
  set_engine(engine = "stats", iter.max = 5000, nstart = 25)

kmeans_wf <- kmeans_wf |> 
  update_model(kmeans_spec)

# cs_recipe <- recipe(~ ., data = abt_cluster) |> 
#   step_pca(all_numeric_predictors(), num_comp = 4, 
#            options = list(retx = TRUE, scale. = TRUE, center = TRUE)) |> 
#   prep()
# 
# kmeans_wf <- workflow(
#   preprocessor = cs_recipe, 
#   spec = kmeans_spec)

set.seed(1001)
kmeans_fit <- kmeans_wf |> 
  fit(data = abt_cluster)

kmeans_fit |> 
  extract_centroids()

kmeans_fit |> 
  extract_cluster_assignment() |> 
  count(.cluster) |> 
  mutate(pct = n/sum(n))

kmeans_fit |> 
  sse_within_total(new_data = abt_cluster)

kmeans_fit |> 
  silhouette_avg(new_data = abt_cluster)

# "Predict" cluster
kmeans_fit |> 
  predict(new_data = abt)

# Add cluster label to original dataset
cs_cluster <- kmeans_fit |> 
  augment(new_data = abt)

cs_cluster |> 
  count(.pred_cluster) |> 
  mutate(Variable = "member") |> 
  pivot_wider(id_cols = Variable, 
              names_from = .pred_cluster, 
              values_from = n) |> 
  bind_rows(
    cs_cluster |> 
      count(.pred_cluster) |> 
      mutate(Variable = "percent member", 
             pct = n/sum(n)) |> 
      pivot_wider(id_cols = Variable, names_from = .pred_cluster, values_from = pct), 
    cs_cluster |> 
      rownames_to_column("id") |> 
      pivot_longer(cols = -c(id, .pred_cluster), names_to = "Variable", values_to = "Values") |> 
      group_by(.pred_cluster, Variable) |> 
      summarise(
        avg = mean(Values), 
        .groups = "drop"
      ) |> 
      pivot_wider(id_cols = Variable, names_from = .pred_cluster, values_from = avg)
  ) 

pairs(cs_cluster |> 
        select(youngest_kid_age_join, recency, frequency, 
               monetary, avg_monthly_frequency), 
      pch = 19, cex = 0.5, 
      col = cs_cluster$.pred_cluster, lower.panel = NULL)
