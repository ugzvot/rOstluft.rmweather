library(magrittr)
library(rmweather)
library(ranger)

devtools::load_all()

config_fn <- "rf_config.yml"
#config_fn <- "rf_config_eth_cn.yml"
#config_fn <- "rf_config_mythenquai.yml"

config <- yaml::read_yaml(config_fn)


# aqmet_public <- rOstluft::store_aqmet_public()
remote <- rOstluft::store_aqmet()
local <- rOstluft::storage_local_rds("rf", format = rOstluft::format_rolf(), read.only = FALSE)

# download the historic data from the remote store
download_from_remote(local, remote, config)

# actual data into the local
actual_data <- rOstluft::read_airmo_csv("tmp/Airmo_export_OL_Dauerstandorte_h1_20160101_bis_20200327.csv")
local$put(actual_data)

# do the heavy lifting take a look only at the prepared data
input_data <-  do_rf(config, local, prepare_data_only = TRUE)

print(input_data$n_observations, n = Inf)

input_data$observations

sprintf("input date ranges from %s to %s", min(input_data$observations$date), max(input_data$observations$date))
sprintf("%d complete rows", sum(complete.cases(input_data$observations)))


res <- do_rf(config, local, n_trees = 500, n_samples = 100, prepare_data_only = FALSE)


# Outputs
res$r2

rmw_plot_importance(rmw_model_importance(res$rf$model))
ggplot2::ggsave("tmp/importance.png", width = 600/300, height = 400/300, scale = 3)


rmw_predict_the_test_set(res$rf$model, res$rf$observations) %>%
  rmw_plot_test_prediction()
ggplot2::ggsave("tmp/test_prediction.png", width = 600/300, height = 600/300, scale = 3)


# SLOW!!!!!
partial_dependencies <- rmw_partial_dependencies(res$rf$model, res$rf$observations, NA, verbose = TRUE)
rmw_plot_partial_dependencies(partial_dependencies)

saveRDS(partial_dependencies, "tmp/partial_dependencies.rds")
