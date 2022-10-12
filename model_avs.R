# This function takes in the following inputs and generates cross-fit estimates of the desired estimand using a stacked ensemble of machine learning algorithms in h2o (https://docs.h2o.ai/h2o/latest-stable/h2o-docs/index.html)
## preprocess: the preprocessed dataset with a user-defined fold column "fold"
## features: the list of covariates for the analysis
## outcome: the name of the variable that is the target of prediction (e.g., outcome, treatment, uncensored)
## treatment: the name of the treatment/exposure variable that should be included (e.g., for treatment-specific models include the specified treatment; for treatment non-specific models, include all treatments)
## estimand: the name of the estimand - this will be used as the label of predictions

model_avs <- function(preprocess, features, outcome, treatment, estimand) {
  # load preprocessed data
  df <- preprocess
  # select outcome distribution
  distribution <- ifelse(grepl("BI|uncensored", outcome), "bernoulli", "gaussian")
  # add treatment indicator for uncensored models
  if(grepl("uncensored", outcome) == TRUE) {features <- append(features, "BI")}
  cat("Estimand:", estimand, "\nFeatures:", features)
  # run analysis
  Sys.sleep(5)
  h2o.init(nthreads = 32, max_mem_size = "64G")
  h2o.removeAll()
  list_preds <- list()
  for(i in 1:5){
    # for treatment outcome models (mu1 and mu0), training uses uncensored cases that received the specific treatment,
    # trained model is applied to entire test set
    h2o_train <- as.h2o(df[df$fold != i & is.na(df[,outcome]) == FALSE & df$BI %in% treatment,])
    h2o_test <- as.h2o(df[df$fold == i, features])
    h2o_model <- h2o.automl(x = features,
                            y = outcome,
                            max_models = 50,
                            training_frame = h2o_train,
                            keep_cross_validation_predictions = TRUE,
                            fold_column = "fold",
                            distribution = distribution,
                            seed = 2001,
                            verbosity = "error"
                            )
    h2o_model_best <- h2o.get_best_model(h2o_model, algorithm = "stackedensemble")
    h2o.saveModel(h2o_model_best, path = "/bd-fs-mnt/sam5_root/sasg_project/sri/projects/papini/git/sbirt/models/", filename = paste("model", outcome, i, sep = "_"), force = TRUE)
    df_preds_raw <- as.data.frame(h2o.predict(h2o_model_best, h2o_test))
    # extract predictions for binary and continuous outcomes
    prediction <- ifelse("p1" %in% names(df_preds_raw), "p1", "predict")
    df_preds <- data.frame(id = df[df$fold == i, "id"], y = df_preds_raw[,prediction])
    names(df_preds) <- c("id", estimand)
    list_preds[[i]] <- df_preds
    }
  # save predictions
  df_preds <- do.call(rbind, list_preds)
  filename <- paste("/bd-fs-mnt/sam5_root/sasg_project/sri/projects/papini/git/sbirt/predictions/", estimand, ".rds", sep = "")
  saveRDS(df_preds, filename)
  h2o.shutdown(prompt = FALSE)
  return(filename)
}