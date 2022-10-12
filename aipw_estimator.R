## This code runs AIPW estimator using the cross-fit estimates from the machine learning analyses
pacman::p_load(tidyverse, magrittr)
# df: dataframe with columns:
#  DATA:
#     Y: the continuous outcome (except NA in rows where D=1)
#     A: binary exposure: 1="exposed", 0="control"
#     D: outcome observation indicator: 1="outcome observed", 0="outcome missing" (df$D = !is.na(df$Y))
#     X: covariates (multiple columns)
#     G: binary group indicator (also counts as a covariate, e.g. use as a predictor when fitting models below)
#  PREDICTIONS:
#     mu1: outcome prediction assuming exposure 
#     mu0: outcome prediction assuming no exposure
#     pi1: predicted probability of exposure
#     pi0: predicted probability of no exposure (same as 1-pi1)
#     delta: predicted probability of observing outcome

list_outcomes <- c("drinking_days_delta", "binge_days_delta", "drinks_week_delta", "drinks_day_delta")
list_results <- list()
for(i in 1:length(list_outcomes)){
  outcome <- list_outcomes[[i]]
  df$Y <- df[ ,paste0(outcome)]
  df$A <- as.numeric(as.character(df$BI))
  df$D <- as.numeric(as.character(df$uncensored))
  df$G <- ifelse(df$dep0 == "Y", 1, 0)
  df$mu1 <- df[ ,paste0("mu1_", outcome)]
  df$mu0 <- df[ ,paste0("mu0_", outcome)]
  df$pi1 <- df$pi
  df$pi0 <- 1- df$pi
  df$delta <- df$delta
  
  # Run AIPW estimators
  df_xi = df %>% 
    mutate(Y = ifelse(is.na(Y), 0, Y)) %>%  # replace NAs with 0 so that NA * (D==0) = 0
    mutate(
      xi1 = (D*(A==1))*(Y-mu1) / (delta*pi1)  +  mu1,
      xi0 = (D*(A==0))*(Y-mu0) / (delta*pi0)  +  mu0
    )
  
  psi_ = list()
  phi_ = list()
  for(g in c(0,1)) {
    df_g = df_xi %>% filter(G==g)
    psi_[[paste0("a1g",g)]]  = df_g %$% mean(xi1)
    psi_[[paste0("a0g",g)]]  = df_g %$% mean(xi0)
    phi_[[paste0("a1g",g)]]  = df_xi %$% (((G==g)/mean(G==g)) * (xi1 - psi_[[paste0("a1g",g)]]))
    phi_[[paste0("a0g",g)]]  = df_xi %$% (((G==g)/mean(G==g)) * (xi0 - psi_[[paste0("a0g",g)]]))
  }
  
  # ATE all
  psi_all <- mean(df_xi$xi1) - mean(df_xi$xi0)
  sigma_all <- sqrt( sum(((df_xi$xi1 - df_xi$xi0) - psi_all)^2)/nrow(df)^2 )
  
  # ATE depressed
  psi_g1 <- psi_[[paste0("a1g1")]] - psi_[[paste0("a0g1")]]
  df_g1 <- df_xi %>% filter(G==1)
  sigma_g1 <- sqrt( sum(((df_g1$xi1 - df_g1$xi0) - psi_g1)^2)/nrow(df_g1)^2 )
  
  # ATE non-depressed
  psi_g0 <- psi_[[paste0("a1g0")]] - psi_[[paste0("a0g0")]]
  df_g0 <- df_xi %>% filter(G==0)
  sigma_g0 <- sqrt( sum(((df_g0$xi1 - df_g0$xi0) - psi_g0)^2)/nrow(df_g0)^2 )
  
  # Difference in ATE (depressed - non-depressed)
  psi_g1minusg0 <- psi_g1 - psi_g0
  phi_g1minusg0 <- ( phi_[[paste0("a1g1")]] - phi_[[paste0("a0g1")]] )  -  ( phi_[[paste0("a1g0")]] - phi_[[paste0("a0g0")]] )
  sigma_g1minusg0 <- sqrt( mean(phi_g1minusg0^2) / nrow(df) )
  
  # Compile results
  results <- data.frame(outcome = outcome,
                        comparison = c("All", "Depressed", "Non-depressed", "Depressed -\nNon-depressed"),
                        ATE = round(c(psi_all, psi_g1, psi_g0, psi_g1minusg0), 2),
                        CI = NA,
                        ci.lo = NA,
                        ci.hi = NA,
                        z = NA)
  list_models <- c("all", "g1", "g0", "g1minusg0")
  for(j in 1:nrow(results)){
    psi <- get(paste0("psi_", list_models[[j]]))
    sigma <- get(paste0("sigma_", list_models[[j]]))
    results[j, "CI"] <- paste0("[", round(psi - 1.96*sigma, 2), ", ", round(psi + 1.96*sigma, 2), "]")
    results[j, "ci.lo"] <- round(psi - 1.96*sigma, 2)
    results[j, "ci.hi"] <- round(psi + 1.96*sigma, 2)
    results[j, "z"] <- round(psi/sigma, 3)
  }
  
  list_results[[i]] <- results
}

# combine all results
results <- do.call(rbind, list_results)
# add p-values
results$p <- round(2*pnorm(q=abs(results$z), lower.tail=FALSE), 4)

results[,c("comparison", "outcome", "ATE", "CI", "z", "p")]
results[results$comparison == "Depressed -\nNon-depressed", c("outcome", "ATE", "CI", "z", "p")]
results[results$comparison == "All", c("outcome", "ATE", "CI", "p")]
results[results$comparison == "Depressed", c("outcome", "ATE", "CI", "p")]