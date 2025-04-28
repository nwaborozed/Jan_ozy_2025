

# Updated pcr data frame
pcr <- data.frame(
  N = c(435,72,309,309,309,67,67,52,59,59,59,326,125,75,106,47,75,67,90,357,100,60,685,242,154,45),
  TP = c(137, 54,15,15,15,35,34,21,23,24,24,0,24,33,70,35,13,0,90,0,43,17,25,171,39,1),
  FP = c(0,0,0,0,0,8,9,1,2,1,1,0,0,0,2,0,0,4,0,0,0,1,0,0,6,0),
  FN = c(298,18,32,33,37,8,8,1,4,3,0,41,101,42,14,10,49,0,0,2,37,34,240,71,38,2),
  TN = c(0,0,262,261,257,16,16,29,30,31,34,285,0,0,20,2,13,63,0,355,20,8,420,0,71,42)
)

# Other data frames (unchanged)
elisa <- data.frame(
  N = c(202,137,59,59,59,106,182,60,60,120,271,156,64,52,201,202,364,364,364,364,456,456),
  TP = c(41,25,23,24,24,70,29,29,25,54,70,18,15,21,41,40,73,74,72,71,45,44),
  FP = c(18,33,4,3,0,14,25,2,0,2,12,26,19,4,14,25,0,0,0,2,0,2),
  FN = c(1,11,2,1,1,2,14,1,5,6,1,2,11,2,1,2,13,12,14,15,5,6),
  TN = c(142,68,30,31,34,20,114,28,30,58,188,110,19,25,145,135,278,278,278,276,406,404)
)

rdt <- data.frame(
  N = c(59,59,60,60,521,456),
  TP = c(24,23,27,22,186,41),
  FP = c(1,1,0,0,1,0),
  FN = c(1,2,3,8,14,9),
  TN = c(33,33,30,30,320,406)
)

elisa$sum <- with(elisa, TP + FP + FN + TN)
if (any(elisa$N != elisa$sum)) warning("Mismatch in elisa: N != TP + FP + FN + TN")
rdt$sum <- with(rdt, TP + FP + FN + TN)
if (any(rdt$N != rdt$sum)) warning("Mismatch in rdt: N != TP + FP + FN + TN")
pcr$sum <- with(pcr, TP + FP + FN + TN)
if (any(pcr$N != pcr$sum)) warning("Mismatch in pcr: N != TP + FP + FN + TN")

library(rstan)

stan_data <- list(
  N_EP = nrow(elisa),
  TP_EP = elisa$TP,
  FP_EP = elisa$FP,
  FN_EP = elisa$FN,
  TN_EP = elisa$TN,
  N_RP = nrow(rdt),
  TP_RP = rdt$TP,
  FP_RP = rdt$FP,
  FN_RP = rdt$FN,
  TN_RP = rdt$TN,
  N_P = nrow(pcr),
  P_P = pcr$TP + pcr$FP,
  N_P_samples = pcr$N
)

# Updated Stan code
stan_code <- "
data {
  int<lower=1> N_EP;
  int<lower=0> TP_EP[N_EP];
  int<lower=0> FP_EP[N_EP];
  int<lower=0> FN_EP[N_EP];
  int<lower=0> TN_EP[N_EP];
  
  int<lower=1> N_RP;
  int<lower=0> TP_RP[N_RP];
  int<lower=0> FP_RP[N_RP];
  int<lower=0> FN_RP[N_RP];
  int<lower=0> TN_RP[N_RP];
  
  int<lower=1> N_P;
  int<lower=0> P_P[N_P];
  int<lower=0> N_P_samples[N_P];
}

parameters {
  real<lower=0, upper=1> Se_E;
  real<lower=0, upper=1> Sp_E;
  real<lower=0, upper=1> Se_P;
  real<lower=0, upper=1> Sp_P;
  real<lower=0, upper=1> Se_R;
  real<lower=0, upper=1> Sp_R;
  
  vector<lower=0, upper=1>[N_EP] pi_EP;
  vector<lower=0, upper=1>[N_RP] pi_RP;
  vector<lower=0, upper=1>[N_P] pi_P;
}

transformed parameters {
  vector[4] prob_EP[N_EP];
  vector[4] prob_RP[N_RP];
  vector[2] prob_P[N_P];
  
  for (i in 1:N_EP) {
    prob_EP[i, 1] = pi_EP[i] * Se_E * Se_P + (1 - pi_EP[i]) * (1 - Sp_E) * (1 - Sp_P);
    prob_EP[i, 2] = pi_EP[i] * Se_E * (1 - Se_P) + (1 - pi_EP[i]) * (1 - Sp_E) * Sp_P;
    prob_EP[i, 3] = pi_EP[i] * (1 - Se_E) * Se_P + (1 - pi_EP[i]) * Sp_E * (1 - Sp_P);
    prob_EP[i, 4] = pi_EP[i] * (1 - Se_E) * (1 - Se_P) + (1 - pi_EP[i]) * Sp_E * Sp_P;
  }
  
  for (i in 1:N_RP) {
    prob_RP[i, 1] = pi_RP[i] * Se_R * Se_P + (1 - pi_RP[i]) * (1 - Sp_R) * (1 - Sp_P);
    prob_RP[i, 2] = pi_RP[i] * Se_R * (1 - Se_P) + (1 - pi_RP[i]) * (1 - Sp_R) * Sp_P;
    prob_RP[i, 3] = pi_RP[i] * (1 - Se_R) * Se_P + (1 - pi_RP[i]) * Sp_R * (1 - Sp_P);
    prob_RP[i, 4] = pi_RP[i] * (1 - Se_R) * (1 - Se_P) + (1 - pi_RP[i]) * Sp_R * Sp_P;
  }
  
  for (i in 1:N_P) {
    prob_P[i, 1] = pi_P[i] * Se_P + (1 - pi_P[i]) * (1 - Sp_P);
    prob_P[i, 2] = 1 - prob_P[i, 1];
  }
}

model {
  Se_E ~ beta(1, 1);
  Sp_E ~ beta(1, 1);
  Se_P ~ beta(1, 1);
  Sp_P ~ beta(1, 1);
  Se_R ~ beta(1, 1);
  Sp_R ~ beta(1, 1);
  
  pi_EP ~ beta(1, 1);
  pi_RP ~ beta(1, 1);
  pi_P ~ beta(1, 1);
  
  for (i in 1:N_EP) {
    target += multinomial_lpmf({TP_EP[i], FP_EP[i], FN_EP[i], TN_EP[i]} | prob_EP[i]);
  }
  
  for (i in 1:N_RP) {
    target += multinomial_lpmf({TP_RP[i], FP_RP[i], FN_RP[i], TN_RP[i]} | prob_RP[i]);
  }
  
  for (i in 1:N_P) {
    P_P[i] ~ binomial(N_P_samples[i], prob_P[i, 1]);
  }
}

generated quantities {
  real Se_E_mean = Se_E;
  real Sp_E_mean = Sp_E;
  real Se_P_mean = Se_P;
  real Sp_P_mean = Sp_P;
  real Se_R_mean = Se_R;
  real Sp_R_mean = Sp_R;
  
  // Pooled prevalence
  real pooled_pi_RP = mean(pi_RP);  // Mean of RDT prevalences
  real pooled_pi_P = mean(pi_P);    // Mean of PCR prevalences
  real pooled_pi_EP = mean(pi_EP);  // Mean of ELISA prevalences
}
"

stan_model <- stan_model(model_code = stan_code)


# Initial values to avoid extremes
init_list <- function(chain_id) {
  list(
    Se_E = 0.9, Sp_E = 0.9,
    Se_P = 0.9, Sp_P = 0.9,
    Se_R = 0.9, Sp_R = 0.9,
    pi_EP = rep(0.5, stan_data$N_EP),
    pi_RP = rep(0.5, stan_data$N_RP),
    pi_P = rep(0.5, stan_data$N_P)
  )
}

# Run sampling with initial values
fit <- sampling(stan_model, data = stan_data, chains = 4, iter = 2000, warmup = 1000, init = init_list)
print(fit)

## Extract summary statistics and keep parameter names
fit_summary <- summary(fit)$summary
fit_df <- as.data.frame(fit_summary)
fit_df$parameter <- rownames(fit_summary)

# Move 'parameter' column to the front
fit_df <- fit_df[, c("parameter", setdiff(names(fit_df), "parameter"))]

# Write to Excel
library(writexl)
write_xlsx(fit_df, path = "blcm_all_estimates.xlsx")
# Compile and run (using your existing stan_data and init_list)

fit_pooled <- sampling(stan_model, data = stan_data, chains = 4, iter = 2000, warmup = 1000, init = init_list)

# Print pooled estimates
print(fit_pooled, pars = c("pooled_pi_RP", "pooled_pi_P", "pooled_pi_EP"))

##############################################################################################

library(MCMCvis)


MCMCtrace(fit, file = "Rstan_trace_plots.pdf")

MCMCtrace(fit_pooled, file = "Rstan_trace_plots.pdf")

# Increase print limit
options(max.print = 99999)

# Full output
print(fit)

# MCMCvis diagnostics
MCMCsummary(fit, params = c("Se_E", "Sp_E", "Se_P", "Sp_P", "Se_R", "Sp_R"))
MCMCtrace(fit, params = c("Se_E", "Sp_E", "Se_P", "Sp_P", "Se_R", "Sp_R"), pdf = TRUE)





# Install the writexl package if not already installed
install.packages("writexl")

# Export to Excel
library(writexl)


# Generate MCMCsummary for sensitivity and specificity parameters
mcmc_summary <- MCMCsummary(fit, params = c("Se_E", "Sp_E", "Se_P", "Sp_P", "Se_R", "Sp_R"))

# View the output (optional)
print(mcmc_summary)

# Export to Excel
write_xlsx(mcmc_summary, path = "sensitivity_specificity_summary.xlsx")




library(rstan)
library(writexl)

# Extract sensitivity and specificity from fit
se_sp_samples <- extract(fit, pars = c("Se_E", "Sp_E", "Se_P", "Sp_P", "Se_R", "Sp_R"))

# Extract or compute pooled prevalence
if (exists("fit_pooled") && class(fit_pooled) == "stanfit") {
  # From fit_pooled
  pooled_prev_samples <- extract(fit_pooled, pars = c("pooled_pi_RP", "pooled_pi_P", "pooled_pi_EP"))
  pooled_pi_RP <- pooled_prev_samples$pooled_pi_RP
  pooled_pi_P <- pooled_prev_samples$pooled_pi_P
  pooled_pi_EP <- pooled_prev_samples$pooled_pi_EP
  cat("Using pooled prevalence from fit_pooled\n")
} else {
  # Post hoc from fit
  cat("fit_pooled not found, computing pooled prevalence post hoc from fit\n")
  pi_RP_samples <- extract(fit, pars = paste0("pi_RP[", 1:6, "]"))$pi_RP  # 4000 x 6
  pi_P_samples <- extract(fit, pars = paste0("pi_P[", 1:26, "]"))$pi_P    # 4000 x 26
  pi_EP_samples <- extract(fit, pars = paste0("pi_EP[", 1:22, "]"))$pi_EP # 4000 x 22
  
  # Compute pooled prevalence (mean across studies for each draw)
  pooled_pi_RP <- rowMeans(pi_RP_samples)
  pooled_pi_P <- rowMeans(pi_P_samples)
  pooled_pi_EP <- rowMeans(pi_EP_samples)
}

# Verify the objects exist and have correct length (4000 draws)
if (!exists("pooled_pi_EP") || length(pooled_pi_EP) != 4000) {
  stop("pooled_pi_EP not properly defined. Check fit or fit_pooled.")
}
cat("Length of pooled_pi_EP:", length(pooled_pi_EP), "\n")

# Compute accuracy
accuracy_E <- se_sp_samples$Se_E * pooled_pi_EP + se_sp_samples$Sp_E * (1 - pooled_pi_EP)
accuracy_P <- se_sp_samples$Se_P * pooled_pi_P + se_sp_samples$Sp_P * (1 - pooled_pi_P)
accuracy_R <- se_sp_samples$Se_R * pooled_pi_RP + se_sp_samples$Sp_R * (1 - pooled_pi_RP)

# Summarize function
summarize_accuracy <- function(samples, test_name) {
  mean_val <- mean(samples)
  ci <- quantile(samples, probs = c(0.025, 0.975))
  cat(sprintf("%s Accuracy:\n", test_name))
  cat(sprintf("Mean: %.3f\n", mean_val))
  cat(sprintf("95%% Credible Interval: [%.3f, %.3f]\n\n", ci[1], ci[2]))
  return(data.frame(Test = test_name, Mean = mean_val, `2.5%` = ci[1], `97.5%` = ci[2]))
}

# Summarize and combine
accuracy_E_summary <- summarize_accuracy(accuracy_E, "ELISA")
accuracy_P_summary <- summarize_accuracy(accuracy_P, "PCR")
accuracy_R_summary <- summarize_accuracy(accuracy_R, "RDT")
accuracy_summary <- rbind(accuracy_E_summary, accuracy_P_summary, accuracy_R_summary)

# Export to Excel
write_xlsx(accuracy_summary, path = "accuracy_summary.xlsx")
cat("Exported accuracy summary to 'accuracy_summary.xlsx'\n")