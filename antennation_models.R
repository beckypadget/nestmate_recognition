options(buildtools.check = function(action) TRUE) # Prevents opening of dialog box to ask about re-starting R between each model run.
priors <- prior(normal(0, 2.5), class="b")
# Run garbage collection (gc()) between running models to prevent brms crashing R.

gc()
antennation_full <- brm(proportion ~ 
                           treatment + treatment:wood_type +
                           size.1 +
                           (1 | day) + (1 | code) + (1|order),
                         data = antennation,
                         family = zero_inflated_beta(),
                         prior = priors,
                         save_all_pars = TRUE,
                         iter = 5000,
                         cores = 4,
                         control = list(adapt_delta = 0.99,
                                        max_treedepth = 15))
antennation_full_posterior <- as.matrix(antennation_full)
saveRDS(antennation_full, "antennation_full.Rds")
antennation_full <- readRDS("antennation_full.Rds")
antennation_full_posterior <- as.matrix(antennation_full)

antennation_noint <- brm(proportion ~ 
                            treatment +
                            size.1 +
                            (1 | day) + (1 | code) + (1|order),
                          data = antennation,
                          family = zero_inflated_beta(),
                          prior = priors,
                          save_all_pars = TRUE,
                          iter = 5000,
                          cores = 4,
                          control = list(adapt_delta = 0.99,
                                         max_treedepth = 15))
antennation_noint_posterior <- as.matrix(antennation_noint)
saveRDS(antennation_noint, "antennation_noint.Rds")
antennation_noint <- readRDS("antennation_noint.Rds")
antennation_noint_posterior <- as.matrix(antennation_noint)

gc()
antennation_treat <- brm(proportion ~ 
                            treatment + treatment:wood_type +
                            (1|day) + (1|code) + (1|order),
                          data = antennation,
                          family = zero_inflated_beta(),
                          prior = priors,
                          save_all_pars = TRUE,
                          iter = 5000,
                          cores = 4,
                          control = list(adapt_delta = 0.99,
                                         max_treedepth = 15))
antennation_treat_posterior <- as.matrix(antennation_treat)
saveRDS(antennation_treat, "antennation_treat.Rds")
antennation_treat <- readRDS("antennation_treat.Rds")
antennation_treat_posterior <- as.matrix(antennation_treat)

gc()
antennation_treatonly <- brm(proportion ~ 
                           treatment +
                           (1|day) + (1|code) + (1|order),
                         data = antennation,
                         family = zero_inflated_beta(),
                         prior = priors,
                         save_all_pars = TRUE,
                         iter = 5000,
                         cores = 4,
                         control = list(adapt_delta = 0.99,
                                        max_treedepth = 15))
antennation_treatonly_posterior <- as.matrix(antennation_treatonly)
saveRDS(antennation_treatonly, "antennation_treatonly.Rds")
antennation_treatonly <- readRDS("antennation_treatonly.Rds")
antennation_treatonly_posterior <- as.matrix(antennation_treatonly)

gc()
antennation_size <- brm(proportion ~ 
                           size.1 +
                           (1|day) + (1|code) + (1|order),
                         data = antennation,
                         family = zero_inflated_beta(),
                         prior = priors,
                         save_all_pars = TRUE,
                         iter = 5000,
                         cores = 4,
                         control = list(adapt_delta = 0.99,
                                        max_treedepth = 15))
antennation_size_posterior <- as.matrix(antennation_size)
saveRDS(antennation_size, "antennation_size.Rds")
antennation_size <- readRDS("antennation_size.Rds")
antennation_size_posterior <- as.matrix(antennation_size)

gc()
antennation_null <- brm(proportion ~ 
                           1 +
                           (1|day) + (1|code) + (1|order),
                         data = antennation,
                         family = zero_inflated_beta(),
                         # prior = priors,
                         save_all_pars = TRUE,
                         iter = 5000,
                         cores = 4,
                         control = list(adapt_delta = 0.99,
                                        max_treedepth = 15))
antennation_null_posterior <- as.matrix(antennation_null)
saveRDS(antennation_null, "antennation_null.Rds")
antennation_null <- readRDS("antennation_null.Rds")
antennation_null_posterior <- as.matrix(antennation_null)


antennation_all <- brm(proportion ~ 
                          treatment + treatment:wood_type +
                          size.1 + treatment:size.1 +
                          (1 | day) + (1 | code) + (1|order),
                        data = antennation,
                        family = zero_inflated_beta(),
                        prior = priors,
                        save_all_pars = TRUE,
                        iter = 5000,
                        cores = 4,
                        control = list(adapt_delta = 0.99,
                                       max_treedepth = 15))
antennation_all_posterior <- as.matrix(antennation_all)
saveRDS(antennation_all, "antennation_all.Rds")
