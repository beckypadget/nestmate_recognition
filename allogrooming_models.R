options(buildtools.check = function(action) TRUE) # Prevents opening of dialog box to ask about re-starting R between each model run.
priors <- prior(normal(0, 2.5), class="b")
# Run garbage collection (gc()) between running models to prevent brms crashing R.

gc()
allogrooming_full <- brm(proportion ~ 
                           treatment + treatment:wood_type +
                           size.1 +
                          (1 | day) + (1 | code) + (1|order),
                         data = allogrooming,
                         family = zero_inflated_beta(),
                         prior = priors,
                         save_all_pars = TRUE,
                         iter = 5000,
                         cores = 4,
                         control = list(adapt_delta = 0.99,
                                        max_treedepth = 15))
allogrooming_full_posterior <- as.matrix(allogrooming_full)
saveRDS(allogrooming_full, "allogrooming_full.Rds")
allogrooming_full <- readRDS("allogrooming_full.Rds")
allogrooming_full_posterior <- as.matrix(allogrooming_full)

allogrooming_noint <- brm(proportion ~ 
                           treatment +
                           size.1 +
                           (1 | day) + (1 | code) + (1|order),
                         data = allogrooming,
                         family = zero_inflated_beta(),
                         prior = priors,
                         save_all_pars = TRUE,
                         iter = 5000,
                         cores = 4,
                         control = list(adapt_delta = 0.99,
                                        max_treedepth = 15))
allogrooming_noint_posterior <- as.matrix(allogrooming_noint)
saveRDS(allogrooming_noint, "allogrooming_noint.Rds")
allogrooming_noint <- readRDS("allogrooming_noint.Rds")
allogrooming_noint_posterior <- as.matrix(allogrooming_noint)

gc()
allogrooming_treat <- brm(proportion ~ 
                            treatment + treatment:wood_type +
                            (1|day) + (1|code) + (1|order),
                         data = allogrooming,
                         family = zero_inflated_beta(),
                         prior = priors,
                         save_all_pars = TRUE,
                         iter = 5000,
                         cores = 4,
                         control = list(adapt_delta = 0.99,
                                        max_treedepth = 15))
allogrooming_treat_posterior <- as.matrix(allogrooming_treat)
saveRDS(allogrooming_treat, "allogrooming_treat.Rds")
allogrooming_treat <- readRDS("allogrooming_treat.Rds")
allogrooming_treat_posterior <- as.matrix(allogrooming_treat)

gc()
allogrooming_treatonly <- brm(proportion ~ 
                            treatment +
                            (1|day) + (1|code) + (1|order),
                          data = allogrooming,
                          family = zero_inflated_beta(),
                          prior = priors,
                          save_all_pars = TRUE,
                          iter = 5000,
                          cores = 4,
                          control = list(adapt_delta = 0.99,
                                         max_treedepth = 15))
allogrooming_treatonly_posterior <- as.matrix(allogrooming_treatonly)
saveRDS(allogrooming_treatonly, "allogrooming_treatonly.Rds")


gc()
allogrooming_size <- brm(proportion ~ 
                            size.1 +
                            (1|day) + (1|code) + (1|order),
                          data = allogrooming,
                          family = zero_inflated_beta(),
                          prior = priors,
                          save_all_pars = TRUE,
                          iter = 5000,
                          cores = 4,
                          control = list(adapt_delta = 0.99,
                                         max_treedepth = 15))
allogrooming_size_posterior <- as.matrix(allogrooming_size)
saveRDS(allogrooming_size, "allogrooming_size.Rds")
allogrooming_size <- readRDS("allogrooming_size.Rds")
allogrooming_size_posterior <- as.matrix(allogrooming_size)

gc()
allogrooming_null <- brm(proportion ~ 
                           1 +
                           (1|day) + (1|code) + (1|order),
                         data = allogrooming,
                         family = zero_inflated_beta(),
                         # prior = priors,
                         save_all_pars = TRUE,
                         iter = 5000,
                         cores = 4,
                         control = list(adapt_delta = 0.99,
                                        max_treedepth = 15))
allogrooming_null_posterior <- as.matrix(allogrooming_null)
saveRDS(allogrooming_null, "allogrooming_null.Rds")
allogrooming_null <- readRDS("allogrooming_null.Rds")
allogrooming_null_posterior <- as.matrix(allogrooming_null)
