library(brms)
allogrooming <- read.csv("Data/allogrooming_all.csv")
allogrooming$proportion <- allogrooming$proportion/100 # make percentages proportions
allogrooming$day <- as.factor(allogrooming$day)
allogrooming$code <- as.factor(allogrooming$code)
allogrooming$treatment <- as.factor(allogrooming$treatment)
allogrooming$wood_type <- as.factor(allogrooming$wood)
allogrooming$inbred <- as.factor(allogrooming$inbred)

options(buildtools.check = function(action) TRUE) # Prevents opening of dialog box to ask about re-starting R between each model run.
priors <- prior(normal(0, 2.5), class="b")
# Run garbage collection (gc()) between running models to prevent brms crashing R.


gc()
allogrooming_full <- brm(proportion ~ 
                           treatment + treatment:wood_type +
                           size.1 + inbred +
                          (1 | day) + (1 | code) + (1|order),
                         data = allogrooming %>% filter(!is.na(inbred)),
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

gc()
allogrooming_noinbred <- brm(proportion ~ 
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
allogrooming_noinbred_posterior <- as.matrix(allogrooming_noinbred)

saveRDS(allogrooming_noinbred, "allogrooming_noinbred.Rds")
allogrooming_noinbred <- readRDS("allogrooming_noinbred.Rds")
allogrooming_noinbred_posterior <- as.matrix(allogrooming_noinbred)

gc()
allogrooming_noint <- brm(proportion ~ 
                           treatment +
                           size.1 + inbred +
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
                            treatment + treatment:wood_type + inbred +
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
                            treatment + inbred +
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
                            size.1 + inbred +
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
allogrooming_inbred <- brm(proportion ~ 
                           inbred +
                           (1|day) + (1|code) + (1|order),
                         data = allogrooming,
                         family = zero_inflated_beta(),
                         prior = priors,
                         save_all_pars = TRUE,
                         iter = 5000,
                         cores = 4,
                         control = list(adapt_delta = 0.99,
                                        max_treedepth = 15))
allogrooming_inbred_posterior <- as.matrix(allogrooming_inbred)
saveRDS(allogrooming_inbred, "allogrooming_inbred.Rds")
allogrooming_inbred <- readRDS("allogrooming_inbred.Rds")
allogrooming_inbred_posterior <- as.matrix(allogrooming_inbred)

gc()
allogrooming_null <- brm(proportion ~ 
                           1 +
                           (1|day) + (1|code) + (1|order),
                         data = allogrooming %>% filter(!is.na(inbred)),
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


## Including the grandparental colonies in the model
allogrooming_plus <- read.csv("Data/allogrooming_plus.csv")
allogrooming_plus$proportion <- allogrooming_plus$proportion/100 # make percentages proportions
allogrooming_plus$day <- as.factor(allogrooming_plus$day)
allogrooming_plus$code <- as.factor(allogrooming_plus$code)
allogrooming_plus$treatment <- as.factor(allogrooming_plus$treatment)
allogrooming_plus$wood_type <- as.factor(allogrooming_plus$wood)
allogrooming_plus$inbred <- as.factor(allogrooming_plus$inbred)

gc()
allogrooming_super <- brm(proportion ~ 
                            treatment + treatment:wood_type +
                            size.1 + inbred + Parent_colony_1 + Parent_colony_2 + Intruder_parent_colony_1 + Intruder_parent_colony_2 +
                            (1 | day) + (1 | code) + (1|order),
                          data = allogrooming_plus,
                          family = zero_inflated_beta(),
                          prior = priors,
                          save_all_pars = TRUE,
                          iter = 5000,
                          cores = 4,
                          control = list(adapt_delta = 0.99,
                                         max_treedepth = 15))
allogrooming_super_posterior <- as.matrix(allogrooming_super)
saveRDS(allogrooming_super, "allogrooming_super.Rds")
allogrooming_super <- readRDS("allogrooming_super.Rds")
allogrooming_super_posterior <- as.matrix(allogrooming_super)
