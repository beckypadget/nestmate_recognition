library(rstanarm)

startle <- read.csv("Data/startle_all.csv")
startle$day <- as.factor(startle$day)
startle$code <- as.factor(startle$code)
startle$treatment <- as.factor(startle$treatment)
startle$wood_type <- as.factor(startle$wood)
startle$inbred <- as.factor(startle$inbred)

#### Models ####
startle_full <- stan_glmer(count ~ 
                             treatment + treatment:wood_type +
                             size.1 + inbred + 
                             (1 | day) + (1 | code) + (1|order),
                         data = startle,
                         family = neg_binomial_2(),
                         prior = normal(0,2.5),
                         diagnostic_file="startle_full.csv", 
                         adapt_delta = 0.99,
                         cores=4)
startle_full_posterior <- as.matrix(startle_full)
# saveRDS(startle_full, "startle_full.Rds")
# 
# startle_full <- readRDS("startle_full.Rds")
# startle_full_posterior <- as.matrix(startle_full)

startle_noinbred <- stan_glmer(count ~ 
                             treatment + treatment:wood_type +
                             size.1 +
                             (1 | day) + (1 | code) + (1|order),
                           data = startle  %>% filter(!is.na(inbred)),
                           family = neg_binomial_2(),
                           prior = normal(0,2.5),
                           diagnostic_file="startle_noinbred.csv", 
                           adapt_delta = 0.99,
                           cores=4)
startle_noinbred_posterior <- as.matrix(startle_noinbred)
# saveRDS(startle_noinbred, "startle_noinbred.Rds")
# 
# startle_noinbred <- readRDS("startle_noinbred.Rds")
# startle_noinbred_posterior <- as.matrix(startle_noinbred)
# 

startle_treat <- stan_glmer(count ~ 
                             treatment + treatment:wood_type + 
                             (1 | code) + (1|day) + (1|order),
                           data = startle,
                           family = neg_binomial_2(),
                           prior = normal(0,2.5),
                           diagnostic_file="startle_treat.csv", 
                           adapt_delta = 0.99,
                           cores=4)
startle_treat_posterior <- as.matrix(startle_treat)
# saveRDS(startle_treat, "startle_treat.Rds")
# 
# startle_treat <- readRDS("startle_treat.Rds")
# startle_treat_posterior <- as.matrix(startle_treat)

startle_noint <- stan_glmer(count ~ 
                              treatment + size.1 +
                              (1 | code) + (1|day) + (1|order),
                            data = startle,
                            family = neg_binomial_2(),
                            prior = normal(0,2.5),
                            diagnostic_file="startle_noint.csv", 
                            adapt_delta = 0.99,
                            cores=4)
startle_noint_posterior <- as.matrix(startle_noint)
# saveRDS(startle_noint, "startle_noint.Rds")
# 
# startle_noint <- readRDS("startle_noint.Rds")
# startle_noint_posterior <- as.matrix(startle_noint)


startle_treatonly <- stan_glmer(count ~ 
                              treatment +
                              (1 | code) + (1|day) + (1|order),
                            data = startle,
                            family = neg_binomial_2(),
                            prior = normal(0,2.5),
                            diagnostic_file="startle_treatonly.csv", 
                            adapt_delta = 0.99,
                            cores=4)
startle_treatonly_posterior <- as.matrix(startle_treatonly)
# saveRDS(startle_treatonly, "startle_treatonly.Rds")
# startle_treatonly <- readRDS("startle_treatonly.Rds")
# startle_treatonly_posterior <- as.matrix(startle_treatonly)

startle_size <- stan_glmer(count ~ 
                              size.1 +
                              (1 | code) + (1|day) + (1|order),
                            data = startle,
                            family = neg_binomial_2(),
                            prior = normal(0,2.5),
                            diagnostic_file="startle_size.csv", 
                            adapt_delta = 0.99,
                            cores=4)
startle_size_posterior <- as.matrix(startle_size)
# saveRDS(startle_size, "startle_size.Rds")
# startle_size <- readRDS("startle_size.Rds")
# startle_size_posterior <- as.matrix(startle_size)


#### Intercept + random only ####
startle_null <- stan_glm(count ~ 1 ,
                           data = startle,
                           family = neg_binomial_2(),
                           prior = normal(0,2.5),
                           diagnostic_file="startle_null.csv",
                         adapt_delta = 0.99,
                         cores=4)
startle_null_posterior <- as.matrix(startle_null)
# saveRDS(startle_null, "startle_null.Rds")
# startle_null <- readRDS("startle_size.Rds")
# startle_null_posterior <- as.matrix(startle_null)
