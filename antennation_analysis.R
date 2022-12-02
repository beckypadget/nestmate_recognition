# antennation
pp_check(antennation_full, "stat_grouped", group="treatment")
pp_check(antennation_null)
pp_check(antennation_treat, "stat_grouped", group="treatment")
# pp_check(antennation_size, "stat_grouped", group="size.1")

ant_all <- mcmc_areas(antennation_all, 
                       pars = c("b_treatment1", "b_treatment1:wood_typeD", "b_size.1", "b_treatment1:size.1"),
                       prob=0.89) +
  ggtitle("Full")
ant_all

ant_full <- mcmc_areas(antennation_full, 
                        pars = c("b_treatment1", "b_treatment1:wood_typeD", "b_size.1"),
                        prob=0.89) +
  ggtitle("Full")
ant_full

ant_treatment <- mcmc_areas(antennation_treat,
                             pars = c("b_treatment1", "b_treatment1:wood_typeD"),
                             prob = 0.89) +
  ggtitle("Treatment only")

ant_noint <- mcmc_areas(antennation_noint,
                         pars = c("b_treatment1", "b_size.1"),
                         prob = 0.89) +
  ggtitle("Treatment only")

ant_treatonly <- mcmc_areas(antennation_treatonly,
                        pars = c("b_treatment1"),
                        prob = 0.89) +
  ggtitle("Treatment only")


ant_size <- mcmc_areas(antennation_size,
                        pars = c("b_size.1"),
                        prob = 0.89) +
  ggtitle("Size only")

null_p <- mcmc_areas(antennation_null, prob=0.89) +
  ggtitle("Intercept only")

((ant_full | ant_noint) / (ant_treatment | ant_size))

comparison <- bayesfactor_models(antennation_full, antennation_treat, antennation_treatonly, antennation_noint, antennation_size, denominator = antennation_null)
comparison
inclusion <- bayesfactor_inclusion(comparison)
inclusion

rope_size <- bayesfactor_parameters(antennation_size, null=c(-1,1))
plot(rope_size)

# ant_intercept_sample <- plogis(antennation_size_posterior[,"b_Intercept"])
ant_treat_sample <- plogis(antennation_full_posterior[,"b_Intercept"] + antennation_full_posterior[,"b_treatment1"]) - plogis(antennation_full_posterior[,"b_Intercept"])
ant_size_sample <- plogis(antennation_full_posterior[,"b_Intercept"] + antennation_full_posterior[,"b_size.1"]) - plogis(antennation_full_posterior[,"b_Intercept"])

hdi(antennation_full_posterior[,"b_treatment1"])

#### Effect sizes ####
ant_treatment_effect <- plogis(antennation_full_posterior[,"b_Intercept"] + antennation_full_posterior[,"b_treatment1"]) - plogis(antennation_full_posterior[,"b_Intercept"])
ant_interaction_effect <- plogis(antennation_full_posterior[,"b_Intercept"] + antennation_full_posterior[,"b_treatment1:wood_typeD"]) - plogis(antennation_full_posterior[,"b_Intercept"])
ant_size_effect <- plogis(antennation_full_posterior[,"b_Intercept"] + antennation_full_posterior[,"b_size.1"]*0.947) - plogis(antennation_full_posterior[,"b_Intercept"] + antennation_full_posterior[,"b_size.1"]*1.04)

median(antennation$size.1)*1.1

median(ant_treatment_effect)
median(ant_interaction_effect)
median(ant_size_effect)*100


ant_size_effects = c()
for(i in seq(0.6, 1.6, 0.01)){
  ant_size_effects = c(ant_size_effects, median(plogis(antennation_full_posterior[,"b_Intercept"] + antennation_full_posterior[,"b_size.1"] * i) - 
                                                    plogis(antennation_full_posterior[,"b_Intercept"] + antennation_full_posterior[,"b_size.1"] * (i + 0.1)))*100) #
}
ant_df_effects = data.frame(seq(0.6, 1.6, 0.01), ant_size_effects)
colnames(ant_df_effects) = c("Size_ratio", "Effect")

ggplot(ant_df_effects, aes(x=Size_ratio, y=Effect)) + 
  geom_line() +
  theme_classic()

#### Figures ####
ant_box <- ggplot(antennation, aes(x=treatment, y=proportion, fill=wood)) + 
  geom_boxplot(varwidth = TRUE) +
  ggtitle("a)") +
  theme_classic() +
  labs(fill="Wood treatment", 
       x = "Newcomer identity", 
       y="Antennation towards newcomer \n (proportion time)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.position = "bottom") +
  scale_fill_manual(values=c("white","#7bccc4", "#08589e"), name="Wood type", breaks=c("D", "S"), labels=c("Different", "Same")) +
  scale_x_discrete(labels=c("Nestmate", "Non-nestmate"))

ant_point <- ggplot(antennation, aes(x=size.1, y=proportion)) +
  geom_point() +
  ggtitle("b)") +
  theme_classic() +
  labs(x = "Size ratio", 
       y="Antennation towards newcomer \n (proportion time)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

ant_hdi_treat_coef <- plot(hdi(antennation_full_posterior[,"b_treatment1"])) + 
  scale_fill_manual(values=c("#33cdc4", "#e0f3db")) +
  ggtitle("c) Newcomer identity") +
  xlab("Coefficient value") +
  ylab("Probability") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.position = "bottom")

ant_hdi_int_coef <- plot(hdi(antennation_full_posterior[,"b_treatment1:wood_typeD"])) + 
  scale_fill_manual(values=c("#33cdc4", "#e0f3db")) +
  ggtitle("b) Newcomer identity:wood type") +
  xlab("Coefficient value") +
  ylab(" ") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.position = "none")

ant_hdi_size_coef <- plot(hdi(antennation_full_posterior[,"b_size.1"])) + 
  scale_fill_manual(values=c("#33cdc4", "#e0f3db")) +
  ggtitle("e)") +
  xlab("Coefficient value") +
  ylab("Probability density") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

((ant_box | ant_point) / (ant_hdi_treat_coef | ant_hdi_int_coef | ant_hdi_size_coef)) + plot_layout(heights = c(3, 1))
ggsave("antennation_plots.png", width=11, height=7)

mean(antennation$proportion)

#### Summary stats ####
summary(antennation_full)
inclusion
hdi(antennation_full_posterior[,"b_size.1"])


test_comparison <- bayesfactor_models(antennation_full, denominator = antennation_all)
test_comparison

View(antennation)
