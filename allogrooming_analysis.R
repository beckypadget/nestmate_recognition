# allogrooming
pp_check(allogrooming_full, "stat_grouped", group="treatment")
pp_check(allogrooming_null)
pp_check(allogrooming_treat, "stat_grouped", group="treatment")
# pp_check(allogrooming_size, "stat_grouped", group="size.1")

pairs(allogrooming_size)

allo_full <- mcmc_areas(allogrooming_full, 
                   pars = c("b_treatment1", "b_treatment1:wood_typeD", "b_size.1"),
                   prob=0.89) +
              ggtitle("Full")
allo_full

allo_treatment <- mcmc_areas(allogrooming_treat,
                        pars = c("b_treatment1", "b_treatment1:wood_typeD"),
                        prob = 0.89) +
              ggtitle("Treatment only")

allo_noint <- mcmc_areas(allogrooming_noint,
                             pars = c("b_treatment1", "b_size.1"),
                             prob = 0.89) +
  ggtitle("Treatment only")

allo_size <- mcmc_areas(allogrooming_size,
                   pars = c("b_size.1"),
                   prob = 0.89) +
              ggtitle("Size only")
plot(allogrooming_size)
null_p <- mcmc_areas(allogrooming_null, prob=0.89) +
              ggtitle("Intercept only")
  
((allo_full | allo_noint) / (allo_treatment | allo_size))

allo_comparison <- bayesfactor_models(allogrooming_full, allogrooming_treat, allogrooming_treatonly, allogrooming_noint, allogrooming_size, denominator = allogrooming_null)
allo_comparison
allo_inclusion <- bayesfactor_inclusion(allo_comparison)
allo_inclusion

#### Effect size ####
allo_treatment_effect <- plogis(allogrooming_full_posterior[,"b_Intercept"] + allogrooming_full_posterior[,"b_treatment1"]) - plogis(allogrooming_full_posterior[,"b_Intercept"])
allo_interaction_effect <- plogis(allogrooming_full_posterior[,"b_Intercept"] + allogrooming_full_posterior[,"b_treatment1:wood_typeD"]) - plogis(allogrooming_full_posterior[,"b_Intercept"])
allo_size_effect <- plogis(allogrooming_full_posterior[,"b_Intercept"] + allogrooming_full_posterior[,"b_size.1"]*0.947) - plogis(allogrooming_full_posterior[,"b_Intercept"] + allogrooming_full_posterior[,"b_size.1"]*1.041)
# allo_size_effect <- plogis(allogrooming_full_posterior[,"b_Intercept"] + allogrooming_full_posterior[,"b_size.1"]) - plogis(allogrooming_full_posterior[,"b_Intercept"])

median(allogrooming$size.1)*1.1


median(allo_treatment_effect)
median(allo_interaction_effect)

allo_size_effect <- plogis(allogrooming_full_posterior[,"b_Intercept"] + allogrooming_full_posterior[,"b_size.1"]*1.5) - plogis(allogrooming_full_posterior[,"b_Intercept"] + allogrooming_full_posterior[,"b_size.1"]*1.6)
median(allo_size_effect)*100

allo_size_effects = c()
for(i in seq(0.6, 1.6, 0.01)){
  allo_size_effects = c(allo_size_effects, median(plogis(allogrooming_full_posterior[,"b_Intercept"] + allogrooming_full_posterior[,"b_size.1"] * i) - 
                                                    plogis(allogrooming_full_posterior[,"b_Intercept"] + allogrooming_full_posterior[,"b_size.1"] * (i + 0.1)))*100) #
}
df_effects = data.frame(seq(0.6, 1.6, 0.01), allo_size_effects)
colnames(df_effects) = c("Size_ratio", "Effect")

ggplot(df_effects, aes(x=Size_ratio, y=Effect)) + 
  geom_line() +
  theme_classic()

#### Figures ####
allo_box <- ggplot(allogrooming, aes(x=treatment, y=proportion, fill=wood)) + 
  geom_boxplot(varwidth = TRUE) +
  ggtitle("a)") +
  theme_classic() +
  labs(fill="Wood treatment", 
       x = "Newcomer identity", 
       y="Allogrooming towards newcomer \n (proportion time)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.position = "bottom") +
  scale_fill_manual(values=c("white","#7bccc4", "#08589e"), name="Wood type", breaks=c("D", "S"), labels=c("Different", "Same")) +
  scale_x_discrete(labels=c("Nestmate", "Non-nestmate"))
allo_box


allo_point <- ggplot(allogrooming, aes(x=size.1, y=proportion)) +
  geom_point() +
  ggtitle("a)") +
  theme_classic() +
  labs(x = "Size ratio", 
       y="Allogrooming towards newcomer \n (proportion time)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))
# allo_point

allo_hdi_treat_coef <- plot(hdi(allogrooming_full_posterior[,"b_treatment1"])) + 
  scale_fill_manual(values=c("#33cdc4", "#e0f3db")) +
  ggtitle("c) Newcomer identity") +
  xlab("Coefficient value") +
  ylab("Probability") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.position = "bottom")

allo_hdi_int_coef <- plot(hdi(allogrooming_full_posterior[,"b_treatment1:wood_typeD"])) + 
  scale_fill_manual(values=c("#33cdc4", "#e0f3db")) +
  ggtitle("b) Newcomer identity:wood type") +
  xlab("Coefficient value") +
  ylab(" ") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.position = "none")

allo_hdi_size_coef <- plot(hdi(allogrooming_full_posterior[,"b_size.1"])) + 
  scale_fill_manual(values=c("#33cdc4", "#e0f3db")) +
  ggtitle("d) ") +
  xlab("Coefficient value") +
  ylab("Probability density") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) 

((allo_box | allo_point) / (allo_hdi_treat_coef | allo_hdi_int_coef | allo_hdi_size_coef)) + plot_layout(heights = c(3, 1))

ggsave("allogrooming_plots.png", width=11, height=7)


#### Summary stats ####
summary(allogrooming_full)
allo_inclusion
hdi(allogrooming_full_posterior[,"b_treatment1"])
hdi(allogrooming_full_posterior[,"b_treatment1:wood_typeD"])
hdi(allogrooming_full_posterior[,"b_size.1"])
