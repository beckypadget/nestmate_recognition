# startle
pp_check(startle_full, "stat_grouped", group="treatment")
pp_check(startle_null)
pp_check(startle_treat, "stat_grouped", group="treatment")
# pp_check(startle_size, "stat_grouped", group="size.1")

startle_full_plot <- mcmc_areas(startle_full, 
                       pars = c("treatment1", "treatment1:wood_typeD", "size.1"),
                       prob=0.89) +
  ggtitle("Full")


startle_treatment_plot <- mcmc_areas(startle_treat,
                            pars = c("treatment1", "treatment1:wood_typeD"),
                            prob = 0.89) +
  ggtitle("Treatment only")

startle_noint_plot <- mcmc_areas(startle_noint,
                        pars = c("treatment1", "size.1"),
                        prob = 0.89) +
  ggtitle("Treatment only")

startle_treatonly_plot <- mcmc_areas(startle_treatonly,
                            pars = c("treatment1"),
                            prob = 0.89) +
  ggtitle("Treatment only")


startle_size_plot <- mcmc_areas(startle_size,
                       pars = c("size.1"),
                       prob = 0.89) +
  ggtitle("Size only")

null_plot <- mcmc_areas(startle_null, prob=0.89) +
  ggtitle("Intercept only")

((startle_full_plot | startle_noint_plot) / (startle_treatment_plot | startle_size_plot))

startle_comparison <- bayesfactor_models(startle_full, startle_treat, startle_treatonly, startle_noint, startle_size, denominator = startle_null)
startle_comparison
startle_inclusion <- bayesfactor_inclusion(startle_comparison)
startle_inclusion

rope_size <- bayesfactor_parameters(startle_size, null=c(-1,1))
plot(rope_size)

# startle_intercept_sample <- exp(startle_size_posterior[,"b_Intercept"])
startle_treat_sample <- exp(startle_full_posterior[,"b_Intercept"] + startle_full_posterior[,"b_treatment1"]) - exp(startle_full_posterior[,"b_Intercept"])
startle_size_sample <- exp(startle_full_posterior[,"b_Intercept"] + startle_full_posterior[,"b_size.1"]) - exp(startle_full_posterior[,"b_Intercept"])

hdi(startle_full_posterior[,"b_treatment1"])

#### Effect size ####
startle_treatment_effect <- exp(startle_full_posterior[,"(Intercept)"] + startle_full_posterior[,"treatment1"]) - exp(startle_full_posterior[,"(Intercept)"])
startle_interaction_effect <- exp(startle_full_posterior[,"(Intercept)"] + startle_full_posterior[,"treatment1:wood_typeD"]) - exp(startle_full_posterior[,"(Intercept)"])
startle_size_effect <- exp(startle_full_posterior[,"(Intercept)"] + startle_full_posterior[,"size.1"]) - exp(startle_full_posterior[,"(Intercept)"])

median(startle_treatment_effect)
median(startle_interaction_effect)
median(startle_size_effect)

startle_size_effects = c()
for(i in seq(0.6, 1.6, 0.01)){
  startle_size_effects = c(startle_size_effects, median(plogis(startle_full_posterior[,"(Intercept)"] + startle_full_posterior[,"size.1"] * i) - 
                                                  plogis(startle_full_posterior[,"(Intercept)"] + startle_full_posterior[,"size.1"] * (i + 0.1)))*100) #
}
startle_df_effects = data.frame(seq(0.6, 1.6, 0.01), startle_size_effects)
colnames(startle_df_effects) = c("Size_ratio", "Effect")

ggplot(startle_df_effects, aes(x=Size_ratio, y=Effect)) + 
  geom_line() +
  theme_classic()


#### Figures ####
startle_box <- ggplot(startle, aes(x=treatment, y=count, fill=wood)) + 
  geom_boxplot(varwidth = TRUE) +
  ggtitle("a)") +
  theme_classic() +
  labs(fill="Wood treatment", 
       x = "Newcomer identity", 
       y="Recoiling from newcomer \n (count)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.position = "bottom") +
  scale_fill_manual(values=c("white","#7bccc4", "#08589e"), name="Wood type", breaks=c("D", "S"), labels=c("Different", "Same")) +
  scale_x_discrete(labels=c("Nestmate", "Non-nestmate"))
# c(0.25,0.9)
startle_point <- ggplot(startle, aes(x=size.1, y=count)) +
  geom_point() +
  ggtitle("c)") +
  theme_classic() +
  labs(x = "Size ratio", 
       y="Recoiling towards newcomer \n (count)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

startle_hdi_treat_coef <- plot(hdi(startle_full_posterior[,"treatment1"])) + 
  scale_fill_manual(values=c("#33cdc4", "#e0f3db")) +
  ggtitle("c) Newcomer identity") +
  xlab("Coefficient value") +
  ylab("Probability") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.position = "bottom")

startle_hdi_int_coef <- plot(hdi(startle_full_posterior[,"treatment1:wood_typeD"])) + 
  scale_fill_manual(values=c("#33cdc4", "#e0f3db")) +
  ggtitle("b) Newcomer identity:wood type") +
  xlab("Coefficient value") +
  ylab(" ") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.position = "none")

startle_hdi_size_coef <- plot(hdi(startle_full_posterior[,"size.1"])) +
  scale_fill_manual(values=c("#33cdc4", "#e0f3db")) +
  ggtitle("f) ") +
  xlab("Coefficient value") +
  ylab("Probability density") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

((startle_box | startle_point) / (startle_hdi_treat_coef | startle_hdi_int_coef | startle_hdi_size_coef ))+ plot_layout(heights = c(3, 1)) #+ plot_layout(guides = "collect") 
ggsave("startle_plots.png", width=11, height=7)

#### Summary stats ####
startle_full
startle_inclusion
hdi(startle_full_posterior[,"treatment1"])
hdi(startle_full_posterior[,"treatment1:wood_typeD"])
hdi(startle_full_posterior[,"size.1"])

View(startle)
length(unique(startle$code))
