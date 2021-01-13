library(rstan)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

interaction_1 <- read.csv("~/book-data/3-10-1-interaction-1.csv")
head(interaction_1, n = 3)
summary(interaction_1)

model.matrix(sales ~ publicity * bargen, interaction_1)

interaction_brms_1 <- brm(
	formula = sales ~ publicity * bargen,
	family = gaussian(link = "identity"),
	data = interaction_1,
	seed = 1,
	prior = c(set_prior("", class = "Intercept"),
						set_prior("", class = "sigma")))
)

interaction_brms_1

eff_1 <- conditional_effects(interaction_brms_1, effects = "publicity:bargen")
plot(eff_1, points = T)

interactions_2 <- read.csv("~/book-data/3-10-2-interactions-2.csv")
head(interactions_2, n = 3)

summary(interactions_2)

interaction_brms_2 <- brm(
	formula = sales ~ publicity * temperature,
	family = gaussian(link = "identity"),
	data = interactions_2,
	seed = 1,
	prior = c(set_prior("", class = "Intercept"),
						set_prior("", class = "sigma")
	)
)


