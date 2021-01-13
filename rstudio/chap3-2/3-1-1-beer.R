library(rstan)
library(bayesplot)

rstan_options(auto_write = TRUE)
options(mc.cores = prallel::detectCores())

# データの用意
file_beer_sales_2 <- read.csv("~/book-data/3-2-1-beer-sales-2.csv")
head(file_beer_sales_2, n = 3)

sample_size <- nrow(file_beer_sales_2)
sample_size

# データの傾向の把握
ggplot(file_beer_sales_2, aes(x = temperature, y = sales)) + 
				geom_point() +
				labs(title = "beer sales and temperature")

data_list <- list(
	N = sample_size,
	sales = file_beer_sales_2$sales,
	temperature = file_beer_sales_2$temperature
)

mcmc_result <- stan(
	file = "~/chap3-2/3-2-1-simple-lm-vec.stan",
	data = data_list,
	seed = 1
)

print(mcmc_result, probs = c(0.025, 0.5, 0.975))

mcmc_sample <- rstan::extract(mcmc_result, permuted = FALSE)

mcmc_combo(
	mcmc_sample,
	pars = c("Intercept", "beta", "sigma")
)

# section: prediction
temperature_pred <- 11:30
temperature_pred

data_list_pred <- list(
	N = sample_size,
	sales = file_beer_sales_2$sales,
	temperature = file_beer_sales_2$temperature,
	N_pred = length(temperature_pred),
	temperature_pred = temperature_pred
)

mcmc_result_pred <- stan(
	file = "~/chap3-2/3-2-1-simple-lm-pred.stan",
	data = data_list_pred,
	seed = 1
)

print(mcmc_result_pred, probs = c(0.025, 0.5, 0.975))

mcmc_sample_pred <- rstan::extract(mcmc_result_pred, permuted = FALSE)

mcmc_intervals(
	mcmc_sample_pred,
	regex_pars = c("sales_pred."),
	prob = 0.8,
	prob_outer = 0.95
)

mcmc_intervals(
	mcmc_sample_pred,
	pars = c("mu_pred[1]", "sales_pred[1]"),
	prob = 0.8,
	prob_outer = 0.95
)

mcmc_areas(
	mcmc_sample_pred,
	pars = c("sales_pred[1]", "sales_pred[20]"),
	prob = 0.6,
	prob_outer = 0.99
)


# section: formula
formula_lm <- formula(sales ~ temperature)
X <- model.matrix(formula_lm, file_beer_sales_2)
head(X, n = 5)

N <- nrow(file_beer_sales_2)
K <- 2
Y <- file_beer_sales_2$sales
data_list_design <- list(N = N, K = K, Y = Y, X = X)

mcmc_result_design <- stan(
	file = "~/chap3-2/3-4-1-lm-design-matrix.stan",
	data = data_list_design,
	seed = 1
)

print(mcmc_result_design, probs = c(0.025, 0.5, 0.975))

install.packages("rstantools")
install.packages("loo")
install.packages("brms")

library(brms)
simple_lm_brms <- brm(
	formula = sales ~ temperature,
	family = gaussian(link = "identity"),
	data = file_beer_sales_2,
	seed = 1
)

simple_lm_brms

as.mcmc(simple_lm_brms, combine_chains = TRUE)
plot(simple_lm_brms)

simple_lm_formula <- bf(sales ~ temperature)
prior_summary(simple_lm_brms)

simple_lm_brms_3 <- brm(
	formula = sales ~ temperature,
	family = gaussian(),
	data = file_beer_sales_2,
	seed = 1,
	prior = c(set_prior("", class = "Intercept"),
						set_prior("", class = "sigma"))
)

stanplot(simple_lm_brms,
				 type = "intervals",
				 pars = "^b_",
				 prob = 0.8,
				 prob_outer = 0.95
)

new_data <- data.frame(temperature = 20)
fitted(simple_lm_brms, new_data)

set.seed(1)
predict(simple_lm_brms, new_data)

eff <- conditional_effects(simple_lm_brms)
plot(eff, points = TRUE)



sales_weather <- read.csv("~/book-data/3-6-1-beer-sales-3.csv")
head(sales_weather, 3)

summary(sales_weather)

ggplot(data = sales_weather, mapping = aes(x = weather, y = sales)) + 
				geom_violin() +
				geom_point(aes(color = weather)) +
				labs(title = "beera and weather")

anova_brms <- brm(
	formula = sales ~ weather,
	family = gaussian(),
	data =sales_weather,
	seed = 1,
	prior = c(set_prior("", class = "Intercept"),
						set_prior("", class = "sigma"))
)
