library(rstan)
library(brms)

rstan_options(auto_write = TRUE)

sales_climate <- read.csv("~/chap3-2/3-7-1-beer-sales-4.csv")
head(sales_climate, 3)

summary(sales_climate)

ggplot(data = sales_climate,
			 mapping = aes(x = temperature, y = sales)) + 
	geom_point(aes(color = weather)) + 
	labs(title = "beera, temp and weather")
)


lm_brms <- brm(
	formula = sales ~ weather + temperature,
	family = gaussian(), 
	data = sales_climate,
	seed = 1,
	prior = c(set_prior("", class = "Intercept"),
						set_prior("", class = "sigma")
	)
)

lm_brms
