library(rstan)

animal_num <- read.csv("~/book-data/2-5-1-animal-num.csv")
head(animal_num, n = 3)

sample_size <- nrow(animal_num)

data_list <- list(animal_num = animal_num$animal_num, N = sample_size)

mcmc_normal <- stan(
	file = "~/chap5/2-5-1-normal-dist.stan",
  data = data_list,
  seed = 1
)

mcmc_poisson <- stan(
  file = "2-5-2-poisson-dist.stan",
  data = data_list,
  seed = 1
)

print(mcmc_normal, par = c("mu", "sigma", "lp__"))
print(mcmc_poisson, par = c("lambda", "lp__"))

y_rep_normal <- rstan::extract(mcmc_normal)$pred
y_rep_poisson <- rstan::extract(mcmc_poisson)$pred
dim(y_rep_normal)
y_rep_normal[1,]
y_rep_poisson[1,]

ppc_hist(y = animal_num$animal_num, yrep = y_rep_normal[1:5,])
ppc_hist(y = animal_num$animal_num, yrep = y_rep_poisson[1:5,])
