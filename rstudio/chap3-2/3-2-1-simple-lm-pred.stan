data {
	int N;
	vector[N] sales;
	vector[N] temperature;

	int N_pred;
	vector[N_pred] temperature_pred;
}

parameters {
	real Intercept;
	real beta;
	real<lower=0> sigma;
}

model {
	for (i in 1:N) {
		sales[i] ~ normal(Intercept + beta*temperature[i], sigma);
	}
}

generated quantities {
	vector[N_pred] mu_pred;
	vector[N_pred] sales_pred;

	for (i in 1:N_pred) {
		// 気温によって変化するビールの売り上げ予測値
		mu_pred[i] = Intercept + beta*temperature_pred[i];
		// 平均がmu_predで、標準偏差がsigmaである正規分布から得られる売り上げ予測値
		// mu_predを平均値とした正規分布から、さらに乱数を生成することによって、MCMCサンプルが得られる
		sales_pred[i] = normal_rng(mu_pred[i], sigma);
	}
}
