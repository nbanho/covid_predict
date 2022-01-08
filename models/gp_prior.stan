data {
  int<lower=1> T;
  real x[T];
  real<lower=0> alpha1;
  real<lower=0> rho1;
  real<lower=0> alpha2;
  real<lower=0> rho2p;
  real<lower=0> rho2e;
  real<lower=0> alpha3;
  real<lower=0> rho3;
}

transformed data {
  matrix[T, T] K1 = gp_exp_quad_cov(x, alpha1, rho1);
  matrix[T, T] K2;
  matrix[T, T] K3 = gp_exp_quad_cov(x, alpha3, rho3);
  matrix[T, T] L;
  
  for (i in 1:T) {
      K2[i,i] = alpha2^2 + 1e-10;
      for (j in (i + 1):T) {
        K2[i,j] = (alpha2 ^ 2) * exp(-2*(sin(pi() * fabs(x[i] - x[j]) / 7) ^ 2) / (rho2p ^ 2)) * exp( - (x[i] - x[j]) ^ 2 / (2 * rho2e ^ 2));
        K2[j,i] = K2[i,j];
      }
    }
  L = cholesky_decompose(K2 + K3 + diag_matrix(rep_vector(1e-10, T)));
}

parameters { }

model { }

generated quantities {
  vector[T] y = multi_normal_cholesky_rng(rep_vector(0, T), L);
}