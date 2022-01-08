functions {
  vector gp_pred_rng(real[] x2,
                              vector y, real[] x1,
                              real alpha0, real rho0,
                              real alpha1, real rho1,
                              real sigma, real delta) {
    int N1 = rows(y);
    int N2 = num_elements(x2);
    vector[N2] f2;
    {
      matrix[N1, N1] k0_x1 = gp_exp_quad_cov(x1, alpha0, rho0);
      matrix[N1, N1] k1_x1 = gp_exp_quad_cov(x1, alpha1, rho1);
      matrix[N1, N1] k_x1 = add_diag(k0_x1 + k1_x1, sigma);
      matrix[N1, N1] l_k_x1 = cholesky_decompose(k_x1);
      vector[N1] l_k_div_y = mdivide_left_tri_low(l_k_x1, y);
      vector[N1] k_div_y = mdivide_right_tri_low(l_k_div_y', l_k_x1)';
      matrix[N1, N2] k0_x1_x2 = gp_exp_quad_cov(x1, x2, alpha0, rho0);
      matrix[N1, N2] k1_x1_x2 = gp_exp_quad_cov(x1, x2, alpha1, rho1);
      matrix[N1, N2] k_x1_x2 = k0_x1_x2 + k1_x1_x2;
      vector[N2] f2_mu = (k_x1_x2' * k_div_y);
      matrix[N1, N2] v_pred = mdivide_left_tri_low(l_k_x1, k_x1_x2);
      matrix[N2, N2] k0_x2 = gp_exp_quad_cov(x2, alpha0, rho0);
      matrix[N2, N2] k1_x2 = gp_exp_quad_cov(x2, alpha1, rho1);
      matrix[N2, N2] cov_f2 = (k0_x2 + k1_x2) - v_pred' * v_pred;
      matrix[N2, N2] diag_delta = diag_matrix(rep_vector(delta, N2));
      f2 = multi_normal_rng(f2_mu, cov_f2 + diag_delta);
    }
    return f2;
  }
}

data {
  int<lower=1> N1;
  int<lower=1> N2;
  real x1[N1];
  vector[N1] y1;
  real x2[N2];
  real rho1_shape;
  real rho1_scale;
}

transformed data {
  real delta = 1e-9;
  real period = 7;
  vector[N1] mu = rep_vector(0, N1);
  real rho0 = N1;
}

parameters {
  real<lower=0> alpha0;
  real<lower=0> rho1;
  real<lower=0> alpha1;
  real<lower=0> sigma;
}

transformed parameters {
  matrix[N1, N1] L_K;
  {
    matrix[N1, N1] K0 = gp_exp_quad_cov(x1, alpha0, rho0);
    matrix[N1, N1] K1 = gp_exp_quad_cov(x1, alpha1, rho1);
    matrix[N1, N1] K = add_diag(K0 + K1, square(sigma));
    L_K = cholesky_decompose(K);
  }
}

model {
  alpha0 ~ normal(0, 2);
  alpha1 ~ normal(0, 2);
  rho1 ~ inv_gamma(rho1_shape, rho1_scale);

  sigma ~ std_normal();
  
  y1 ~ multi_normal_cholesky(mu, L_K);
}

generated quantities {
  vector[N2] f2 = gp_pred_rng(x2, y1, x1, alpha0, rho0, alpha1, rho1, sigma, delta);
  vector[N2] y2;
  for (n2 in 1:N2) {
    y2[n2] = normal_rng(f2[n2], sigma);
  }
}
 