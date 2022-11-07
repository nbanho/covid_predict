functions {
  
  matrix gp_exp_periodic_cov1(real[] x, real alpha, real rho_p, real period, real rho_e) {
    int N = num_elements(x);
    matrix[N, N] K;
    for (i in 1:N) {
      K[i,i] = alpha^2;
      for (j in (i + 1):N) {
        K[i,j] = (alpha ^ 2) * exp(-2*(sin(pi() * fabs(x[i] - x[j]) / period) ^ 2) / (rho_p ^ 2)) * exp(- ((x[i]-x[j]) ^ 2) / (2 * rho_e ^ 2));
        K[j,i] = K[i,j];
      }
    }
    return K;
  }
  
  matrix gp_exp_periodic_cov2(real[] x1, real[] x2, real alpha, real rho_p, real period, real rho_e) {
    int N1 = num_elements(x1);
    int N2 = num_elements(x2);
    matrix[N1, N2] K;
    for (i in 1:N1) {
      for (j in 1:N2) {
        K[i,j] = (alpha ^ 2) * exp(-2*(sin(pi() * fabs(x1[i] - x2[j]) / period) ^ 2) / (rho_p ^ 2)) * exp(- ((x1[i]-x2[j]) ^ 2) / (2 * rho_e ^ 2));
      }
    }
    return K;
  }
  
  vector gp_pred_rng(real[] x2,
                              vector y, real[] x1,
                              real alpha0, real rho0,
                              real alpha1, real rho1,
                              real alpha2, real rho2p, real period, real rho2e,
                              real sigma, real delta) {
    int N1 = rows(y);
    int N2 = num_elements(x2);
    vector[N2] f2;
    {
      matrix[N1, N1] k0_x1 = gp_exp_quad_cov(x1, alpha0, rho0);
      matrix[N1, N1] k1_x1 = gp_exp_quad_cov(x1, alpha1, rho1);
      matrix[N1, N1] k2_x1 = gp_exp_periodic_cov1(x1, alpha2, rho2p, period, rho2e);
      matrix[N1, N1] k_x1 = add_diag((k0_x1 + k1_x1 + k2_x1), square(sigma));
      matrix[N1, N1] l_k_x1 = cholesky_decompose(k_x1);
      vector[N1] l_k_div_y = mdivide_left_tri_low(l_k_x1, y);
      vector[N1] k_div_y = mdivide_right_tri_low(l_k_div_y', l_k_x1)';
      matrix[N1, N2] k0_x1_x2 = gp_exp_quad_cov(x1, x2, alpha0, rho0);
      matrix[N1, N2] k1_x1_x2 = gp_exp_quad_cov(x1, x2, alpha1, rho1);
      matrix[N1, N2] k2_x1_x2 = gp_exp_periodic_cov2(x1, x2, alpha2, rho2p, period, rho2e);
      matrix[N1, N2] k_x1_x2 = (k0_x1_x2 + k1_x1_x2 + k2_x1_x2);
      vector[N2] f2_mu = (k_x1_x2' * k_div_y);
      matrix[N1, N2] v_pred = mdivide_left_tri_low(l_k_x1, k_x1_x2);
      matrix[N2, N2] k0_x2 = gp_exp_quad_cov(x2, alpha0, rho0);
      matrix[N2, N2] k1_x2 = gp_exp_quad_cov(x2, alpha1, rho1);
      matrix[N2, N2] k2_x2 = gp_exp_periodic_cov1(x2, alpha2, rho2p, period, rho2e);
      matrix[N2, N2] cov_f2 = (k0_x2 + k1_x2 + k2_x2) - v_pred' * v_pred;
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
  real<lower=0> rho_short;
}

transformed data {
  real delta = 1e-9;
  vector[N1] mu = rep_vector(0, N1);
  real rho0 = N1;
  real rho1 = rho_short;
  real period = 7;
  real rho2p = 1;
  real rho2e = N1;
}

parameters {
  real<lower=0> alpha0;
  real<lower=0> alpha1;
  real<lower=0> alpha2;
  real<lower=0> sigma;
}

transformed parameters {
  matrix[N1, N1] L_K;
  {
    matrix[N1, N1] K0 = gp_exp_quad_cov(x1, alpha0, rho0);
    matrix[N1, N1] K1 = gp_exp_quad_cov(x1, alpha1, rho1);
    matrix[N1, N1] K2 = gp_exp_periodic_cov1(x1, alpha2, rho2p, period, rho2e);
    matrix[N1, N1] K = add_diag((K0 + K1 + K2), square(sigma));
    L_K = cholesky_decompose(K);
  }
}

model {
  alpha0 ~ student_t(3, 0, 5);
  alpha1 ~ student_t(5, 0, 2);
  alpha2 ~ student_t(7, 0, 2);
  
  sigma ~ std_normal();
  
  y1 ~ multi_normal_cholesky(mu, L_K);
}

generated quantities {
  vector[N2] f2 = gp_pred_rng(x2, y1, x1, alpha0, rho0, alpha1, rho1,alpha2, rho2p, period, rho2e, sigma, delta);
  vector[N2] y2;
  for (n2 in 1:N2) {
    y2[n2] = normal_rng(f2[n2], sigma);
  }
}
 