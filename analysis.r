
rm(list = ls())

source("./project_support.r")

dir_init("./temp")

n_draws <- 1e5 # for simulations
n_iter <- 2000
n_chains <- 2



print("prep data")

# simulated
x_gb_sim <- rtetris(n_draws, algo = "gameboy", verbose = TRUE)
x_nes_sim <- rtetris(n_draws, algo = "nes", verbose = TRUE)
x_modern_sim <- rtetris(n_draws, algo = "modern", verbose = TRUE)

# empirical
draws <- read.csv("./tetris_draws.csv", stringsAsFactors = FALSE)
d <- draws[grep("gameboy", draws$game_version), ]

x_gb <- d$letter
y_gb <- match(x_gb, tetrominoes)

run_ids <- unique(d$run_id)
out <- character()
for (j in seq_along(run_ids)) {
  run_pieces <- paste(d$letter[d$run_id == run_ids[j]], collapse = "")
  out <- paste0(out, "|", run_pieces)
}

d <- draws[grep("nes", draws$game_version), ]

x_nes <- d$letter
y_nes <- match(x_nes, tetrominoes)

run_ids <- unique(d$run_id)
out <- character()
for (j in seq_along(run_ids)) {
  run_pieces <- paste(d$letter[d$run_id == run_ids[j]], collapse = "")
  out <- paste0(out, "|", run_pieces)
}



print("fit models")

print("fit model0 for gameboy")

outcome_prior <- c(1, 1, 1, 1, 1, 1, 1)

dat_list <- list(
  K = 7,
  y = y_gb,
  T = length(y_gb),
  alpha = outcome_prior
)

m0 <- stan(file = "./stan/categorical.stan",
 data = dat_list, iter = n_iter, chains = n_chains)

model0_gb_post <- extract.samples(m0)



print("fit model0 to nes")

outcome_prior <- c(1, 1, 1, 1, 1, 1, 1)

dat_list <- list(
  K = 7,
  y = y_nes,
  T = length(y_nes),
  alpha = outcome_prior
)

m0 <- stan(file = "./stan/categorical.stan",
 data = dat_list, iter = n_iter, chains = n_chains)

model0_nes_post <- extract.samples(m0)


print("fit the first-order models to nes")

# fit the single-chain version first

transition_prior <- matrix(1, 7, 7)

dat_list <- list(
  K = 7,
  z = y_nes,
  T = length(y_nes),
  theta_prior = transition_prior
)

m1_single <- stan(file = "./stan/first-order-markov-single.stan",
 data = dat_list, iter = n_iter, chains = n_chains)

model1_single_post <- extract.samples(m1_single)

# fit the multi-chain version

transition_prior <- matrix(1, 7, 7)

d <- draws[grep("nes", draws$game_version), ]
run_ids <- unique(d$run_id)
N <- length(run_ids)
T <- rep(NA, N)
for (i in 1:N) T[i] <- sum(d$run_id == run_ids[i])
S <- c(0, cumsum(T)[-N])

dat_list <- list(
  z = y_nes,
  K = 7,
  N = N,
  T = T,
  S = S,
  Q = length(y_nes),
  theta_prior = transition_prior
)

m1 <- stan(file = "./stan/first-order-markov.stan",
 data = dat_list, iter = n_iter, chains = n_chains)

model1_post <- extract.samples(m1)



print("fit the second-order models to gameboy data")

transitions <- matrix(1, nrow = 7, ncol = 7)
transitions <- t(apply(transitions, 1, simplex))
theta_prior <- vector("list", 7)
for (i in 1:7){
  theta_prior[[i]] <- transitions
}

dat_list <- list(
  K = 7,
  z = y_gb,
  T = length(y_gb),
  theta_prior = theta_prior
)

m2_single <- stan(file = "./stan/second-order-markov-single.stan",
 data = dat_list, iter = n_iter, chains = n_chains)

model2_single_post <- extract.samples(m2_single)

# now fit the multi-chain model

d <- draws[grep("gameboy", draws$game_version), ]
run_ids <- unique(d$run_id)
N <- length(run_ids)
T <- rep(NA, N)
for (i in 1:N) T[i] <- sum(d$run_id == run_ids[i])
S <- c(0, cumsum(T)[-N])

dat_list <- list(
  K = 7,
  z = y_gb,
  N = N,
  T = T,
  S = S,
  Q = length(y_gb),
  theta_prior = theta_prior
)

m2 <- stan(file = "./stan/second-order-markov.stan",
 data = dat_list, iter = n_iter, chains = n_chains)

model2_post <- extract.samples(m2)



print("make graphs")

# plot drought distributions

png("./temp/drought_lengths.png", res = 300, units = "in", height = 5, width = 6)
plot(1, 1, ylim = c(1e-4, 1), xlim = c(0, 60), las = 1,
  col = nes_color, type = "n", yaxt = "n", ylab = "droughts > t",
  xlab = "t, time since last I-piece", log = "y")
abline(h = c(1:10/10, 1:10/100, 1:10/1000, 1:10/10000), col = col.alpha("gray", 0.3))
abline(h = c(1, 0.1, 0.01, 0.001, 0.0001, 0.00001), col = col.alpha("gray", 0.9))
abline(v = c(10, 20, 30, 40, 50, 60, 70), col = col.alpha("gray", 0.9))
points(as.numeric(names(drought_ccdf(x_nes_sim))), drought_ccdf(x_nes_sim),
  col = nes_color, type = "l", lwd = 1.2)
points(as.numeric(names(drought_ccdf(x_gb_sim))), drought_ccdf(x_gb_sim),
  col = gb_color, type = "l", lwd = 1.2)
points(as.numeric(names(drought_ccdf(x_modern_sim))), drought_ccdf(x_modern_sim),
  col = modern_color, type = "l", lwd = 1.2)
curve((6/7)^(x+1), add = TRUE, lty = 2, col = col_alpha(unif_color, 0.9))
axis(2, at = c(1:10/10, 1:10/100, 1:10/1000, 1:10/10000), labels = FALSE)
axis(2, at = c(1, 0.1, 0.01, 0.001, 0.0001, 0.00001),
  labels = c("100%", "10%", "1%", "0.1%", "0.01%", "0.001%"),
  las = TRUE)
text(34, 0.0015, "NES", col = nes_color)
text(34, 0.015, "Gameboy", col = gb_color)
text(27, 0.035, "Uniform", col = unif_color)
text(7, 0.036, "7-Bag", col = modern_color)
dev.off()

################

png("./temp/randomizer_counts.png", res = 300, units = "in", height = 4, width = 8)
par(mfrow = c(1, 2))
barplot(table(x_gb_sim), border = NA, col = "dodgerblue", main = "Gameboy",
    yaxt = "n", ylim = c(0, 15e3), ylab = "piece counts")
abline(h = 1e5/7, lty = 2)
axis(2, at = c(0, 5e3, 10e3, 15e3), labels = c("0", "500", "1K", "15K"))
barplot(table(x_nes_sim), border = NA, col = "dodgerblue", main = "NES",
    yaxt = "n", ylim = c(0, 15e3))
abline(h = 1e5/7, lty = 2)
axis(2, at = c(0, 5e3, 10e3, 15e3), labels = c("0", "500", "1K", "15K"))
dev.off()

############

print("graph model0 gameboy simulated vs observed")

png("./temp/model0_estimates_vs_sim_gameboy.png",
  height = 5, width = 5, res = 300, units = "in")

post <- model0_gb_post

frq_means <- apply(post$theta, 2, mean)
frq_lb_89 <- apply(post$theta, 2, function(z) HPDI(z, 0.89))[1,]
frq_ub_89 <- apply(post$theta, 2, function(z) HPDI(z, 0.89))[2,]

y_sim <- match(x_gb_sim, tetrominoes)
sim_frq <- as.vector(prop.table(table(y_sim)))

xlims <- c(0.1, 0.2)
ylims <- c(0.1, 0.2)

set.seed(1)
x_off <- rnorm(7, 0, 0.0005)
plot(1, 1, frame.plot = FALSE, type = "n", las = 1,
  xlim = xlims, ylim = ylims, xlab = "predicted frequency",
  ylab = "estimated frequency")
abline(0, 1, lty = 2, col = "gray")
abline(h = 1/7, lty = 2, col = "gray")
points(sim_frq + x_off, frq_means, pch = 20, col = "dodgerblue")
pos_vec <- c(2, 4, 4, 2, 4, 4, 2)
for (i in 1:7) {
  lines(c(sim_frq[i], sim_frq[i]) + x_off[i],
    c(frq_lb_89[i], frq_ub_89[i]), col = col_alpha("dodgerblue", 0.4), lwd = 1)
  text(sim_frq[i] + x_off[i], frq_means[i], labels = tetrominoes[i], pos = pos_vec[i])
}
dev.off()

############

print("graph model0 nes simulated vs observed")

png("./temp/model0_estimates_vs_sim_nes.png",
  height = 5, width = 5, res = 300, units = "in")

post <- model0_nes_post

frq_means <- apply(post$theta, 2, mean)
frq_lb_89 <- apply(post$theta, 2, function(z) HPDI(z, 0.89))[1,]
frq_ub_89 <- apply(post$theta, 2, function(z) HPDI(z, 0.89))[2,]

y_sim <- match(x_nes_sim, tetrominoes)
sim_frq <- as.vector(prop.table(table(y_sim)))

xlims <- c(0.1, 0.2)
ylims <- c(0.1, 0.2)

set.seed(1)
x_off <- rnorm(7, 0, 0.0005)
plot(1, 1, frame.plot = FALSE, type = "n", las = 1,
  xlim = xlims, ylim = ylims, xlab = "predicted frequency",
  ylab = "estimated frequency"
)
abline(0, 1, lty = 2, col = "gray")
abline(h = 1/7, lty = 2, col = "gray")
points(sim_frq + x_off, frq_means, pch = 20, col = "dodgerblue")
pos_vec <- c(3, 4, 4, 2, 4, 4, 2)
for (i in 1:7) {
  lines(c(sim_frq[i], sim_frq[i]) + x_off[i],
    c(frq_lb_89[i], frq_ub_89[i]), col = col_alpha("dodgerblue", 0.4), lwd = 1)
  text(sim_frq[i] + x_off[i], frq_means[i], labels = tetrominoes[i], pos = pos_vec[i])
}
dev.off()

############

print("graph model1 simulated vs observed")

png("./temp/model1_estimates_vs_sim_nes.png",
  height = 5, width = 5, res = 300, units = "in")

post <- model1_post

theta_mu <- matrix(NA, 7, 7)
theta_sd <- matrix(NA, 7, 7)
theta_lb <- matrix(NA, 7, 7)
theta_ub <- matrix(NA, 7, 7)
for (i in 1:7) {
  theta_mu[i, ] <- apply(post$theta[, i, ], 2, mean)
  theta_sd[i, ] <- apply(post$theta[, i, ], 2, sd)
  theta_lb[i, ] <- apply(post$theta[, i, ], 2, HPDI)[1,]
  theta_ub[i, ] <- apply(post$theta[, i, ], 2, HPDI)[2,]
}

current_state <- y_sim[1:length(y_sim)-1]
next_state <- y_sim[2:length(y_sim)]
sim_frq <- t(apply(table(current_state, next_state), 1, simplex))

set.seed(1)
x_off <- rnorm(7, 0, 0.00015)
plot(1, 1, frame.plot = FALSE, type = "n", las = 1,
  xlab = "predicted frequency",
  ylab = "estimated frequency",
  xlim = c(0, 0.3), ylim = c(0, 0.3)
)
abline(0, 1, lty = 2, col = "gray")
abline(h = 1/7, lty = 2, col = "gray")
for (i in 1:7) {
  for (j in 1:7) {
    lines(c(sim_frq[i, j], sim_frq[i, j]),
      c(theta_lb[i, j], theta_ub[i, j]),
      col = col_alpha("dodgerblue", 0.3))
  }
}
for (i in 1:7) {
  for (j in 1:7) {
    points(sim_frq[i, j], theta_mu[i, j], pch = 20,
      col = col_alpha("dodgerblue", 0.8))
  }
}
dev.off()

############

print("graph model2 simulated vs observed")

png("./temp/model2_estimates_vs_sim_gameboy.png",
  height = 5, width = 5, res = 300, units = "in")

post <- model2_post

theta_mu <- list()
theta_sd <- list()
theta_lb <- list()
theta_ub <- list()
theta_p <- list()

for (i in 1:7) {
  transitions <- post$theta[ , i, , ]
  transition_mu <- matrix(NA, 7, 7)
  transition_sd <- matrix(NA, 7, 7)
  transition_lb <- matrix(NA, 7, 7)
  transition_ub <- matrix(NA, 7, 7)
  for (j in 1:7) {
    transition_mu[j,] <- apply(transitions[ , j, ], 2, mean)
    transition_lb[j,] <- apply(transitions[ , j, ], 2, HPDI)[1, ]
    transition_ub[j,] <- apply(transitions[ , j, ], 2, HPDI)[2, ]
    transition_sd[j,] <- apply(transitions[ , j, ], 2, sd)
  }
  theta_mu[[i]] <- transition_mu
  theta_sd[[i]] <- transition_sd
  theta_ub[[i]] <- transition_ub
  theta_lb[[i]] <- transition_lb
}

last_piece <- y_sim[1:(length(y_sim) - 2)]
current_piece <- y_sim[2:(length(y_sim) - 1)]
next_piece <- y_sim[3:length(y_sim)]

sim_frq <- list()
for (i in 1:7) {
  tar <- which(last_piece == i)
  sim_frq[[i]] <- t(apply(table(current_piece[tar], next_piece[tar]), 1, simplex))
}

x_dat <- numeric()
y_dat <- numeric()

for (i in 1:7) {
  x_dat <- c(x_dat, as.vector(sim_frq[[i]]))
  y_dat <- c(y_dat, as.vector(theta_mu[[i]]))
}

plot(1, 1, frame.plot = FALSE, type = "n", las = 1,
  xlab = "predicted frequency",
  ylab = "estimated frequency",
  xlim = c(0, 0.6), ylim = c(0, 0.6)
)
abline(0, 1, lty = 2, col = "gray")
abline(h = 1/7, lty = 2, col = "gray")

points(x_dat, y_dat, pch = 20, col = col_alpha("dodgerblue", 0.4))

dev.off()


######## beta-binomial model of gamma ########

prep <- prep_gb_analysis(x_gb)
tar <- which(prep############
$ost_bias == 1)
n_chose_favored <- sum(prep$chose_favored[tar]) # 156
n_total <- length(tar) # 188
# the ratio is 0.830, so gamma = 0.401

post_p_mu <- (n_chose_favored + 3) / (n_total + 7)
gamma_mu <- post_p_mu - 3/7
gamma_sd <- sqrt(post_p_mu * (1 - post_p_mu) / (n_total + 7 + 1))

prep <- prep_gb_analysis(x_gb_sim)
tar <- which(prep$ost_bias == 1)
n_chose_favored <- sum(prep$chose_favored[tar])
n_total <- length(tar)
# the ratio is 0.813 so gamma = 0.384

prep <- prep_gb_analysis(x_nes)
tar <- which(prep$ost_bias == 1)
n_chose_favored <- sum(prep$chose_favored[tar]) # 61
n_total <- length(tar) # 131

post_p_mu <- (n_chose_favored + 3) / (n_total + 7)
gamma_mu <- post_p_mu - 3/7
gamma_sd <- sqrt(post_p_mu * (1 - post_p_mu) / (n_total + 7 + 1))
# the ratio is 0.466, so gamma = 0.037

prep <- prep_gb_analysis(x_nes_sim)
tar <- which(prep$ost_bias == 1)
n_chose_favored <- sum(prep$chose_favored[tar])
n_total <- length(tar)
# the ratio is 0.491, so gamma = 0.062

######## cumulative signals plot ########

png("temp/cumulative_signals.png", res = 300,
  units = "in", height = 5, width = 5)

plot(1, 1, type = "n", xlim = c(0, 75), ylim = c(0, 75),
  xlab = "n, num. visits to 12 special states", ylab = "x, num. draws of favorite outcomes")

gamma <- 0.384 # gameboy predicted amount
p <- 3/7 + gamma
add_interval_polygon(p, col_alpha(gb_color, 0.1))

text(61, 47, labels = "Gameboy", col = gb_color,
  srt = 35)

gamma <- 0.037 # nes predicted amount
p <- 3/7 + gamma
add_interval_polygon(p, col_alpha(nes_color, 0.1))

text(64, 33.5, labels = "NES", col = nes_color, srt = 22)

gamma <- 0 # uniform baseline
p <- 3/7 + gamma
abline(0, p, col = unif_color, lty = 2) # uniform baseline

text(69, 27, labels = "Uniform", col = unif_color, srt = 20)

# add gameboy runs
d <- draws[grep("gameboy", draws$game_version),]
runs <- sort(unique(d$run_id))
for (i in seq_along(runs)) {
  tar <- which(d$run_id == runs[i])
  x <- d$letter[tar]
  prep <- prep_gb_analysis(x)
  tar <- which(prep$ost_bias == 1)
  n_chose_favored_cumu <- cumsum(prep$chose_favored[tar])
  n_total_cum <- 1:length(tar)
  points(n_total_cum, n_chose_favored_cumu, type = "l",
    col = col_alpha(gb_color, 0.7), lwd = 2)
}

# add nes runs
d <- draws[grep("nes", draws$game_version),]
runs <- sort(unique(d$run_id))
for (i in seq_along(runs)) {
  tar <- which(d$run_id == runs[i])
  x <- d$letter[tar]
  prep <- prep_gb_analysis(x)
  tar <- which(prep$ost_bias == 1)
  n_chose_favored_cumu <- cumsum(prep$chose_favored[tar])
  n_total_cum <- 1:length(tar)
  points(n_total_cum, n_chose_favored_cumu, type = "l",
    col = col_alpha(nes_color, 0.7), lwd = 2)
}

dev.off()



#########

dir_init("./output")

files <- list.files("./temp", full.names = TRUE)
file.copy(files, "./output")

unlink("./temp", recursive = TRUE)
