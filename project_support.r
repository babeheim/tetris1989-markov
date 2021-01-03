
library(rethinking)
library(tetristools)

modern_color <- "#1a9641"
nes_color <- "#ca0020"
gb_color <- "#0571b0"
unif_color <- "black"

tetrominoes <- c("O", "S", "T", "I", "J", "Z", "L")

simplex <- function(x) x/sum(x)

add_interval_polygon <- function(p, col = NA) {
  maxn <- 100
  lb <- rep(NA, maxn)
  ub <- rep(NA, maxn)
  for (n in 1:maxn) {
    lb[n] <- n * p - 2 * sqrt(p * (1 - p) * n)
    ub[n] <- n * p + 2 * sqrt(p * (1 - p) * n)
  }
  polygon(c(1:maxn, rev(1:maxn)), c(lb, rev(ub)),
    border = NA, col = col)
}

drought_ccdf <- function(x) {
  hits <- which(x == "I")
  drought_lengths <- diff(hits) - 1
  drought_pmf <- prop.table(table(drought_lengths)) # pr(X = x)
  drought_cdf <- cumsum(drought_pmf) # pr(X <= x)
  out <- 1 - cumsum(drought_pmf) # pr(X > x)
  return(out)
}

prep_gb_analysis <- function(draws) {

  # this is calculating everything w.r.t. the gameboy transitions

  tetrominoes <- c("L", "J", "I", "O", "Z", "S", "T")

  x <- draws

  # define second-order states
  state_list <- paste(rep(tetrominoes, each = 7), rep(tetrominoes, times = 7), sep = "")
  current_state <- paste0(x[1:(length(x) - 2)], x[2:(length(x) - 1)])
  next_piece <- c(x[3:length(x)])

  chose_favored <- as.logical(rbinom(length(next_piece), 1, 1/7))

  o_bias <- current_state %in% c("OO", "OI", "OJ", "OL")
  s_bias <- current_state %in% c("SS", "SJ", "SZ", "SL")
  t_bias <- current_state %in% c("TT", "TI", "TZ", "TL")
  i_bias <- current_state %in% c("IL", "II")
  j_bias <- current_state %in% c("JL", "JJ")
  z_bias <- current_state %in% c("ZL", "ZZ")
  l_bias <- current_state == "LL"

  for (i in seq_along(chose_favored)) {
    if (o_bias[i]) chose_favored[i] <- next_piece[i] %in% c("Z", "T", "S")
    if (s_bias[i]) chose_favored[i] <- next_piece[i] %in% c("I", "T", "O")
    if (t_bias[i]) chose_favored[i] <- next_piece[i] %in% c("J", "S", "O")
    if (i_bias[i]) chose_favored[i] <- !next_piece[i] %in% c("I", "L")
    if (j_bias[i]) chose_favored[i] <- !next_piece[i] %in% c("J", "L")
    if (z_bias[i]) chose_favored[i] <- !next_piece[i] %in% c("Z", "L")
    if (l_bias[i]) chose_favored[i] <- !next_piece[i] %in% c("L")
  }

  ost_bias <- o_bias | s_bias | t_bias
  ijz_bias <- i_bias | j_bias | z_bias

  out <- data.frame(
    current_state,
    next_piece,
    ost_bias = as.numeric(ost_bias),
    ijz_bias = as.numeric(ijz_bias),
    l_bias = as.numeric(l_bias),
    chose_favored = as.numeric(chose_favored)
  )
  return(out)
  
}

col_alpha <- function (acol, alpha = 0.2){
    acol <- col2rgb(acol)
    acol.red <- acol["red",]/255
    acol.green <- acol["green",]/255
    acol.blue <- acol["blue",]/255
    acol <- mapply(function(red, green, blue, alphas) rgb(red, green, blue, alphas), acol.red, acol.green, acol.blue, alpha)
    return(as.character(acol))
}

dir_init <- function(path, verbose=FALSE, overwrite=TRUE){
  if(substr(path, 1, 2)!='./') stop('path argument must be formatted
    with "./" at beginning')
  contents <- dir(path, recursive=TRUE)
  if(dir.exists(path)){
    if(overwrite){
      if(verbose){
        if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
        if(length(contents)>0) print(paste('folder ', path, ' wiped of ', length(contents), ' files/folders.', sep=""))
      }
      if(dir.exists(path)) unlink(path, recursive=TRUE)
      dir.create(path)
    }
  } else {
    if(verbose){
      print(paste('folder ', path, ' created.', sep=""))
    }
    dir.create(path)
  }
}
