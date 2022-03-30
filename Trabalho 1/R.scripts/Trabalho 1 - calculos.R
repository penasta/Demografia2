######################## Pacotes

if (!require(pacman)) {
  install.packages('pacman')
  library(pacman)
}

pacman::p_load(ggplot2,intRinsic,ipumsr)

################################################################################
# Funções

intrinsicr <- function(alpha,beta,L,l0,f,RS0) {
  # Calcula a taxa intrinseca de crescimento r a partir de nfa e nLa para vetores L e f de tamanhos arbitrÃ¡rios
  N <- length(L)
  age <- alpha + (beta-alpha)*((1:N)-0.5)/N
  TLR <- sum(L*f/(l0*(1+RS0))); r = log(TLR)/27
  for (i in (1:8)) {
    result <- sum(exp(-r*age)*L*f/(l0*(1+RS0))); r <- r + (result-1)/27
  }
  r
}

################################################################################
# Estados

l_pe <- c(487035,483464,479151, 474496,469110,462286,452925)
f_pe <- c(0.0717,	0.1037,	0.0868,	0.0578,	0.0292,	0.0085,	0.0008)

f_pa <- c(0.0992,	0.1364,	0.0953,	0.0559,	0.0284,	0.0102,	0.0018)
l_pa <- c(483833,478553,472163,465432,458115,449672,438511)

r_pe <- intrinsicr(15,50,l_pe,100000,f_pe,.92); r_pe
r_pa <- intrinsicr(15,50,l_pa,100000,f_pa,1.01); r_pa


