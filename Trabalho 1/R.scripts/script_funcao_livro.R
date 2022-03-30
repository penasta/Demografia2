intrinsicr <- function(alpha,beta,L,l0,f,RS0) {
  # Calcula a taxa intrinseca de crescimento r a partir de nfa e nLa para vetores L e f de tamanhos arbitrários
  N <- length(L)
  age <- alpha + (beta-alpha)*((1:N)-0.5)/N
  TLR <- sum(L*f/(l0*(1+RS0))); r = log(TLR)/27
  for (i in (1:8)) {
    result <- sum(exp(-r*age)*L*f/(l0*(1+RS0))); r <- r + (result-1)/27
  }
  r
}

# Com parâmetros: 
# alpha = idade inicial
# beta = idade final
# L = nLx - Tempo a ser vivido pelos sobreviventes da coorte na idade x, entre esta idade e o início do próximo grupo etário. É o n de pessoas-ano entre as idades x e x+n
# l0 = no exemplo, foi colocado o valor 100000 - Atribuir um valor arbitrário para l0 (em geral l0 = 1000 ou 100000).
# f = Número médio de nascimentos das mulheres em cada grupo de idade do período reprodutivo.
# RS0 = Razão de Sexos ao Nascer (colocar algo entre 100-105)

# OBSERVAÇÃO: Os vetores L e f devem ter o mesmo número de entradas