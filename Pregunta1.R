#apartat a (SI NO EM DIUEN QUE LA DISTRIBUCIO ES CONTINUA I LA N ES MES PETITA QUE 30, RESPONDRE NA)

curve(dnorm(x, 95.3, 5.7), xlim=c(80,120), col="red") #distribucion poblacional



set.seed(84) #per calcular en paeticular a 84, a tots ens surt el mateix numero
n <- 4
simul <- rnorm(n, 95.3, 5.7)
sum(simul)

#suma muestral

set.seed(84)
Y <- function(i){sum(rnorm(n, 95.3, 5.7))}
simul100000 <- sapply(1:100000,Y)
simul100000
hist(simul100000)
mean(simul100000)

4*95.3

#apartat b
#varianza suma muestral
#teoria V(Y) = n*sigma^2
n <- 100
sigma <- 5.7

100*5.7^2

#apartat c  

#P(X>103)
# 1-pnorm(103, mu, sigma)
mu <- 95.3
sigma <- 5.7
1-pnorm(103, 95.3, 5.7)

#apartat d
#Probabilitat que la mitjana de la resistencia sigui menor que 98kg
#P(X>98)
pnorm(98, 95.3, 5.7/sqrt(4))

#apartat e
#teoria P(SSq>32)=P(W>32*(n-1)/sigma^2)
# W es una variable aleatoria chi cuadrado
1 - pchisq(32*(100-1)/5.7^2, 100-1)
