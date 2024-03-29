library(gamlss.dist)
library(tseries)
library(nortest)
library(dplyr)
library(ggplot2)

setwd("C:/Users/zleko/Desktop/PAKIETY2/")
#zadanie 1

taus <- seq(0.5,2,0.1)
X <- rSHASHo(100, mu = -1, sigma = 3, nu = 0, tau = 10)
a <- shapiro.test(X)
a
moc <- function(taus,n){
  len <- length(taus)
  test_shapiro <- c(1:len)
  test_lillie <- c(1:len)
  test_jb <- c(1:len)
  for(j in 1:len){
    s <- c(1:n)
    l <- c(1:n)
    jb <- c(1:n)
    for(i in 1:n){
      X <- rSHASHo(100, mu = -1, sigma = 3, nu = 0, tau = taus[j]) #generowanie z rozk�adu
      s[i] <- shapiro.test(X)$p.value
      l[i] <- lillie.test(X)$p.value
      jb[i] <- jarque.bera.test(X)$p.value
    }
# H0: Pr�ba pochodzi z rozk�adu normalnego - jest fa�szywa
    s<- ifelse(s<0.05, 0, 1) # je�li p warto�� jest mniejsza od 5% odrzucamy hipotez� zerow� [0]
    l<- ifelse(l<0.05, 0, 1)
    jb<- ifelse(jb<0.05, 0, 1)
# B��d drugiego rodzaju - b��d polegaj�cy na nieodrzuceniu hipotezy zerowej (musimy zliczy� ile razy nie odrzucili�my)
# Moc = 1 - b�2
    test_shapiro[j] <- 1 - sum(s)/n
    test_lillie[j] <- 1 - sum(l)/n
    test_jb[j] <- 1 - sum(jb)/n
  }
  list(shapiro = c(test_shapiro), lillie = c(test_lillie),jb = c(test_jb))
}
sh <- moc(taus,1000)
plot(taus,sh$shapiro)
points(taus,sh$lillie,col = "red")   
points(taus,sh$jb, col = "green")

# transformacja sko�no�ci
x <- seq(-6, 6, length.out = 301)
nu_list <- -3:3
df <- data.frame()
for (nu in nu_list) {
  temp_df <- data.frame(x = x, 
                        y = dSHASHo2(x, mu = 0, sigma = 1, nu = nu, tau = 1))
  temp_df$nu <- nu
  df <- rbind(df, temp_df)
}
bb <-df %>% filter(nu >= 0) %>%
ggplot(aes(x = x, y = y, col = factor(nu))) +
  geom_line(aes(linetype=factor(nu))) +theme_bw(base_size = 20) +
  labs(col = expression(nu), linetype = expression(nu), y= "G�sto��", x = "X") +
  theme(legend.position="top")
ggsave("skos.pdf")
df %>% filter(nu <= 0) %>%
  ggplot(aes(x = x, y = y, col = factor(nu))) +
  geom_line(aes(linetype=factor(nu))) +theme_bw()

# transformacja ogon�w
tau_list <- c(0.25, 0.75, 1, 1.5)
df <- data.frame()
for (tau in tau_list) {
  temp_df <- data.frame(x = x, 
                        y = dSHASHo(x, mu = 0, sigma = 1, nu = 0, tau = tau))
  temp_df$tau <- tau
  df <- rbind(df, temp_df)
}
aa<-ggplot(data = df, aes(x = x, y = y, col = factor(tau))) +
  geom_line(aes(linetype=factor(tau))) +theme_bw(base_size = 20) +labs(col = expression(tau), linetype = expression(tau),
                                y= "G�sto��", x = "X") +
  theme(legend.position="top")

aa

pdf("kurt.pdf")
print(aa)
dev.off()




pdf("skos.pdf")
print(bb)
dev.off()



