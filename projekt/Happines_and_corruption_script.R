
library(corrplot)
library(cowplot)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)


df <- read.csv("Data\\WorldHappiness_Corruption_2015_2020.csv")
df$family <- df$family + df$social_support  
df <- subset(df, select = -c(social_support))
apply(df == 0, 2, which)
df[df==0] <- NA
#View(df)
colnames(df) <- c("Pañstwo", "Szczêœcie", "PKB na osobê", "Rodzina", "Zdrowie", "Wolnoœæ", "Szczodroœæ", "Zaufanie do rz¹du","Dystopia", "Kontynent","Rok", "Korupcja")
#View(df)


q <- aggregate(Szczêœcie ~ Rok + Kontynent, data = df, FUN = mean)
r <- ggplot(q, aes(x=Rok, y=Szczêœcie, shape= Kontynent)) +
  geom_point() +
  stat_summary(fun = "mean", colour = "red", size = 5, geom = "point",shape = "o") +
  theme(legend.position="top")

q_mean <- aggregate(cbind(Szczêœcie,`PKB na osobê`,Rodzina, Zdrowie, Wolnoœæ, Szczodroœæ, `Zaufanie do rz¹du`, Dystopia, Korupcja) ~ Kontynent, data = df, FUN = mean)

#Uœredniony df
df2 <- aggregate(df, list(df$Pañstwo), mean, na.rm = TRUE)
df2 <- select(df2, -Pañstwo, -Kontynent,-Rok)
names(df2)[names(df2) == 'Group.1'] <- 'Pañstwo'
df2 <- merge(df2, select(df, "Pañstwo", "Kontynent"))
df2 <- df2[!duplicated(df2),]

s <- ggplot(df2, aes(y = Szczêœcie, x = reorder(Pañstwo,Szczêœcie),fill=Kontynent))+
  geom_bar(stat="identity", width=0.5)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size = 3))+
  theme(legend.position="top") + xlab("")

#View(df2)
#corr pêtla
corr_df_all <- data.frame()
metody <- c("pearson","spearman")
for(m in metody){
  corr <- cor(df2[,c(3:10)], df2$Szczêœcie, use = "complete.obs",method = m)
  corr_df <- data.frame(labels(corr),corr)
  colnames(corr_df) <- c("status","corr_method","corr")
  corr_df$corr_method <- m
  corr_df_all <- rbind(corr_df_all,corr_df)
}
t <- ggplot(corr_df_all, aes(x=reorder(status,corr),y=corr,fill=corr_method)) +
  geom_bar(position="dodge",stat="identity", width=0.5)+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))+
  theme(legend.position="top") + xlab("") + scale_fill_discrete(name = "Metoda korelacji") +ylab("korelacja")


#œwiat bajer
world <- ne_countries(scale = "medium", returnclass = "sf")
world1 <- left_join(world, df2, by = c("name"="Pañstwo"))
u <- ggplot(data = world1) +
  geom_sf(aes(fill = Szczêœcie)) +
  scale_fill_viridis_c(option = "plasma")+
  theme(legend.position="top")

#macierz korelacji z szczesciem w plotrach +hist
p <- list()
h <- list()
index <- 1
for(k in colnames(df2)){
  if(k != "Pañstwo" & k!= "Kontynent" & k != "Szczêœcie"){
    p[[index]] <-  ggplot(df2, aes(x = .data[[k]], y = Szczêœcie))+ 
      geom_point()+
      geom_smooth(method=lm)
    h[[index]] <- ggplot(df2, aes(x = .data[[k]]))+
      geom_histogram(bins = 10)
    index <- index + 1
  }
}
w <- plot_grid(plotlist = p)
v <- plot_grid(plotlist = h)
x <- ggplot(df2, aes(x = Korupcja, y = `Zaufanie do rz¹du`))+ 
  geom_point()
#cor(df2$`Zaufanie do rz¹du`,df2$Korupcja)