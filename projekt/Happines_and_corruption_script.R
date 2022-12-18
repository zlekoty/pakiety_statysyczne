
library(corrplot)
library(cowplot)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(rnaturalearth)
library(rnaturalearthdata)
require(tigerstats)


df <- read.csv("Data\\WorldHappiness_Corruption_2015_2020.csv")
df$family <- df$family + df$social_support  
df <- subset(df, select = -c(social_support,dystopia_residual))
df[df==0] <- NA
#View(df)
colnames(df) <- c("Pa�stwo", "Szcz�cie", "PKB na jednego mieszka�ca", "Rodzina", "Zdrowie", "Wolno��", "Szczodro��", "Zaufanie do rz�du","Kontynent","Rok", "Korupcja")
#View(df)


###########################
q <- aggregate(Szcz�cie ~ Rok + Kontynent, data = df, FUN = mean)
ggplot(q, aes(x=Rok, y=Szcz�cie, shape= Kontynent)) +
  geom_point() +
  stat_summary(fun = "mean", colour = "red", size = 5, geom = "point",shape = "o")
###############################


#U�redniony df
df2 <- aggregate(df, list(df$Pa�stwo), mean, na.rm = TRUE)
df2 <- select(df2, -Pa�stwo, -Kontynent,-Rok)
names(df2)[names(df2) == 'Group.1'] <- 'Pa�stwo'
df2 <- merge(df2, select(df, "Pa�stwo", "Kontynent"))
df2 <- df2[!duplicated(df2),]

s <- ggplot(df2, aes(y = Szcz�cie, x = reorder(Pa�stwo,Szcz�cie),fill=Kontynent))+
  geom_bar(stat="identity", width=0.5)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size = 3))
s
#View(df2)
#corr wszystkich
res <- cor(df2[,c(2:9)], use = "complete.obs" )
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#corr wszystkie z szcesciem pearson
corr_pearson <- cor(df2[,c(3:9)], df2$Szcz�cie, use = "complete.obs")
corr_df_pearson <- data.frame(labels(corr_pearson),corr_pearson)
colnames(corr_df_pearson) <- c("status","corr_method","corr")
corr_df_pearson$corr_method <- "pearson"
ggplot(corr_df_pearson, aes(x=reorder(status,corr_pearson),y=corr_pearson)) +
  geom_bar(stat="identity", width=0.5)

#corr kendall
corr_kendall <- cor(df2[,c(3:9)], df2$Szcz�cie, use = "complete.obs",method = "kendall")
corr_df_kendall <- data.frame(labels(corr_kendall),corr_kendall)
colnames(corr_df_kendall) <- c("status","corr_method","corr")
corr_df_kendall$corr_method <- "kendall"
ggplot(corr_df_kendall, aes(x=reorder(status,corr_kendall),y=corr_kendall)) +
  geom_bar(stat="identity", width=0.5)

#corr spearman
corr_spearman <- cor(df2[,c(3:9)], df2$Szcz�cie, use = "complete.obs",method = "spearman")
corr_df_spearman <- data.frame(labels(corr_spearman),corr_spearman)
colnames(corr_df_spearman) <- c("status","corr_method","corr")
corr_df_spearman$corr_method <- "spearman"
ggplot(corr_df_spearman, aes(x=reorder(status,corr_spearman),y=corr_spearman)) +
  geom_bar(stat="identity", width=0.5)

# 3 CORR W 1
corr_df_all <- rbind(corr_df_pearson,corr_df_kendall,corr_df_spearman)
t <- ggplot(corr_df_all, aes(x=reorder(status,corr),y=corr,fill=corr_method)) +
  geom_bar(position="dodge",stat="identity", width=0.5)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


#�wiat bajer
world <- ne_countries(scale = "medium", returnclass = "sf")
world1 <- left_join(world, df2, by = c("name"="Pa�stwo"))
u <- ggplot(data = world1) +
  geom_sf(aes(fill = Szcz�cie)) +
  scale_fill_viridis_c(option = "plasma")

#macierz korelacji z szczesciem w plotrach +hist
p <- list()
h <- list()
index <- 1
for(k in colnames(df2)){
  if(k != "Pa�stwo" & k!= "Kontynent" & k != "Szcz�cie"){
    p[[index]] <-  ggplot(df2, aes(x = .data[[k]], y = Szcz�cie))+ 
      geom_point()+
      geom_smooth(method=lm)
    h[[index]] <- ggplot(df2, aes(x = .data[[k]]))+
      geom_histogram(bins = 10)
    index <- index + 1
  }
}
w <- plot_grid(plotlist = p)
v <- plot_grid(plotlist = h)
#
ggplot(df2, aes(x = `Zaufanie do rz�du`, y = Korupcja))+ 
  geom_point()
cor(df2$`Zaufanie do rz�du`,df2$Korupcja)