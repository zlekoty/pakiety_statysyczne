library(dplyr)
require(tigerstats)
library(corrplot)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)



df <- read.csv("C:\\Users\\zleko\\Desktop\\WorldHappiness_Corruption_2015_2020.csv")
df$family <- df$family + df$social_support  
df <- subset(df, select = -c(social_support,dystopia_residual))
df[df==0] <- NA
#View(df)
colnames(df) <- c("Pañstwo", "Szczêœcie", "PKB na osobê", "Rodzina", "Zdrowie", "Wolnoœæ", "Szczodroœæ", "Zaufanie do rz¹du","Kontynent","Rok", "Korupcja")
#View(df)


###########################
q <- aggregate(Szczêœcie ~ Rok + Kontynent, data = df, FUN = mean)
ggplot(q, aes(x=Rok, y=Szczêœcie, shape= Kontynent)) +
  geom_point() +
  stat_summary(fun = "mean", colour = "red", size = 5, geom = "point",shape = "o")+ 
  theme(legend.position = "top")
###############################


#Uœredniony df
df2 <- aggregate(df, list(df$Pañstwo), mean, na.rm = TRUE)
df2 <- select(df2, -Pañstwo, -Kontynent,-Rok)
names(df2)[names(df2) == 'Group.1'] <- 'Pañstwo'
df2 <- merge(df2, select(df, "Pañstwo", "Kontynent"))
df2 <- df2[!duplicated(df2),]
View(df2)

ggplot(df2, aes(y = Szczêœcie, x = reorder(Pañstwo,Szczêœcie),fill=Kontynent))+
  geom_bar(stat="identity", width=0.5)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size = 3))+ 
  theme(legend.position = "top") +xlab("")

#View(df2)
#corr wszystkich
res <- cor(df2[,c(2:9)], use = "complete.obs" )
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#corr wszystkie z szcesciem pearson
corr_pearson <- cor(df2[,c(3:9)], df2$Szczêœcie, use = "complete.obs")
corr_df_pearson <- data.frame(labels(corr_pearson),corr_pearson)
colnames(corr_df_pearson) <- c("status","corr_method","corr")
corr_df_pearson$corr_method <- "pearson"
#ggplot(corr_df_pearson, aes(x=reorder(status,corr_pearson),y=corr_pearson)) +
 # geom_bar(stat="identity", width=0.5)

#corr kendall
corr_kendall <- cor(df2[,c(3:9)], df2$Szczêœcie, use = "complete.obs",method = "kendall")
corr_df_kendall <- data.frame(labels(corr_kendall),corr_kendall)
colnames(corr_df_kendall) <- c("status","corr_method","corr")
corr_df_kendall$corr_method <- "kendall"
#ggplot(corr_df_kendall, aes(x=reorder(status,corr_kendall),y=corr_kendall)) +
 # geom_bar(stat="identity", width=0.5)

#corr spearman
corr_spearman <- cor(df2[,c(3:9)], df2$Szczêœcie, use = "complete.obs",method = "spearman")
corr_df_spearman <- data.frame(labels(corr_spearman),corr_spearman)
colnames(corr_df_spearman) <- c("status","corr_method","corr")
corr_df_spearman$corr_method <- "spearman"
#ggplot(corr_df_spearman, aes(x=reorder(status,corr_spearman),y=corr_spearman)) +
 # geom_bar(stat="identity", width=0.5)

# 3 CORR W 1
corr_df_all <- rbind(corr_df_pearson,corr_df_kendall,corr_df_spearman)
ggplot(corr_df_all, aes(x=reorder(status,corr),y=corr,fill=corr_method)) +
  geom_bar(position="dodge",stat="identity", width=0.5)+
  theme(axis.text.x=element_text(angle=35,hjust=1,vjust=1)) + xlab("") +ylab("Korelacja") + 
  scale_fill_discrete(name = "Metoda korelacji") +
  theme(legend.position = "top")


#œwiat bajer
world <- ne_countries(scale = "medium", returnclass = "sf")
world1 <- left_join(world, df2, by = c("name"="Pañstwo"))
ggplot(data = world1) +
  geom_sf(aes(fill = Szczêœcie)) +
  scale_fill_viridis_c(option = "plasma")+ 
  theme(legend.position = "top")

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
plot_grid(plotlist = p)
plot_grid(plotlist = h)
#
ggplot(df2, aes(x = `Zaufanie do rz¹du`, y = Korupcja))+ 
  geom_point()+
  geom_smooth()
cor(df2$`Zaufanie do rz¹du`,df2$Korupcja)