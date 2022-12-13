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
View(df)
colnames(df) <- c("Pañstwo", "Szczêœcie", "PKB na jednego mieszkañca", "Rodzina", "Zdrowie", "Wolnoœæ", "Szczodroœæ", "Zaufanie do rz¹du","Kontynent","Rok", "Korupcja")
View(df)


###########################
q <- aggregate(Szczêœcie ~ Rok + Kontynent, data = df, FUN = mean)
ggplot(q, aes(x=Rok, y=Szczêœcie, shape= Kontynent)) +
  geom_point() +
  stat_summary(fun = "mean", colour = "red", size = 5, geom = "point",shape = "o")
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
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size = 3))

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
ggplot(corr_df_pearson, aes(x=reorder(status,corr_pearson),y=corr_pearson)) +
  geom_bar(stat="identity", width=0.5)

#corr kendall
corr_kendall <- cor(df2[,c(3:9)], df2$Szczêœcie, use = "complete.obs",method = "kendall")
corr_df_kendall <- data.frame(labels(corr_kendall),corr_kendall)
colnames(corr_df_kendall) <- c("status","corr_method","corr")
corr_df_kendall$corr_method <- "kendall"
ggplot(corr_df_kendall, aes(x=reorder(status,corr_kendall),y=corr_kendall)) +
  geom_bar(stat="identity", width=0.5)

#corr spearman
corr_spearman <- cor(df2[,c(3:9)], df2$Szczêœcie, use = "complete.obs",method = "spearman")
corr_df_spearman <- data.frame(labels(corr_spearman),corr_spearman)
colnames(corr_df_spearman) <- c("status","corr_method","corr")
corr_df_spearman$corr_method <- "spearman"
ggplot(corr_df_spearman, aes(x=reorder(status,corr_spearman),y=corr_spearman)) +
  geom_bar(stat="identity", width=0.5)

# 3 CORR W 1
corr_df_all <- rbind(corr_df_pearson,corr_df_kendall,corr_df_spearman)
ggplot(corr_df_all, aes(x=reorder(status,corr),y=corr,fill=corr_method)) +
  geom_bar(position="dodge",stat="identity", width=0.5)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


#œwiat bajer
world <- ne_countries(scale = "medium", returnclass = "sf")
world1 <- left_join(world, df2, by = c("name"="Pañstwo"))
ggplot(data = world1) +
  geom_sf(aes(fill = Szczêœcie)) +
  scale_fill_viridis_c(option = "plasma")

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
  geom_point()
cor(df2$`Zaufanie do rz¹du`,df2$Korupcja)
#





























































dane_2015 <- df[df$Year == 2015, ]
countries <- unique(df$Country)

country_rev <- df %>% group_by(df$happiness_score)

plot(dane_2015$government_trust,dane_2015$happiness_score)
pairs(~df$family+df$dystopia_residual,data = df,
      main = "Scatterplot Matrix")

plot(c("a","b"),c(1,2))
ord <- dane_2015[order(dane_2015$happiness_score),]
plot(ord$happiness_score,ord$Country,las=2)


#mo¿na zobaczyæ ze na przestrzeni lat poziom szczêœcia zasadniczo siê utrzymywa³
xyplot(happiness_score~Year,data=df,
       groups = Country, type=c("p","smooth"))
filter(df, df$happiness_score<5 && df$gdp_per_capita>5)   
#zatem mo¿na uœredniæ wyniki (trzebaby wszystko sprawdziæ)
df1 <- df
df1$mean_happines <- ave(df1$happiness_score, df1$Country)
df1 <- df1[!duplicated(df1$Country),]
View(df1)

ggplot(df1, aes(x = mean_happines, y = reorder(Country,mean_happines)))+
  geom_point(aes(color =continent))



# zobaczyc srednio z ca³ego œwiata jak siê zmienia³o

plot(df$cpi_score,df$happiness_score)
plot(df$family,df$happiness_score)
plot(df$health,df$happiness_score)
plot(df$government_trust,df$happiness_score)
plot(df$dystopia_residual,df$happiness_score)

res <- cor(df1[,c(2,3,4,5,6)])
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


