library(dplyr)
require(tigerstats)
library(corrplot)


df <- read.csv("C:\\Users\\zleko\\Desktop\\WorldHappiness_Corruption_2015_2020.csv")
df[df==0] <- NA
View(df)

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
