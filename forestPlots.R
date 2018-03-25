
library(readr)
statsbygenre <- read_csv("musicDataScience/statsbygenre.csv")
View(statsbygenre)

countsByGenre <- c(535, 514, 997, 483, 3089, 538, 2659, 2416)
statsbygenre[,ncol(statsbygenre)+1] <-  countsByGenre
colnames(statsbygenre)[ncol(statsbygenre)] <- 'n'



library(ggplot2)


createForestPlot <- function(df, title1){
  
  fPlot <- ggplot(data=df, aes(x=label, y=mean, ymin=lower, ymax=upper)) +
  geom_pointrange(fatten = 8, colour = rainbow(8)) + 
  coord_flip()+
  xlab("Genre") + ylab("Mean (95% CI)") +
  labs(title = title1)+
  theme_bw() 
  return(fPlot)
  
}

names <- c('Acousticness', 'Danceability', 'Duration (ms)', 'Energy','Instrumentalness','Liveness','Loudness', 
  'Speechiness', 'Tempo', 'Time Signature','Valence','Popularity')



forestPlots <- lapply(c(3:16), function(x){
  print(x)
  label <- statsbygenre$genre
  mean <- statsbygenre[,x]
  lower <- mean - 1.96*(sqrt(statsbygenre[,(x+28)]/statsbygenre$n))
  upper <- mean + 1.96*(sqrt(statsbygenre[,(x+28)]/statsbygenre$n))
  data <- data.frame(label, mean, lower, upper)
  data <- as.data.frame(apply(data, 2,as.numeric))
  colnames(data) <- c('label','mean','lower','upper')
  data$label <- statsbygenre$genre
  title2 = paste("Mean of", names[x-2],"by Genre")
  png(paste(names[x-2],".png", sep = ""))
  myplot <- createForestPlot(data, title2)
  print(myplot)
  dev.off()
  myplot
})

