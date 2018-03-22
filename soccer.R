library("RSQLite")
library(DBI)
# connect to the sqlite file
con = dbConnect(RSQLite::SQLite(), dbname="Soccer.sqlite")
# get a list of all tables
alltables = dbListTables(con)
dbListTables(con) # print table names to know which one will we use 

#export Player  and Player_Attributes tables from SQLite to CSV
Player =  dbReadTable(con, "Player")
write.csv(Player, file = "Player.csv") 
P_Attributes  = dbReadTable(con, "Player_Attributes")
write.csv(P_Attributes, file = "Player_Attributes.csv")


Player = read.csv("Player.csv")
P_Attributes = read.csv("Player_Attributes.csv")
Players_Attributes= merge(Player,P_Attributes, by.x = "player_api_id", 
                          by.y = "player_api_id")

colnames(Players_Attributes)

needed_att = Players_Attributes[,-c(1,2,3,5,6,7,8,9,10,11,12,15,16,17)]
Players_Attributes=na.omit(Players_Attributes)#remove NA

avg_scores=apply(needed_att[,2:ncol(needed_att)],2,function(x)
  tapply(x,needed_att$player_name,mean))

avg_scores=na.omit(avg_scores)

avg_scores_pca=prcomp(avg_scores,scale. = T)
summary(avg_scores_pca)

smoothScatter(avg_scores_pca$x[,1:2],nrpoints = 0, 
              colramp = colorRampPalette(c("white", "gray5")))

comp <- data.frame(avg_scores_pca$x[,1:2])
k <- kmeans(comp, 3, nstart=25)
player_classes_Kmean3 = k$cluster
table(player_classes_Kmean3)
plot(comp, col=player_classes_Kmean3)

names(player_classes_Kmean3[player_classes_Kmean3==1][1:5])
names(player_classes_Kmean3[player_classes_Kmean3==2][1:5])
names(player_classes_Kmean3[player_classes_Kmean3==3][1:5])


k <- kmeans(comp, 4, nstart=25)
player_classes_Kmean4 = k$cluster
table(player_classes_Kmean4)
plot(comp, col=player_classes_Kmean4)

names(player_classes_Kmean4[player_classes_Kmean4==1][100:105])
names(player_classes_Kmean4[player_classes_Kmean4==2][100:105])
names(player_classes_Kmean4[player_classes_Kmean4==3][100:105])
names(player_classes_Kmean4[player_classes_Kmean4==4][100:105])

colnames(avg_scores)

player_classes_Kmean4[which(player_classes_Kmean4==1)]="defender"
player_classes_Kmean4[which(player_classes_Kmean4==2)]="attacker"
player_classes_Kmean4[which(player_classes_Kmean4==3)]="goalkeeper"
player_classes_Kmean4[which(player_classes_Kmean4==4)]="midfielder"
player_classes_Kmean4=factor(player_classes_Kmean4, 
                             levels = c("defender", "attacker", "goalkeeper","midfielder"))



k=4
boxplot(avg_scores[,k]~player_classes_Kmean4,main=colnames(avg_scores)[k],
        ylab="Average Score")


k=24
boxplot(avg_scores[,k]~player_classes_Kmean4,main=colnames(avg_scores)[k],
        ylab="Average Score")

k=35
boxplot(avg_scores[,k]~player_classes_Kmean4,main=colnames(avg_scores)[k],
        ylab="Average Score")

