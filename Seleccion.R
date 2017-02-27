#data of advanced stats
BD <- readRDS("big_data.rds")
BD <- BD[complete.cases(BD),]
BD <- BD[,-1]
BD$MPG <- as.numeric(as.character(BD$MP))/as.numeric(as.character(BD$G))

BD <- BD[,c(1,2,3,4,30)]


#Data of stats per 100 possessions

NS <- readRDS("NS.rds")
NS <- NS[complete.cases(NS),]
NS <- NS[,-1]

#Merge by player and season

BD <- merge(NS, BD, all = TRUE)

BD <- BD[complete.cases(BD),]


BD$Season <- (as.numeric(gsub("\\-.*","",BD$Season)) + 1)

NN <- dplyr::group_by(BD, Season)
NN <- dplyr::summarise(NN, n())

BD[,c(7:33)]<- sapply(BD[, c(7:33)], as.character)
BD[,c(7:33)]<- sapply(BD[, c(7:33)], as.numeric)

colnames(BD) <- gsub("3", "Three", colnames(BD))
colnames(BD) <- gsub("2", "Two", colnames(BD))
colnames(BD) <- gsub("%", "pct", colnames(BD))


library(ggplot2)
library(MuMIn)
library(caret)

BD$ThreePpct[is.na(BD$ThreePpct)] <- 0
preprocov <- preProcess(BD[,-c(1:9,33)])
BD[,-c(1:9,33)] <- predict(preprocov, BD[,-c(1:9,33)])

Vars<- BD[,c(13,15,17,20,21,22,23,24,26,28:31)]

#Get lower triangle matrix to subsett without high correlation


Models <- list()

options(na.action = "na.fail")


for(i in 1:length(unique(BD$Season))){
  temp <- dplyr::filter(BD, Season == unique(BD$Season)[i])
  Vars<- BD[,c(13,15,17,20,21,22,23,24,26,28:31)]
  smat <- abs(cor(Vars)) <= .7
  smat[!lower.tri(smat)] <- NA
  Model <- glm(MPG~ TwoPA + ThreePA + FTA+ TRB +  AST + STL + BLK +  TOV + PTS +  TwoPpct + ThreePpct + eFGpct + FTpct, data = temp)
  Select.Model <- dredge(Model, subset = smat)
  Best.Model<-get.models(Select.Model, 1)[[1]]
  Models[[i]] <- data.frame(matrix(summary(Best.Model)$coefficients[,1], ncol = length(summary(Best.Model)$coefficients[,1]), nrow = 1))
  colnames(Models[[i]]) <- rownames(summary(Best.Model)$coefficients)
  Models[[i]]$Season <- unique(BD$Season)[i]
}

Models <- Reduce(function(x,y) merge(x, y, all = TRUE), Models)

Models <- dplyr::arrange(Models, Season)
View(Models)

saveRDS(Models, "Models.rds")
Models <- readRDS("Models.rds")
library(reshape2)

Models <- Models[,-1]

#how many nonNA per column:

colSums(!is.na(Models))
rowSums(!is.na(Models))

dfm <- melt(Models, id.vars="Season")
dfm$sign <- ifelse(dfm$value > 0 , "+" , ifelse(dfm$value < 0, "-", ""))


p <- ggplot(dfm, aes(x=variable, y=Season)) +  geom_tile(aes(fill=value), colour = "black") + scale_fill_gradient2(low = "blue", high = 'red', name="Strength") + theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1)) + theme_classic()
p


dfm2 <- dplyr::filter(dfm, variable == "AST" | variable == "TRB" | variable == "PTS" | variable == "TOV")

ggplot(data=dfm2, aes(x=Season, y=abs(value), colour=variable)) +geom_point() + geom_smooth(aes(fill = variable)) + ylab("Strength of relationship")


###############LImiting Variables


Models2 <- list()

options(na.action = "na.fail")


for(i in 1:length(unique(BD$Season))){
  temp <- dplyr::filter(BD, Season == unique(BD$Season)[i])
  Vars<- BD[,c(13,15,17,20,21,22,23,24,26,28:31)]
  smat <- abs(cor(Vars)) <= .7
  smat[!lower.tri(smat)] <- NA
  Model <- glm(MPG~ TwoPA + ThreePA + FTA+ TRB +  AST + STL + BLK +  TOV + PTS +  TwoPpct + ThreePpct + eFGpct + FTpct, data = temp)
  Select.Model <- dredge(Model, subset = smat,m.lim = c(0, 3))
  Best.Model<-get.models(Select.Model, 1)[[1]]
  Models2[[i]] <- data.frame(matrix(summary(Best.Model)$coefficients[,1], ncol = length(summary(Best.Model)$coefficients[,1]), nrow = 1))
  colnames(Models2[[i]]) <- rownames(summary(Best.Model)$coefficients)
  Models2[[i]]$Season <- unique(BD$Season)[i]
}

Models2 <- Reduce(function(x,y) merge(x, y, all = TRUE), Models2)

Models2 <- dplyr::arrange(Models2, Season)
View(Models2)



library(reshape2)

Models2 <- Models2[,-1]

dfm <- melt(Models2, id.vars="Season")
dfm$sign <- ifelse(dfm$value > 0 , "+" , ifelse(dfm$value < 0, "-", ""))


p <- ggplot(dfm, aes(x=variable, y=Season)) +  geom_tile(aes(fill=value), colour = "black") + scale_fill_gradient2(low = "blue", high = 'red', name="Strength") + theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1))
p

dfm2 <- dplyr::filter(dfm, variable == "AST" | variable == "TRB" | variable == "PTS")

ggplot(data=dfm2, aes(x=Season, y=abs(value), colour=variable)) +geom_point() + geom_smooth(aes(fill = variable))
