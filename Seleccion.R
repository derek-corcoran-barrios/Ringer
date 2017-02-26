#data of advanced stats
BD <- readRDS("big_data.rds")
BD <- BD[complete.cases(BD),]
BD <- BD[,-1]

#Data of trad stats

NS <- readRDS("NS.rds")
NS <- NS[complete.cases(NS),]
BD <- BD[,-1]

#Merge by player and season

BD <- merge(NS, BD, all = TRUE)


BD$Season <- (as.numeric(gsub("\\-.*","",BD$Season)) + 1)
BD$MPG <- as.numeric(as.character(BD$MP))/as.numeric(as.character(BD$G))

BD[,c(7:8,11:30)]<- sapply(BD[, c(7:8,11:30)], as.character)
BD[,c(7:8,11:30)]<- sapply(BD[, c(7:8,11:30)], as.numeric)

colnames(BD) <- gsub("3", "Three", colnames(BD))

colnames(BD) <- gsub("%", "pct", colnames(BD))


library(ggplot2)

ggplot(BD, aes(x = TRBpct, y = MPG)) + geom_point(aes(color = Season)) + geom_smooth(aes(fill = Season))
ggplot(BD, aes(x = ThreePAr, y = MPG)) + geom_point(aes(color = Season)) + geom_smooth(method = "lm",aes(fill = Season))

library(MuMIn)
library(caret)

preprocov <- preProcess(BD[,-c(1,2,3,31)])
BD[,-c(1,2,3,31)] <- predict(preprocov, BD[,-c(1,2,3,31)])

Models <- list()

options(na.action = "na.fail")


for(i in 1:length(unique(BD$Season))){
  temp <- dplyr::filter(BD, Season == unique(BD$Season)[i])
  Model <- glm(MPG~ ThreePAr + I(ThreePAr^2) + Age + FTr+ I(FTr^2) +  ASTpct + I(ASTpct^2) + STLpct +  BLKpct, data = temp)
  Select.Model <- dredge(Model)
  Best.Model<-get.models(Select.Model, 1)[[1]]
  Models[[i]] <- data.frame(matrix(summary(Best.Model)$coefficients[,1], ncol = length(summary(Best.Model)$coefficients[,1]), nrow = 1))
  colnames(Models[[i]]) <- rownames(summary(Best.Model)$coefficients)
  Models[[i]]$Season <- unique(BD$Season)[i]
}

Models <- Reduce(function(x,y) merge(x, y, all = TRUE), Models)

Models <- dplyr::arrange(Models, Season)
View(Models)

Model <- lm(MPG~ Season + ThreePAr + I(ThreePAr^2) + Age + FTr+ I(FTr^2) +  ASTpct + I(ASTpct^2) + STLpct +  BLKpct, data = BD)
