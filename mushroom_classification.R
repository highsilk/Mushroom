# Library   
library(dplyr)
library(ggplot2)
library(caret)
library(e1071)
library(GGally)
library(ROCR)
 
# Data
origin.mushroom <- read.table("C:/RClass/mushroom/agaricus-lepiota.data", sep=",", stringsAsFactors=T)
attach(origin.mushroom)
mushroom <- origin.mushroom
head(mushroom)
dim(mushroom)
str(mushroom)
sum(is.na(mushroom))

## Rename the variables
colnames(mushroom) <- c("class", "cap_shape", "cap_surface", "cap_color", "bruises", "odor", 
                        "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape",
                        "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", 
                        "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", 
                        "veil_color", "ring_number", "ring_type", "spore_print_color", "population", 
                        "habitat")

## Class level change
mushroom$class <- factor(mushroom$class,levels = c("p", "e"))

## Check missing data
unique(mushroom$stalk_root)
table(mushroom$stalk_root)

## Value check in each variable
str(mushroom) # only one variable in the veil_type
unique(mushroom$class)
unique(mushroom$cap_shape)
unique(mushroom$cap_surface)
unique(mushroom$cap_color)
unique(mushroom$bruises)
unique(mushroom$odor)
unique(mushroom$gill_attachment)
unique(mushroom$gill_spacing)
unique(mushroom$gill_size)
unique(mushroom$gill_color)
unique(mushroom$stalk_shape)
unique(mushroom$stalk_root) # missing value(?)
unique(mushroom$stalk_surface_above_ring)
unique(mushroom$stalk_surface_below_ring)
unique(mushroom$stalk_color_above_ring)
unique(mushroom$stalk_color_below_ring)
unique(mushroom$veil_type) # only one factor
unique(mushroom$veil_color)
unique(mushroom$ring_number)
unique(mushroom$ring_type)
unique(mushroom$spore_print_color)
unique(mushroom$population)
unique(mushroom$habitat)

## KNN imputation
mushroom$stalk_root[mushroom$stalk_root == "?"] <- NA
mushroom$stalk_root <- factor(droplevels(mushroom$stalk_root,exclude="missing"))
table(mushroom$stalk_root, useNA = "always")
sum(is.na(mushroom$stalk_root))

library(DMwR2)
Imputed_data <- knnImputation(mushroom,k=floor(sqrt(8124)))
#Imputed_data <- knnImputation(mushroom,k=10)
str(Imputed_data)
table(Imputed_data$stalk_root)
sum(is.na(Imputed_data$stalk_root))
mushroom <- Imputed_data
table(mushroom$stalk_root)
## Missing data in the variable stalk_root (replace it with the mode of the column ="b")
#mushroom$stalk_root[mushroom$stalk_root == "?"] <- "b"

## Remove the variable veil_type column
mushroom <- select(mushroom,-c(veil_type))

## Variable name (decision tree/random forest)
# list(): check the level of each variable
levels(mushroom$class) <- c("poisonous","edible")
levels(mushroom$odor) <- c("almond","creosote","foul","anise","musty","none","pungent","spicy","fishy")
levels(mushroom$spore_print_color) <- c("buff","chocolate","black","brown","orange","green","purple","white","yellow")
levels(mushroom$cap_shape) <- c("bell","conical","flat","knobbed","sunken","convex")
levels(mushroom$cap_color) <- c("buff","cinnamon","red","gray","brown","pink","green","purple","white","yellow")
levels(mushroom$cap_surface) <- c("fibrous","grooves","smooth","scaly")
levels(mushroom$bruises) <- c("no","bruises")
levels(mushroom$gill_attachment) <- c("attached","free")
levels(mushroom$gill_spacing) <- c("close","crowded")
levels(mushroom$gill_size) <- c("broad","narrow")
levels(mushroom$gill_color) <- c("buff","red","gray","chocolate","black","brown","orange","pink","green","purple","white","yellow")
levels(mushroom$stalk_shape) <- c("enlarging","tapering")
levels(mushroom$stalk_root) <- c("bulbous","club","equal","rooted", "missing")
levels(mushroom$stalk_surface_above_ring) <- c("fibrous","silky","smooth","scaly")
levels(mushroom$stalk_surface_below_ring) <- c("fibrous","silky","smooth","scaly")
levels(mushroom$stalk_color_above_ring) <- c("buff","cinnamon","red","gray","brown","orange","pink","white","yellow")
levels(mushroom$stalk_color_below_ring) <- c("buff", "cinnamon","red","gray","brown","orange","pink","white","yellow")
levels(mushroom$veil_color) <- c("brown","orange","white","yellow")
levels(mushroom$ring_number) <- c("none","one","two")
levels(mushroom$ring_type) <- c("evanescent","flaring","large","none","pendant")
levels(mushroom$population) <- c("abundant","clustered","numerous","scattered","several","solitary")
levels(mushroom$habitat) <- c("wood","grasses","leaves","meadows","paths","urban","waste")
mushroom$stalk_root <- factor(droplevels(mushroom$stalk_root,exclude="missing"))
table(mushroom$stalk_root)

# EDA
## The distribution of Target Variable
prop.table(table(mushroom$class))
p1=ggplot(mushroom, aes(class, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=18),legend.text = element_text(size=18),axis.text=element_text(size=18)
        ,axis.title=element_text(size=18))
p1

# Visualize data (Odor/Gill Color/Spore Print Color)
# Class/Odor
ggplot(mushroom, aes(x=class, y=odor, col=class)) + 
  geom_jitter(alpha = 0.5) + scale_color_manual(breaks = c("edible", "poisonous"), values = c("blue", "orange")) +
  theme(text = element_text(size = 20))  

# Class/Gill Color
ggplot(mushroom, aes(x=class, y=gill_color, col=class)) + 
  geom_jitter(alpha = 0.5) + scale_color_manual(breaks = c("edible", "poisonous"), values = c("blue", "orange"))+
  theme(text = element_text(size = 20))  

# Class/Spore Print Color
ggplot(mushroom, aes(x=class, y=spore_print_color, col=class)) + 
  geom_jitter(alpha = 0.5) + scale_color_manual(breaks = c("edible", "poisonous"), values = c("blue", "orange"))+
  theme(text = element_text(size = 20))  

# cap shape/cap color
ggplot(mushroom, aes(x=cap_shape, y=cap_color, col=class)) + 
  geom_jitter(alpha = 0.5) + scale_color_manual(breaks = c("edible", "poisonous"), values = c("blue", "orange"))

# gill size/gill color
ggplot(mushroom, aes(x=gill_size, y=gill_color, col=class)) + 
  geom_jitter(alpha = 0.5) + scale_color_manual(breaks = c("edible", "poisonous"), values = c("blue", "orange")) +
  theme(text = element_text(size = 20))  

# cap color/gill color
ggplot(mushroom, aes(x=cap_color, y=gill_color, col=class)) + 
  geom_jitter(alpha = 0.5) + scale_color_manual(breaks = c("edible", "poisonous"), values = c("blue", "orange"))+
  theme(text = element_text(size = 17))  


# Chi-square test: all variables
CHI <- lapply(mushroom[,-1], function(x) chisq.test(mushroom[,1],x)); CHI
CHI.result <- do.call(rbind,CHI)[,c(1,3)]
CHI.result <- data.frame(CHI.result)
CHI.result

## Correlation Plot
### One hot encoding (22 variables -< 95 variables)
library(caret)
mushroom1=data.frame(mushroom)
dummy <- dummyVars(" ~ .", data=mushroom1, fullRank=T)
mushroom1 <- data.frame(predict(dummy, newdata=mushroom1))
colnames(mushroom1)[1] <- "class"
#str(mushroom1)

### Multicollinearity plot (One hot Encoding data/Correlation Plot))
library(ggcorrplot)
a=round(cor(mushroom1,mushroom1),1)
ggcorrplot(cor(mushroom,mushroom))
ggcorrplot(a)

### Label encoding (Original data/Correlation Plot)
### variable (-> numeric)
mushroom3 <- mushroom
mushroom3[sapply(mushroom3,is.factor)] <- data.matrix(mushroom3[sapply(mushroom3,is.factor)])
str(mushroom3)
ggcorrplot(cor(mushroom3,mushroom3))


## Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# odor/bruises
g1 <- ggplot(mushroom, aes(x=odor, y=bruises, col=class)) + 
  geom_jitter(alpha = 0.5) + scale_color_manual(breaks = c("edible", "poisonous"), values = c("blue", "orange")) +
  theme(text = element_text(size = 19)) + theme(axis.text.x=element_text(angle=90)) 

# gill color/gill size
g2 <- ggplot(mushroom, aes(x=gill_color, y=gill_size, col=class)) + 
  geom_jitter(alpha = 0.5) + scale_color_manual(breaks = c("edible", "poisonous"), values = c("blue", "orange")) +
  theme(text = element_text(size = 19)) + theme(axis.text.x=element_text(angle=90)) 

# spore print color/odor
g3 <- ggplot(mushroom, aes(x=spore_print_color, y=odor, col=class)) + 
  geom_jitter(alpha = 0.5) + scale_color_manual(breaks = c("edible", "poisonous"), values = c("blue", "orange")) +
  theme(text = element_text(size = 19)) + theme(axis.text.x=element_text(angle=90)) 

# population/habitat
g4 <- ggplot(mushroom, aes(x=population, y=habitat, col=class)) + 
  geom_jitter(alpha = 0.5) + scale_color_manual(breaks = c("edible", "poisonous"), values = c("blue", "orange")) +
  theme(text = element_text(size = 19)) + theme(axis.text.x=element_text(angle=90)) 

multiplot(g1,g4, cols=1)

# Bar plot
p2=ggplot(mushroom, aes(cap_shape, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p3=ggplot(mushroom, aes(cap_surface, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p4=ggplot(mushroom, aes(cap_color, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p5=ggplot(mushroom, aes(bruises, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p6=ggplot(mushroom, aes(odor, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p7=ggplot(mushroom, aes(gill_attachment, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p8=ggplot(mushroom, aes(gill_spacing, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p9=ggplot(mushroom, aes(gill_size, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p10=ggplot(mushroom, aes(gill_color, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p11=ggplot(mushroom, aes(stalk_shape, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p12=ggplot(mushroom, aes(stalk_root, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p13=ggplot(mushroom, aes(stalk_surface_above_ring, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p14=ggplot(mushroom, aes(stalk_surface_below_ring, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p15=ggplot(mushroom, aes(stalk_color_above_ring, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p16=ggplot(mushroom, aes(stalk_color_below_ring, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
#ggplot(mushroom, aes(veil_type, ..count..)) + geom_bar(aes(fill=class), position="dodge")
p17=ggplot(mushroom, aes(veil_color, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p18=ggplot(mushroom, aes(ring_number, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p19=ggplot(mushroom, aes(ring_type, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p20=ggplot(mushroom, aes(spore_print_color, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p21=ggplot(mushroom, aes(population, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
p22=ggplot(mushroom, aes(habitat, ..count..)) + geom_bar(aes(fill=class), position="dodge")+
  theme(legend.title = element_text(size=15),legend.text = element_text(size=15),axis.text=element_text(size=15)
        ,axis.title=element_text(size=15))
multiplot(p2, p3, p4, p5, cols=2)
multiploy(p6, p7, p8, p9, cols=2)
multiplot(p10, p11, p12, p13, cols=2)
multiplot(p14, p15, p16, p17, cols=2)
multiplot(p18, p19, p20, p21, cols=2)
multiplot(p22," "," "," ", cols=2)


# Method
## [1] Decision Tree
### training/test split
set.seed(2022)
train=sample(nrow(mushroom), floor(nrow(mushroom)*0.8), replace=FALSE) # 70% of data
mushroom.train=mushroom[train,]
mushroom.test=mushroom[-train,]
target.train=mushroom[train,1]
target.test=mushroom[-train,1]

### Model
library(rpart)
library(rpart.plot)
mushroom.dt <- rpart(class~.,data=mushroom.train,method="class") # cp=0.001
summary(mushroom.dt)
rpart.plot(mushroom.dt,branch.lty=2, shadow.col="gray", nn=TRUE) 
printcp(mushroom.dt)
plotcp(mushroom.dt, cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5) 
cp1 <- mushroom.dt$cptable[which.min(mushroom.dt$cptable[,"xerror"]),"CP"]
pruned.tree <- prune(mushroom.dt, cp=cp1)
rpart.plot(pruned.tree, extra=106, branch.lty=2, shadow.col="gray", nn=TRUE)
summary(pruned.tree)

## Predicting test values
dt.pred <- predict(mushroom.dt,newdata=mushroom.test,type="class")
table(dt.pred, target.test)
dt.accuracy.rate <- mean(dt.pred==target.test)
dt.accuracy.rate # accuracy
1-dt.accuracy.rate # misclassification

## Predicting test values (post pruning)
dt.pred <- predict(pruned.tree,newdata=mushroom.test,type="class")
table(dt.pred, target.test)
dt.accuracy.rate <- mean(dt.pred==target.test)
dt.accuracy.rate # accuracy
1-dt.accuracy.rate # misclassification

## ROC Curve
### ver.1
tree.pred <- predict(pruned.tree, mushroom.test, type="prob")[,2]
library(pROC)
tree.roc <- roc(target.test, tree.pred)
print(tree.roc)
plot(tree.roc)

### ver.2
library(ggplot2)
library(pROC)
rocobj1 <- roc(target.test, tree.pred)
rocobj1$auc 
ggroc(rocobj1, size=1, colour="blue") + theme(text = element_text(size = 20)) + 
  annotate("text", x=0.1, y=0.1, label= "AUC=99.44%", col="blue", size=5)

## [2] Random Forest
### Target variable
#str(mushroom)
#set.seed(2022)
#mushroom$class <- ifelse(mushroom$class=="edible",1,0)
#mushroom$class=as.factor(mushroom$class)
str(mushroom)
### Split training/test split
set.seed(12345)
train=sample(nrow(mushroom), floor(nrow(mushroom)*0.8), replace=FALSE)
mushroom.train=mushroom[train,]
mushroom.test=mushroom[-train,]
target.train=mushroom[train,1]
target.test=mushroom[-train,1]

### Random Forest
library(randomForest)
# randomForest -> mtry=sqrt(p):classification 
mushroom.rf <- randomForest(class~.,data=mushroom.train,importance=TRUE) 
mushroom.rf
mushroom.rf$importance
plot(mushroom.rf, xlim=c(0,30))  
print(mushroom.rf)
varImpPlot(mushroom.rf,sort=TRUE,main="Variable Importance")
library(vip)
vip(mushroom.rf, aesthetics = list(col = "purple2"),)

### Variable Importance Plot (rf)
library(broom)
mushroom.rf$importance[,4] %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  ggplot(aes(reorder(names,x),x, fill=reorder(names,x))) +
  geom_col() +
  coord_flip() +
  #  ggtitle("Important Variables") +
  labs(x= "", y = "Mean Decrease Gini") + 
  theme(text = element_text(size = 19)) +
  theme(legend.position = "none")

### Error Plot
oob.err.data <- data.frame(
  Trees = rep(1:nrow(mushroom.rf$err.rate), 3), 
  Type = rep(c("OOB","Poisonous","Edible"), each = nrow(mushroom.rf$err.rate)),
  Error = c(mushroom.rf$err.rate[,"OOB"], mushroom.rf$err.rate[,2], mushroom.rf$err.rate[,3]))

ggplot(data = oob.err.data, aes(x = Trees, y= Error)) + geom_line(aes(color = Type))  +
  theme(text = element_text(size = 16)) 

ggplot(data = oob.err.data, aes(x = Trees, y= Error)) + geom_line(aes(color = Type)) +  xlim(0, 30) +
  theme(text = element_text(size = 16)) 

### Predicting test values
rf.pred <- predict(mushroom.rf,newdata=mushroom.test,type="class")
table(rf.pred, target.test)
rf.accuracy.rate <- mean(rf.pred==target.test)
rf.accuracy.rate # accuracy
1-rf.accuracy.rate # misclassification

### ver.1
rf.pred <- predict(mushroom.rf, mushroom.test, type="prob")[,2]
library(pROC)
rf.roc <- roc(target.test, rf.pred)
print(rf.roc)
plot(rf.roc)

### ver.2
library(ggplot2)
library(pROC)
rocobj2 <- roc(target.test, rf.pred)
rocobj2$auc
ggroc(rocobj2, size=1, colour="blue") + theme(text = element_text(size = 20)) + 
  annotate("text", x=0.1, y=0.1, label= "AUC=100%", col="blue", size=5)


## [3] Naive Bayes
## One hot encoding (22 variables -< 95 variables)
set.seed(2022)
library(caret)
mushroom1=data.frame(mushroom)
dummy <- dummyVars(" ~ .", data=mushroom1, fullRank=T)
mushroom1 <- data.frame(predict(dummy, newdata=mushroom1))
colnames(mushroom1)[1] <- "class"
#str(mushroom1)

### Split training/test dataset
set.seed(2022)
train=sample(nrow(mushroom1), floor(nrow(mushroom)*0.8), replace=FALSE) # 80% of data
mushroom1.train=mushroom1[train,]
mushroom1.test=mushroom1[-train,]
target1.train=mushroom1[train,1]
target1.test=mushroom1[-train,1]

### Naive Bayes
library(e1071)
mushroom.nv <- naiveBayes(class~.,data=mushroom1.train)
mushroom.nv
nv.pred <- predict(mushroom.nv, newdata=mushroom1.test)
library(caret)
confusionMatrix(nv.pred,as.factor(target1.test))

### Predicting test values
nv.pred <- predict(mushroom.nv,newdata=mushroom1.test,type="class")
table(nv.pred, target1.test)
nv.accuracy.rate <- mean(nv.pred==target1.test)
nv.accuracy.rate # accuracy
1-nv.accuracy.rate # misclassification

### ROC Curve
### ver.1
nv.pred <- predict(mushroom.nv, mushroom1.test, type="raw")[,2]
library(pROC)
nv.roc <- roc(target1.test, nv.pred)
print(nv.roc)
plot(nv.roc)
### ver.2
#load necessary packages
library(ggplot2)
library(pROC)
rocobj3 <- roc(target1.test, nv.pred)
rocobj3$auc
ggroc(rocobj3, size=1, colour="blue") + theme(text = element_text(size = 20)) + 
  annotate("text", x=0.1, y=0.1, label= "AUC=96%", col="blue", size=5)

## [4] SVM
## One hot encoding (22 variables -< 95 variables)
set.seed(2022)
library(caret)
mushroom1=data.frame(mushroom)
dummy <- dummyVars(" ~ .", data=mushroom1, fullRank=T)
mushroom1 <- data.frame(predict(dummy, newdata=mushroom1))
colnames(mushroom1)[1] <- "class"
#str(mushroom1)

### Split training/test dataset
set.seed(2022)
train=sample(nrow(mushroom1), floor(nrow(mushroom)*0.8), replace=FALSE) # 80% of data
mushroom1.train=mushroom1[train,]
mushroom1.test=mushroom1[-train,]
target1.train=mushroom1[train,1]
target1.test=mushroom1[-train,1]

### SVM 
library(e1071)
mushroom.svm <- svm(class~., data=mushroom1.train, type='C-classification', kernel="radial")
summary(mushroom.svm)
test.svm <- predict(mushroom.svm, newdata = mushroom1.test)
table(test.svm, target1.test)

### ROC Curve
library(ggplot2)
library(pROC)
rocobj4 <- roc(target1.test, as.numeric(test.svm))
rocobj4$auc
ggroc(rocobj4, size=1, colour="blue") + theme(text = element_text(size = 20)) + 
  annotate("text", x=0.1, y=0.1, label= "AUC=100%", col="blue", size=5)

##### SVM tuning #####
gammalist <- c(0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05)
tune.out <- tune.svm(as.factor(class)~., data=mushroom1.train, 
                     kernel="radial",cost=2^(-1:5),gamma=gammalist)
summary(tune.out)
summary(tune.out$best.model)
svm1 <- predict(tune.out$best.model,d.test[,-1])
confusionMatrix(svm1,as.factor(target1.test$class))

tune.out2 <- tune.svm(as.factor(class)~., data=d.train, 
                      kernel="linear",cost=2^(-1:5),gamma=gammalist)
summary(tune.out2)
summary(tune.out2$best.model)
svm2 <- predict(tune.out2$best.model,d.test[,-1])
confusionMatrix(svm2,as.factor(d.test$class))


### ROC Curve (all together)
library(ggplot2)
library(pROC)
#### Decision Tree
rocobj1 <- roc(target.test, tree.pred)
rocobj1$auc 
ggroc(rocobj1, size=1, colour="blue", linetype=1) + theme(text = element_text(size = 20)) + 
  annotate("text", x=0.1, y=0.1, label= "AUC=99.44%", col="blue", size=5) 
#### Random FOrest
rocobj2 <- roc(target.test, rf.pred)
rocobj2$auc
ggroc(rocobj2, size=1, colour="blue", linetype=1) + theme(text = element_text(size = 20)) + 
  annotate("text", x=0.1, y=0.1, label= "AUC=100%", col="blue", size=5)
#### Naive Bayes
rocobj3 <- roc(target1.test, nv.pred)
rocobj3$auc
ggroc(rocobj3, size=1, colour="blue", linetype=1) + theme(text = element_text(size = 20)) + 
  annotate("text", x=0.1, y=0.1, label= "AUC=96.09%", col="blue", size=5)
#### SVM
rocobj4 <- roc(target1.test, as.numeric(test.svm))
rocobj4$auc
ggroc(rocobj4, size=1, colour="blue", linetype=2) + theme(text = element_text(size = 20)) + 
  annotate("text", x=0.1, y=0.1, label= "AUC=100%", col="blue", size=5)

ggroc(list(Decision_Tree = rocobj1, Random_Forest = rocobj2, Naive_Bayes = rocobj3, SVM = rocobj4),
      aes=c("color","linetype")) + 
  theme(text = element_text(size = 20)) +
  annotate("text", x=0.1, y=0.2, label= "AUC=99.44%", col="orange", size=5) +
  annotate("text", x=0.1, y=0.3, label= "AUC=100%", col="green", size=5) +
  annotate("text", x=0.1, y=0.1, label= "AUC=96%", col="red", size=5) +
  annotate("text", x=0.1, y=0.4, label= "AUC=100%", col="blue", size=5) +
  geom_line(size=1) +
  scale_color_manual(values=c('orange', 'green', 'red', 'blue')) +
  scale_linetype_manual(values=c(1,1,1,2))
