#ML PROJ - xgboost

#import data
ml_t <- read_csv('/Users/jacksonperry/Desktop/Fall3/MLProject_train.csv')
ml_v <- read_csv('/Users/jacksonperry/Desktop/Fall3/MLProject_valid.csv')

#remove rows with na
ml_t <- ml_t[complete.cases(ml_t), ]
ml_v <- ml_v[complete.cases(ml_v), ]

#random subset
set.seed(718)
train <- ml_t[sample(nrow(ml_t), 100000), c(1:148,150)]
X=model.matrix(target2~. ,data=train)[,-1]
y = train$target2
test <- ml_v[,c(1:148,150)]
Xt=model.matrix(target2~. ,data=test)[,-1]
yt= test$target2

#run xgboost
xgb <- xgboost(data = X, 
               label = y, 
               eta = 0.1,
               max_depth = 15, 
               nround=20, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               objective = "binary:logistic",
               nthread = 3
)

#score test data
y_pred <- predict(xgb, Xt)

#find optimal threshold
for (i in 1:50){
  cutoff <- as.double(i/50)
  classpred <- rep(0, length(y_pred))
  classpred[which(y_pred >= cutoff)] <- 1
  cm <- confusionMatrix(factor(classpred), factor(yt))
  print(as.double(i/50))
  print(cm$byClass[3])
} #.1 to .2 looks good

for (i in 1:20){
  cutoff <- .1 + as.double(i/200)
  classpred <- rep(0, length(y_pred))
  classpred[which(y_pred >= cutoff)] <- 1
  cm <- confusionMatrix(factor(classpred), factor(yt))
  print(.1 + as.double(i/200))
  print(cm$byClass[3])
} #.145 looks like the optimal cutoff with PPV = .97

#let's take a closer look at that
cutoff = .145
classpred <- rep(0, length(y_pred))
classpred[which(y_pred >= cutoff)] <- 1
cm <- confusionMatrix(factor(classpred), factor(yt))
cm
#we're predicting way more of the event than we should be

#let's calculate PPV at a depth of 20%
cutoff <- (sort(y_pred))[length(y_pred)-.2*(length(y_pred))]
classpred <- rep(0, length(y_pred))
classpred[which(y_pred >= cutoff)] <- 1
cm <- confusionMatrix(factor(classpred), factor(yt))
cm
#PPV at a depth of 20% is .7738