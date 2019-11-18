#ML PROJ - ridge and lasso

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

#ridge regression
cv.out = cv.glmnet(X, y, alpha=0, family="binomial") 
plot(cv.out)
bestlambda=cv.out$lambda.min
bestlambda
ridge.mod.betas = coef(cv.out, s=bestlambda)
pred.ridge = as.double(predict(cv.out, s=bestlambda, newx=Xt, type='response'))
#PPV at depth 20%
cutoff <- (sort(pred.ridge))[length(pred.ridge)-.2*(length(pred.ridge))]
classpred <- rep(0, length(pred.ridge))
classpred[which(pred.ridge >= cutoff)] <- 1
cm <- confusionMatrix(factor(classpred), factor(yt))
cm$byClass[3] #.7925373


#lasso regression
cv.out = cv.glmnet(X, y, alpha=1, family="binomial") 
plot(cv.out)
bestlambda=cv.out$lambda.min
bestlambda
lasso.mod.betas = coef(cv.out, s=bestlambda)
pred.lasso = as.double(predict(cv.out, s=bestlambda, newx=Xt, type='response'))
#PPV at depth 20%
cutoff <- (sort(pred.lasso))[length(pred.lasso)-.2*(length(pred.lasso))]
classpred <- rep(0, length(pred.lasso))
classpred[which(pred.lasso >= cutoff)] <- 1
cm <- confusionMatrix(factor(classpred), factor(yt))
cm$byClass[3] #.7925373

