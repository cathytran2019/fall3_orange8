import os
os.chdir("C:/Users/Savannah Hampton/Documents/Master of Science in Analytics/Machine Learning")
import pandas as pd
import xgboost as xgb
from sklearn.metrics import roc_auc_score
import numpy as np
import warnings
warnings.filterwarnings('ignore')
from xgboost import XGBClassifier

train=pd.read_csv("MLProject_train.csv")
validation=pd.read_csv("MLProject_valid.csv")
forsubmission=pd.read_csv("MLProject_test.csv")

subsettrain=train[0:100000]
subsetvalid=validation[0:100000]
subsetforsubmission=forsubmission[0:100000]

x1, y1= subsettrain.iloc[:,:-2],subsettrain.iloc[:,-2]
x2 ,y2= subsettrain.iloc[:,:-2],subsettrain.iloc[:,-1]
x1test,y1test=subsetvalid.iloc[:,:-2], subsetvalid.iloc[:,-2]
x2test,y2test=subsetvalid.iloc[:,:-2], subsetvalid.iloc[:,-1]
x1sub,y1sub=subsetforsubmission.iloc[:,:-2],subsetforsubmission.iloc[:,-2]
x2sub,y2sub=subsetforsubmission.iloc[:,:-2],subsetforsubmission.iloc[:,-1]

data_matrix1=xgb.DMatrix(data=x1,label=y1)
data_matrix2=xgb.DMatrix(data=x2,label=y2)


x_train1=x1
y_train1=y1
x_train2=x2
y_train2=y2

x_valid1=x1test
y_valid1=y1test
x_valid2=x2test
y_valid2=y2test

x_submit1=x1sub
y_submit1=y1sub
x_submit2=x2sub
y_submit2=y2sub

xgb=XGBClassifier(objective='binary:logistic', silent=True, nthread=1)
xgb.fit(x_train1,y_train1)
preds=xgb.predict(x_valid1)
auc=roc_auc_score(y_valid1,preds)
print(auc)
#AUC: 0.6398713519275205

xgb=XGBClassifier(objective='binary:logistic', colsample_bylevel=0.5)
xgb.fit(x_train1,y_train1)
preds=xgb.predict(x_valid1)
auc=roc_auc_score(y_valid1,preds)
print(auc)
#AUC: 0.6558538052935665

xgb=XGBClassifier(objective='binary:logistic', learning_rate=0.03)
xgb.fit(x_train1,y_train1)
preds=xgb.predict(x_valid1)
auc=roc_auc_score(y_valid1,preds)
print(auc)
#AUC: 0.6433311481336692

xgb=XGBClassifier(objective='binary:logistic', learning_rate=1)
xgb.fit(x_train1,y_train1)
preds=xgb.predict(x_valid1)
auc=roc_auc_score(y_valid1,preds)
print(auc)
#AUC: 0.639093596641531

xgb=XGBClassifier(objective='binary:logistic', learning_rate=0.0000001)
xgb.fit(x_train1,y_train1)
preds=xgb.predict(x_valid1)
auc=roc_auc_score(y_valid1,preds)
print(auc)
#AUC: 0.7136840400710952

xgb=XGBClassifier(objective='binary:logistic', max_delta_step=1)
xgb.fit(x_train1,y_train1)
preds=xgb.predict(x_valid1)
auc=roc_auc_score(y_valid1,preds)
print(auc)
#AUC: 0.6446068651161619

xgb=XGBClassifier(objective='binary:logistic', learning_rate=0.0000001, silent=True, nthread=1)
xgb.fit(x_train1,y_train1)
preds=xgb.predict(x_valid1)
auc=roc_auc_score(y_valid1,preds)
print(auc)
#AUC: 0.7136840400710952

xgb=XGBClassifier(objective='binary:logistic', learning_rate=0.0000001)
xgb.fit(x_train1,y_train1)
preds=xgb.predict(x_valid1)
auc=roc_auc_score(y_valid1,preds)
print(auc)
xgb=XGBClassifier(objective='binary:logistic', learning_rate=0.0000001)
xgb.fit(x_train1,y_train1)
preds=xgb.predict(x_submit1)
auc=roc_auc_score(y_submit1,preds)
print(auc)
#AUC: 0.7185387878028251

xgb=XGBClassifier(objective='binary:logistic', silent=True, nthread=1)
xgb.fit(x_train2,y_train2)
preds=xgb.predict(x_valid2)
auc=roc_auc_score(y_valid2,preds)
print(auc)
#AUC: 0.6488561489337162

xgb=XGBClassifier(objective='binary:logistic', colsample_bylevel=0.5)
xgb.fit(x_train2,y_train2)
preds=xgb.predict(x_valid2)
auc=roc_auc_score(y_valid2,preds)
print(auc)
#AUC: 0.6999885204526374

xgb=XGBClassifier(objective='binary:logistic', learning_rate=0.03)
xgb.fit(x_train2,y_train2)
preds=xgb.predict(x_valid2)
auc=roc_auc_score(y_valid2,preds)
print(auc)
#AUC: 0.6063406797072178

xgb=XGBClassifier(objective='binary:logistic', learning_rate=1)
xgb.fit(x_train2,y_train2)
preds=xgb.predict(x_valid2)
auc=roc_auc_score(y_valid2,preds)
print(auc)
#AUC: 0.5340234472437018

xgb=XGBClassifier(objective='binary:logistic', learning_rate=0.0000001)
xgb.fit(x_train2,y_train2)
preds=xgb.predict(x_valid2)
auc=roc_auc_score(y_valid2,preds)
print(auc)
#AUC: 0.6039937559845187

xgb=XGBClassifier(objective='binary:logistic', max_delta_step=1)
xgb.fit(x_train2,y_train2)
preds=xgb.predict(x_valid2)
auc=roc_auc_score(y_valid2,preds)
print(auc)
#AUC: 0.6330997728229764

xgb=XGBClassifier(objective='binary:logistic', learning_rate=0.0000001, silent=True, nthread=1)
xgb.fit(x_train2,y_train2)
preds=xgb.predict(x_valid2)
auc=roc_auc_score(y_valid2,preds)
print(auc)
#AUC:0.6039937559845187

xgb=XGBClassifier(objective='binary:logistic', colsample_bylevel=0.5)
xgb.fit(x_train2,y_train2)
preds=xgb.predict(x_valid2)
auc=roc_auc_score(y_valid2,preds)
print(auc)
#AUC: 0.6999885204526374
xgb=XGBClassifier(objective='binary:logistic', colsample_bylevel=0.5)
xgb.fit(x_train2,y_train2)
preds=xgb.predict(x_submit2)
auc=roc_auc_score(y_submit2,preds)
print(auc)
#AUC: 0.6984066649366939