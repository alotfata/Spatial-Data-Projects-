library(lctools)
library(SpatialML)
library(caret)
library(GWmodel)
library(sp)
library(randomForest)
library(readr)
library(rgdal)
library(ranger)
#import csv for GRF and OLS models
data <- read_csv("ras_df.csv")
#import shapefile for GWR models
spatialdataset <- readOGR("C:/Users/Geosciences/Desktop/micromobility/physicalFinalMarco.shp")
data=as.data.frame(data)

##caluclate bw for GWR
#134 is found as optimal
gwr_bw=bw.gwr(Pinactive ~ Crime+Poverty+pct_transi+pct_tran_1+avg_hh_siz+avg_hh_s_1+Eviction + Unemploy+GreenS+retail_den, spatialdataset, approach="AICc", kernel="bisquare",
              adaptive=TRUE,p=2, theta=0,longlat=F)



#GRF optimize mtry parameter
### cross validate random forest using grid search
control <- trainControl(method="repeatedcv", repeats=10, number=10, search="grid")
#set.seed(123)
tunegrid <- expand.grid(.mtry=c(1:10))
rf_gridsearch <- train(Pinactive~  Crime+Poverty+rentertransit+ownertransit+rentersize+ownersize+Eviction + Unemploy+GreenS+retail_den, data= data, method="rf", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
#6 is best value for mtry parameter



###bw for GRF
results_vector_grf_tuning=vector()
for(bw in seq(from=10, to=100, by=10)){
  set.seed(i)
  grf16.a <- grf(Pinactive~  Crime+Poverty+rentertransit+ownertransit+rentersize+ownersize+Eviction + Unemploy+GreenS+retail_den, dframe=data,bw=bw, kernel="adaptive", mtry=5,coords=data[,10:11],weighted=TRUE,ntree=1000)
  results_vector_grf_tuning[bw]=grf16.a$LocalModelSummary$l.r.OOB
}
#bw=30



###



eval_grf <- data.frame(R_squared=double(),
                              MAE=double(),
                              RMSE=double(),
                              stringsAsFactors=FALSE
)

eval_grf_lm_050 <- data.frame(R_squared=double(),
                       MAE=double(),
                       RMSE=double(),
                       stringsAsFactors=FALSE
)

eval_ols = data.frame(R_squared=double(),
                               MAE=double(),
                               RMSE=double(),
                               stringsAsFactors=FALSE)
eval_gwr = data.frame(R_squared=double(),
                       MAE=double(),
                       RMSE=double(),
                       stringsAsFactors=FALSE)

## random testing and splitting 10 repeats ##
for (i in 1:10){
  #90% best
  #set.seed(i)
  set.seed(i)
  smp_size <- floor(0.90 * nrow(data))
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  gwr_train=subset(spatialdataset, spatialdataset$geoid10 %in% train$geoid10 )
  gwr_test=subset(spatialdataset, spatialdataset$geoid10 %in% test$geoid10 )
  gwr.basic=gwr.predict(Pinactive ~ Crime+Poverty+pct_transi+pct_tran_1+avg_hh_siz+avg_hh_s_1+Eviction + Unemploy+GreenS+retail_den, bw=134,data=gwr_train, predictdata=gwr_test, kernel="bisquare",adaptive=TRUE, p=2,
                        theta=0, longlat=F) 
  grf <- grf(Pinactive ~ Crime+Poverty+pct_transi+pct_tran_1+avg_hh_siz+avg_hh_s_1+Eviction + Unemploy+GreenS+retail_den, importance="permutation",weighted=TRUE,dframe=train, bw=30,ntree=1000,kernel="adaptive",mtry=5, coords=train[,10:11])
  glm=lm(Pinactive ~ Crime+Poverty+pct_transi+pct_tran_1+avg_hh_siz+avg_hh_s_1+Eviction + Unemploy+GreenS+retail_den, data=train)
  lm_pred= as.data.frame(predict(glm,newdata=test))
  grf_pred=predict.grf(grf, test, x.var.name="X", y.var.name="Y", local.w=1, global.w=0)
  grf_pred_lm_050=predict.grf(grf,test,x.var.name="X",y.var.name="Y",local.w=0.5,global.w=0.5)
  metrics_grf=postResample(pred = grf_pred, obs = test$Pinactive)
  metrics_grf_lm_050=postResample(pred =  grf_pred_lm_050, obs = test$Pinactive)
  metrics_lm=postResample(pred = lm_pred[,1], obs = test$Obesity)
  metrics_gwr=postResample(pred=gwr.basic[["SDF"]]@data[["prediction"]],obs=gwr_test$Pinactive)
  eval_grf[i,1]=metrics_grf[2]
  eval_grf[i,2]=metrics_grf[3]
  eval_grf[i,3]=metrics_grf[1]
  eval_grf_lm_050[i,1]=metrics_grf_lm_050[2]
  eval_grf_lm_050[i,2]=metrics_grf_lm_050[3]
  eval_grf_lm_050[i,3]=metrics_grf_lm_050[1]
  eval_ols[i,1]=metrics_lm[2]
  eval_ols[i,2]=metrics_lm[3]
  eval_ols[i,3]=metrics_lm[1]
  eval_gwr[i,1]=metrics_gwr[2]
  eval_gwr[i,2]=metrics_gwr[3]
  eval_gwr[i,3]=metrics_gwr[1]
}

##extract final results
mean(eval_ols$R_squared)
mean( eval_grf$R_squared)
mean( eval_grf_lm_050$R_squared)
mean( eval_gwr$R_squared)
mean(eval_ols$RMSE)
mean( eval_grf$RMSE)
mean( eval_grf_lm_050$RMSE)
mean( eval_gwr$RMSE)
mean(eval_ols$MAE)
mean( eval_grf$MAE)
mean( eval_grf_lm_050$MAE)
mean( eval_gwr$MAE)



