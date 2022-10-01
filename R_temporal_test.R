

install.packages("rjava")

library(rJava)

library(tidyverse)

rr<-readRDS("C:/Users/Geosciences/Desktop/AsthmaStudy/phys_inact/models.rds")
write.csv(rr,"C:/Users/Geosciences/Desktop/AsthmaStudy/phys_inact/models.csv", row.names = TRUE )




###bw for GRF
results_vector_grf_tuning=vector()
for(bw in seq(from=10, to=100, by=10)){
  set.seed(i)
  grf16.a <- grf(Obesity ~ green+Bike+ crimeratio+ Unemploy+ Eviction +Vacanthous+POV, dframe=data,bw=bw, kernel="adaptive", mtry=5,coords=data[,10:11],weighted=TRUE,ntree=500)
  results_vector_grf_tuning[bw]=grf16.a$LocalModelSummary$l.r.OOB
}
#bw=30


set.seed(1234)
for(bw in seq(from=20, to=80, by=1)){
  #set.seed(bw)
  grf16.a <- grf(asthma~insurance+cancer+UrbanIntense+NoInternet+UNEMP+singleparent+LimEnglish+Housing_T+NDVI+VacantH, dframe=data,bw=bw, kernel="adaptive",coords=data[,13:14],weighted=TRUE,ntree=1000,mtry=4, nthreads = 6)
  eval_BW_GRF[bw,1]=grf16.a$LocalModelSummary$l.r.OOB
  metrics_bw_grf=postResample(pred = (grf16.a$LGofFit$LM_yfitOOB +grf16.a$Global.Model$predictions)/2, obs = data$Pinactive)
  eval_BW_GRF[bw,2]=metrics_bw_grf[2]
  metrics_bw_grf=postResample(pred = (grf16.a$LGofFit$LM_yfitOOB*0.25) +(grf16.a$Global.Model$predictions*0.75), obs = data$Pinactive)
  eval_BW_GRF[bw,3]=metrics_bw_grf[2]
}