inputTimeSeries <- ts(data = c(rep(c(1,2,3,4,5,6,7,8,9,10),times=12)),start = c(1995,1),frequency = 12)

naRemover<-function(value)
{
  if(is.na(value)==T)
  {
    return(0)
  }else {
    return(value)
  }
}


X12ARIMA<-function(inputTimeSeries)
{
  
  firstMovingAverageRoughTrend<-ma(inputTimeSeries,order = 12)
  
  roughTrendEstimate<-ma(firstMovingAverageRoughTrend,order = 2)
  
  centeredRatio<-inputTimeSeries/roughTrendEstimate
  
  roughSeasonalEstimate<-centeredRatio
  
  outlierRemoved<-0
  
  for(i in 0:(ceiling(length(roughSeasonalEstimate)/60)-1))
  {
    outlierTreatment<-roughSeasonalEstimate[((60*i)+1):(60*(i+1))]
    thresholdSd<-1.5*sd(outlierTreatment,na.rm = T)
    treatIndex<-which(outlierTreatment>abs(thresholdSd))+60*i
    for(j in treatIndex)
    {
      if((naRemover(outlierTreatment[j])-abs(thresholdSd))>1)
      {
        outlierTreatment[j]<-(outlierTreatment[j-1]+outlierTreatment[j+1])/2
      }
      else
      {
        outlierTreatment[j]<-(outlierTreatment[j-1]+outlierTreatment[j+1] + (1-((outlierTreatment[j])-abs(thresholdSd)))*outlierTreatment[j])/3
      }
      
    }
    outlierRemoved<-c(outlierRemoved,outlierTreatment)
  }
  
  outlierRemoved<-ts(data = outlierRemoved,start = attributes(roughSeasonalEstimate)$tsp[1],end = attributes(roughSeasonalEstimate)$tsp[2],freq=attributes(roughSeasonalEstimate)$tsp[3])
  
  for(i in 1:12)
  {
    window(outlierRemoved,
           c(attributes(outlierRemoved)[[1]][1], i),
           deltat = 1)<-
      ma(ma(subset(outlierRemoved, month = i),order=3),
         order=3)
  }
  
  
  
}


Seriesafterstep5inthedocument<-X12ARIMA(inputTimeSeries)
