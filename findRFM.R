findRFM <- function(customerdata,
                    recencyWeight = 4, 
                    frequencyWeight = 4,
                    monetoryWeight = 4)
{
  
  
  
  
  
  colnames(customerdata) <- c("TransNo","CustomerID","DateofPurch","Amount")
  
  if (class(customerdata$DateofPurch) != "Date")
  {
    stop("The date of purchase should be in date format")
  }
  
  
  customerdata <- mutate(customerdata,TransNo = as.character(TransNo),
                         CustomerID = as.character(CustomerID))
  
  rfmData <- customerdata %>% group_by(CustomerID) %>%summarize(TotalValue = sum(Amount),
                                                                LastTransaction = max(DateofPurch),
                                                                NoTransaction = n())
  
  rfmData <- mutate(rfmData,
                    MonetoryPercentile = percent_rank(TotalValue),
                    FrequencyPercentile = percent_rank(NoTransaction),
                    RecencyPercentile = percent_rank(LastTransaction))
  rfmData <- rfmData %>% 
    rowwise() %>% 
    mutate(MonetoryScore = individualScore(MonetoryPercentile),
           FrequencyScore = individualScore(FrequencyPercentile), 
           RecencyScore = individualScore(RecencyPercentile))
  
  rfmData <- mutate(rfmData,MonetoryWeightedScore = MonetoryScore*monetoryWeight,
                    FrequencyWeightedScore = FrequencyScore*frequencyWeight, 
                    RecencyWeightedScore = RecencyScore*recencyWeight )
  rfmData <- mutate(rfmData,
                    FinalScore = RecencyWeightedScore+
                      FrequencyWeightedScore+
                      MonetoryWeightedScore)
  
  rfmData <- mutate(rfmData,
                    FinalWeightedScore = FinalScore/(monetoryWeight+frequencyWeight+recencyWeight))
  
  rfmData <- mutate(rfmData,
                    FinalCustomerClass = paste("Class",floor(FinalWeightedScore),sep = "-"))
  
  hist(rfmData$FinalWeightedScore)
  rfmData
  
}

individualScore <- function (percentile)
{
  if(percentile>0.8)
  {
    5
  }
  else if(percentile>0.6)
  {
    4
  }
  else if(percentile>0.4)
  {
    3
  }
  else if(percentile>0.2)
  {
    2
  }
  else
  {
    1
  }
}


