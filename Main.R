#library(ggplot2)
#library(tidyverse)
#library(zoo)
#pd <- import('pandas')


dataFramesConversion <- function(index){
  df <- data.frame()
  start=3+14*(index -1)
  end=16+14*(index -1)
  
  for(i in 1:12)
    df <- rbind(df,data.frame(matrix(unlist(AAAdata[(start+14*10*(i-1)):(end+14*10*(i-1))]),nrow = 17)))
  
  return(df)
}


voivodeshipYearAverage <- function(Frame){
  means <- data.frame()
  for(j in 1:17){
    numbers <- list(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    for(i in 1:12){
      numbers <- mapply("+", numbers, Frame[j+17*(i-1),] , SIMPLIFY = FALSE)
    }
    #  print(data.frame(matrix(unlist(numbers),nrow =1)))
    means <- rbind(means,data.frame(matrix(unlist(numbers),nrow =1)))
  }
  
  return (list(unlist(numbers)/12))
}


#View(data)
#name(data)

#df <- data.frame(matrix(unlist(data[3:16]),nrow = 17))
#view(df)

AAAdata<-(read.csv(file = ".\\CENY_2917_CTAB_20200213132521.csv",header = TRUE,sep = ';', dec = ',', stringsAsFactors = FALSE, encoding = 'UTF-8'))
  voivodeships <- c("Polska","Dolnoslaskie","Kujawsko-Pomorskie","Lubelskie","Lubuskie","Lodzkie","Malopolskie",
                    "Mazowieckie","Opolskie","Podkarpackie","Podlaskie","Pomorskie","Slaskie","Swietokrzyskie",
                    "Warminsko-Mazurskie","Wielkopolskie","Zachodniopomorskie")
  years <- c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
  
  Ryz <- dataFramesConversion(1)
  RyzAvg <- voivodeshipYearAverage(Ryz)
  
  Rostbef <- dataFramesConversion(2)
  RostbefAvg <- voivodeshipYearAverage(Rostbef)
  
  Szynka <- dataFramesConversion(3)
  SzynkaAvg <- voivodeshipYearAverage(Szynka)
  
  Mleko <- dataFramesConversion(4)
  MlekoAvg <- voivodeshipYearAverage(Mleko)
  
  Jaja <- dataFramesConversion(5)
  JajaAvg <- voivodeshipYearAverage(Jaja)
  
  Garnitur <- dataFramesConversion(6)
  GarniturAvg <- voivodeshipYearAverage(Garnitur)
  
  Rajstopy <- dataFramesConversion(7)
  RajstopyAvg <- voivodeshipYearAverage(Rajstopy)
  
  Spodnie <- dataFramesConversion(8)
  SpodnieAvg <- voivodeshipYearAverage(Spodnie)
  
  podzelowanieObuwia <- dataFramesConversion(9)
  podzelowanieObuwiaAvg <- voivodeshipYearAverage(podzelowanieObuwia)
  
  Pasta <- dataFramesConversion(10)
  PastaAvg <- voivodeshipYearAverage(Pasta)
  

