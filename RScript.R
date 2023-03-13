
#Importing Data

library("readxl")
Data1 = read_excel("Excelusssy.xlsx", sheet="Problem")
Data2 = read_excel("Excelusssy.xlsx", sheet="Problem2")
Data3 = read_excel("Excelusssy.xlsx", sheet="Problem3")
Data4 = read_excel("Excelusssy.xlsx", sheet="Problem4")
Data5 = read_excel("Excelusssy.xlsx", sheet="Problem5")
Data6 = read_excel("Excelusssy.xlsx", sheet="Problem6")
p=c(0.05,0.45,0.45,0.05)
ExpertsS=c()
for (i in seq(1,5)){
  ExpertsS[[i]]=c(0,0,0,0)
}






for (j in seq(1,5)){
  k=paste("Data",as.character(j),sep='')
  Data = get(k)

  for (i in seq(1,5)){
    if (Data$Data[1] < Data[[i+1]][1]){
      ExpertsS[[i]]=ExpertsS[[i]]+c(1,0,0,0)
    }
    if (Data$Data[1] < Data[[i+1]][2] & Data$Data[1] >= Data[[i+1]][1]){
      ExpertsS[[i]]=ExpertsS[[i]]+c(0,1,0,0)
    }
    if (Data$Data[1] <= Data[[i+1]][3] & Data$Data[1] >= Data[[i+1]][2]){
      ExpertsS[[i]]=ExpertsS[[i]]+c(0,0,1,0)
    }
    if (Data$Data[1] > Data[[i+1]][3]){
      ExpertsS[[i]]=ExpertsS[[i]]+c(0,0,0,1)
    }
    

  }
}




for (i in seq(1,5)){
  ExpertsS[[i]]= ExpertsS[[i]]/5
}




I=function(i,p){
  Rel=0
  for (k in seq(1,4)){
    if (ExpertsS[[i]][k] !=0){
      Rel=Rel+(ExpertsS[[i]][k]*log(ExpertsS[[i]][k]/p[k]))
    }
  }
  return(Rel)
}
Cal=function(RelInformation){
  1-pchisq(2*5*RelInformation,df=3)
} 

CalibrationScores=c()
for (i in seq(1,5)){
  CalibrationScores[i]=Cal(I(i,p))
}
