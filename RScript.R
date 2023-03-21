
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

L=c(1-0.1*199,0.00044-0.1*(100-0.00044),300000-0.1*(60000000-300000),10-0.1*(1000-10),0.1-0.1*(1000-0.1))
U=c(200+0.1*199,100+0.1*(100-0.00044),60000000+0.1*(60000000-300000),1000+0.1*(1000-10),1000+0.1*(1000-0.1))


InformationScore = function(j){
  r = c()
  Information=0
  for(i in seq(1,5)){
    k=paste("Data",as.character(i),sep='')
    Data=get(k)
    r[[i]]=c((Data[[j+1]][1]-L[i])/(U[i]-L[i]),(Data[[j+1]][2]-Data[[j+1]][1])/(U[i]-L[i]),(Data[[j+1]][3]-Data[[j+1]][2])/(U[i]-L[i]),(U[i]-Data[[j+1]][3])/(U[i]-L[i]))
  }
  for (l in seq(1,5)){
    for (i in seq(1,4)){
      Information = Information + p[i]*log(p[i]/r[[l]][i])
    
    
    }  
  }
  x=c()
  x=Information/5
  y=r
  return(c(x,y))
}
InformationScores = c()
for (i in seq(1,5)){
  InformationScores=append(InformationScores,InformationScore(i)[1])
}


#Need to make the cdf's of 5 experts for 5 questions
CustomCDF = function(Expert,Question){
#Heb half uur geprobeerd maar niks werkte :(
#We moeten voor elke expert een soort eigen cdf maken op basis van hun quantiles, hebben we nodig voor de decision maker
}



DecisionMaker = function(alpha){
  
}

