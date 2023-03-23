
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

l1 = 400-0.1*(10000-400)
u1 = 10000+0.1*(10000-400)

L=c(1-0.1*199,0.00044-0.1*(100-0.00044),300000-0.1*(60000000-300000),10-0.1*(1000-10),0.1-0.1*(1000-0.1),l1)
U=c(200+0.1*199,100+0.1*(100-0.00044),60000000+0.1*(60000000-300000),1000+0.1*(1000-10),1000+0.1*(1000-0.1),u1)


InformationScore = function(j){
  r = c()
  Information=0
  for(i in seq(1,6)){
    k=paste("Data",as.character(i),sep='')
    Data=get(k)
    r[[i]]=c((Data[[j+1]][1]-L[i])/(U[i]-L[i]),(Data[[j+1]][2]-Data[[j+1]][1])/(U[i]-L[i]),(Data[[j+1]][3]-Data[[j+1]][2])/(U[i]-L[i]),(U[i]-Data[[j+1]][3])/(U[i]-L[i]))
  }
  for (l in seq(1,6)){
    for (i in seq(1,4)){
      Information = Information + p[i]*log(p[i]/r[[l]][i])
    
    
    }  
  }
  x=c()
  x=Information/6
  y=r
  return(c(x,y))
}
InformationScores = c()
for (i in seq(1,5)){
  InformationScores=append(InformationScores,InformationScore(i)[1])
}



#Need to make the cdf's of 5 experts for 5 questions
CustomCDF = function(Expert,x,question){
#On basis of the quantiles, make a cdf, based on linear interpolation between the quantiles
  
  k=paste("Data",as.character(question),sep='')
  Data = get(k)
  
  Quantiles=c()
  Quantiles[1]=L[question]
  Quantiles[c(2,3,4)] = Data[[Expert+1]]
  Quantiles[5]=U[question]
  
  

  if(x<=Quantiles[1]){
    return(0)
  }
  if(x<=Quantiles[2] & x>Quantiles[1]){
    rc = (0.05)/(Quantiles[2] - Quantiles[1])
    y = (x-Quantiles[1])*rc+0
    return(y)
  }
  if(x<=Quantiles[3] & x>Quantiles[2]){
    rc = (0.45)/(Quantiles[3] - Quantiles[2])
    y = (x-Quantiles[2])*rc+0.05
    return(y)
  }
  if(x<=Quantiles[4] & x>Quantiles[3]){
    rc = (0.45)/(Quantiles[4] - Quantiles[3])
    y = (x-Quantiles[3])*rc+0.5
    return(y)
  }
  if(x<=Quantiles[5] & x>Quantiles[4]){
    rc = (0.05)/(Quantiles[5] - Quantiles[4])
    y = (x-Quantiles[4])*rc+0.95
    return(y)
  }
  if(x>=Quantiles[5]){
    return(1)
  }
}


PlotExpert = function(e,color,question,reset=FALSE){
  x=seq(L[question],U[question],(U[question]-L[question])/10000)
y=c()
for(i in seq(length(x))){
  y[i] = CustomCDF(e,x[i],question)
}
if(reset==TRUE){
  plot(x,y,type='l', col=color)
}
else{
  lines(x,y,type='l', col=color)
}
}
que = 1
PlotExpert(1,'black',que,TRUE)
PlotExpert(2,'orange',que)
PlotExpert(3,'red',que)
PlotExpert(4,'blue',que)
PlotExpert(5,'green',que)
legend(x="bottomright",
       legend = c("Expert1","Expert2","Expert3","Expert4","Expert5"),
       lty = c(1,1,1,1,1),
       col = c('black', 'orange', 'red', 'blue', 'green'),
       lwd = 2
)

WeightAlpha = function(e, alpha){
  if(CalibrationScores[e]>=alpha){
    wa = CalibrationScores[e] * InformationScores[[e]]
    return(wa)
  }
  return(0)
}


DecisionMaker = function(alpha,x,question){
  teller = 0
  noemer = 0
  for(i in seq(1,5)){
    teller = teller + WeightAlpha(i,alpha) * CustomCDF(i,x,question)
    noemer = noemer + WeightAlpha(i,alpha)
  }
  return(teller/noemer)
}
PlotDM = function(x,y){
  lines(x, y, type='l', col = 'purple',lwd=2)
}
ListDecisionMaker = function(alpha,question,plot=FALSE){
  y = c()
  x=seq(L[question],U[question],(U[question]-L[question])/10000)
  for(i in seq(length(x))){
    y[i] = DecisionMaker(alpha,x[i],question)
  }
  if(plot==TRUE){
    PlotDM(x,y)
  }
  return(y)
}

y = ListDecisionMaker(0,6,TRUE)
legend(x="bottomright",
       legend = c("Expert1","Expert2","Expert3","Expert4","Expert5","DecisionMaker"),
       lty = c(1,1,1,1,1,1),
       col = c('black', 'orange', 'red', 'blue', 'green','purple'),
       lwd = 2
)


QuantileCalculator = function(q,y,question){
  x=seq(L[question],U[question],(U[question]-L[question])/10000)
  i = 1
  while(y[i]<q & i < length(y)){
    i = i + 1
  }
  return(x[i])
}
Quantiles=function(y,question){
  q = c()
  q[1] = QuantileCalculator(0.05,y,question)
  q[2] = QuantileCalculator(0.5,y,question)
  q[3] = QuantileCalculator(0.95,y,question)
  return(q)
}

Quant = Quantiles(y,6)


#Calculating Decision Maker hokjes

CDMH = function(alpha){
  temp = c(0,0,0,0)
for (question in seq(1,5)){
  k=paste("Data",as.character(question),sep='')
  Data = get(k)
  temp2 = ListDecisionMaker(alpha,question)

    if (Data$Data[1] < QuantileCalculator(0.05,temp2,question)){
      temp=temp+c(1,0,0,0)

    }
    if (Data$Data[1] < QuantileCalculator(0.5,temp2,question) & Data$Data[1] >= QuantileCalculator(0.05,temp2,question)){
      temp=temp+c(0,1,0,0)

    }
    if (Data$Data[1] <= QuantileCalculator(0.95,temp2,question) & Data$Data[1] >= QuantileCalculator(0.5,temp2,question)){
      temp=temp+c(0,0,1,0)

    }
    if (Data$Data[1] > QuantileCalculator(0.95,temp2,question)){
      temp=temp+c(0,0,0,1)


    
    }  
  print('gaat goed')
}
  print(temp)
  return(temp/5)
}



#DecisionMakerScores

DMInformationScore = function(Alpha){
  r = c()
  Information=0
  for(i in seq(1,6)){
  y = ListDecisionMaker(Alpha,i)
  r[[i]]=c((QuantileCalculator(0.05,y,i)-L[i])/(U[i]-L[i]),(QuantileCalculator(0.5,y,i)-QuantileCalculator(0.05,y,i))/(U[i]-L[i]),(QuantileCalculator(0.95,y,i)-QuantileCalculator(0.5,y,i))/(U[i]-L[i]),(U[i]-QuantileCalculator(0.95,y,i))/(U[i]-L[i]))
  }
  for (j in seq(1,6)){
    for (i in seq(1,4)){
      if(r[[j]][i]!=0){
      Information = Information + p[i]*log(p[i]/r[[j]][i])
      }
      
    } 
  }
  return(Information/6)
}

DMI=function(alpha){
  Hokjes = CDMH(alpha)
  Rel=0
  for (k in seq(1,4)){
    if (Hokjes[k] !=0){
      Rel=Rel+(Hokjes[k]*log(Hokjes[k]/p[k]))
    }
  }
  return(Rel)
}
DMCal=function(RelInformation){
  1-pchisq(2*5*RelInformation,df=3)
}

DMCalScore = function(alpha){
  return(DMCal(DMI(alpha)))
}

CalDM = DMCalScore(0.5)

InfoDM = DMInformationScore(0.3)



