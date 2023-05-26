a12coeff<-function(sizes,plotting=TRUE)
{
#sizes=c(50,25,10,2,1)
#jitter to avoif equality of sizes
# resclae need to be fixed
  

#sizes=jitter(sizes)
logSizes<-sort(log(sizes,10),TRUE)
logRanks<-log(sort(rank(-logSizes,ties.method="random"),FALSE),10)
expectedSizes<-numeric(length=length(sizes))
for (x in 2:length(sizes)){expectedSizes[x]= max(sizes)/x}
expectedSizes[1]=max(sizes)
LogExp<-log(expectedSizes,10)
minS=min(c(logSizes,LogExp))


#actual computation

#STEP 1: rescale so that square edge is sqrt(2)
#Rescale should range with max=sqrt(2) min=

newMax=sqrt(2)
newMin=0
scaleR=(max(logRanks)-min(logRanks))/(sqrt(2)-0)
RescaledRank=(logRanks-min(logRanks))/scaleR
scale=(max(LogExp)-min(LogExp))/(sqrt(2)-0)
RescaledExpSizes=(LogExp-min(LogExp))/scale
RescaledObSizes=(logSizes-min(LogExp))/scale


#there are observed sizes below expected:
if(min(logSizes)<min(LogExp))
  {
newMin=(min(LogExp)-min(logSizes))/(max(LogExp)-min(LogExp))*-sqrt(2)+100
newMax=sqrt(2)+100
scale=(max(logSizes)-min(logSizes))/(newMax-newMin)
RescaledExpSizes=(newMin+(LogExp-min(logSizes))/scale)-100
RescaledObSizes=(newMin+(logSizes-min(logSizes))/scale)-100
  }













#check plot:
if(plotting==TRUE)
  {
plot(logRanks,logSizes,type="l",ylim=c(minS,max(logSizes)),xlab="rank",ylab="size")
lines(logRanks, LogExp,lty=2)
abline(h=min(LogExp))
}

#STEP 2
#Compute the actual A1 and A2 Coefficients using the Trapezoid Rule
A1=0
A2=0

for (x in 2:(length(sizes)))
  {
   



    #If the Observed line is above the expected in the entire section:
    #In this case Observed Area is larger than than the expected one.
      if (RescaledObSizes[x]>=RescaledExpSizes[x]&RescaledObSizes[x-1]>=RescaledExpSizes[x-1])
        {
          h<-abs(RescaledRank[x]-RescaledRank[x-1])
          bE<-RescaledExpSizes[x]+RescaledExpSizes[x-1]
          bO<-RescaledObSizes[x]+RescaledObSizes[x-1]
          obsArea<-bO/2*h
          expArea<-bE/2*h
          tmp<-obsArea-expArea

          #In this case Observed Area is larger than than the expected one.
          A1=A1+tmp
        }


      #If the Observed line is above 0 in the entire section, and below the expected for the entire section:
      if (RescaledObSizes[x]<=RescaledExpSizes[x]&RescaledObSizes[x]>=0&RescaledObSizes[x-1]<=RescaledExpSizes[x-1]&RescaledObSizes[x-1]>=0)
        {

          h<-abs(RescaledRank[x]-RescaledRank[x-1])
          bE<-RescaledExpSizes[x]+RescaledExpSizes[x-1]
          bO<-RescaledObSizes[x]+RescaledObSizes[x-1]
          obsArea<-bO/2*h
          expArea<-bE/2*h
          tmp<-expArea-obsArea
          #In this case Observed Area is smaller than than the expected one
          A2=A2+tmp
        }

      


   #If the portion is above 0 but below the expectation in one of the two parts (intersection)

      if((RescaledObSizes[x]>0&RescaledObSizes[x-1]>0)&xor(RescaledObSizes[x]<RescaledExpSizes[x],RescaledObSizes[x-1]<RescaledExpSizes[x-1]))

      {

        #calculate the intersection point first:
        x1=RescaledRank[x-1]
        x2=RescaledRank[x]
        x3=x1
        x4=x2
        y1=RescaledExpSizes[x-1]
        y2=RescaledExpSizes[x]
        y3=RescaledObSizes[x-1]
        y4=RescaledObSizes[x]
        
        intersectionPointX<-((x1*y2-y1*x2)*(x3-x4)-(x1-x2)*(x3*y4-y3*x4))/((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))
        intersectionPointY<-((x1*y2-y1*x2)*(y3-y4)-(y1-y2)*(x3*y4-y3*x4))/((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))  

        #calculate area before intersection (above 0 so positive values)
         h<-abs(intersectionPointX-RescaledRank[x-1])
         bE<-abs(intersectionPointY+RescaledExpSizes[x-1])
         bO<-abs(intersectionPointY+RescaledObSizes[x-1])
         obsArea<-bO/2*h
         expArea<-bE/2*h

         tmp<-obsArea-expArea

         #In this case Observed Area is larger than than the expected one.
        if (tmp<0)
          {A2=A2+abs(tmp)}
        if (tmp>0)
          {A1=A1+abs(tmp)}
        
          
        

        #calculate area after intersection
         h<-abs(intersectionPointX-RescaledRank[x])
         bE<-intersectionPointY+RescaledExpSizes[x]
         bO<-intersectionPointY+RescaledObSizes[x]
         obsArea<-bO/2*h
         expArea<-bE/2*h
         tmp<-expArea-obsArea
         #In this case Observed Area is smaller than than the expected one
         if (tmp>0)
          {A2=A2+abs(tmp)}
        if (tmp<0)
          {A1=A1+abs(tmp)}
      }
    

 #If the portion is below 0 with no intersection (all Ys are negative in this case)

    if (RescaledObSizes[x]<0&RescaledObSizes[x-1]<0)
      {
        
         h<-abs(RescaledRank[x]-RescaledRank[x-1])
         bE<-RescaledExpSizes[x]+RescaledExpSizes[x-1]
         bO<-(abs(RescaledObSizes[x])+abs(RescaledObSizes[x-1]))
         obsArea<-bO/2*h
         expArea<-bE/2*h
         tmp<-obsArea+expArea
         A2=A2+tmp
      }


#If the portion is partially below 0 with no intersection to expected line
      if(xor(RescaledObSizes[x]<0,RescaledObSizes[x-1]<0)&(RescaledObSizes[x-1]<RescaledExpSizes[x-1])&(RescaledObSizes[x]<RescaledExpSizes[x]))
      {
        
        #calculate the intersection point first:
        x1=RescaledRank[x-1]
        x2=RescaledRank[x]
        x3=x1
        x4=x2
        y1=0
        y2=0
        y3=RescaledObSizes[x-1]
        y4=RescaledObSizes[x]
 
        intersectionPointX<-((x1*y2-y1*x2)*(x3-x4)-(x1-x2)*(x3*y4-y3*x4))/((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))
        intersectionPointY1<-0


        x1=RescaledRank[x-1]
        x2=RescaledRank[x]
        x3=intersectionPointX
        x4=intersectionPointX
        y1=RescaledExpSizes[x-1]
        y2=RescaledExpSizes[x]
        y3=max(RescaledExpSizes)
        y4=min(RescaledExpSizes)

        intersectionPointY2<-((x1*y2-y1*x2)*(y3-y4)-(y1-y2)*(x3*y4-y3*x4))/((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4)) 

        
        #Calculate Area before intersection (this has all positive Ys)
        #This is based on difference of the two areas:

         h<-abs(intersectionPointX-RescaledRank[x-1])
         bE<-intersectionPointY2+RescaledExpSizes[x-1]
         bO<-RescaledObSizes[x-1]
         obsArea<-(bO/2*h)
         expArea<-(bE/2*h)
         tmp<-expArea-obsArea
         A2=A2+tmp
        
        #Calculate Area after intersection
        #This is based on the sum of the two areas:

         h<-abs(intersectionPointX-RescaledRank[x])
         bE<-intersectionPointY2+RescaledExpSizes[x]
         bO<-abs(RescaledObSizes[x])
         obsArea<-(bO/2*h)
         expArea<-(bE/2*h)
         tmp<-obsArea+expArea
         A2=A2+tmp
     }


#If the portion is partially below 0 with intersection on the expected line     
if((RescaledObSizes[x-1]>RescaledExpSizes[x-1])&(RescaledObSizes[x]<0))
  {
    #two intersection points to calculate:
    #First intersection expected Line vs Observed Line

        x1=RescaledRank[x-1]
        x2=RescaledRank[x]
        x3=x1
        x4=x2
        y1=RescaledExpSizes[x-1]
        y2=RescaledExpSizes[x]
        y3=RescaledObSizes[x-1]
        y4=RescaledObSizes[x]
        
        intersectionPointX0<-((x1*y2-y1*x2)*(x3-x4)-(x1-x2)*(x3*y4-y3*x4))/((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))
        intersectionPointY0<-((x1*y2-y1*x2)*(y3-y4)-(y1-y2)*(x3*y4-y3*x4))/((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))

        #points(intersectionPointX0,intersectionPointY0)

   #second intersections observed line vs 0-line
        
        x1=RescaledRank[x-1]
        x2=RescaledRank[x]
        x3=x1
        x4=x2
        y1=0
        y2=0
        y3=RescaledObSizes[x-1]
        y4=RescaledObSizes[x]
 
        intersectionPointX1<-((x1*y2-y1*x2)*(x3-x4)-(x1-x2)*(x3*y4-y3*x4))/((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))
        intersectionPointY1<-0


        x1=RescaledRank[x-1]
        x2=RescaledRank[x]
        x3=intersectionPointX1
        x4=intersectionPointX1
        y1=RescaledExpSizes[x-1]
        y2=RescaledExpSizes[x]
        y3=max(RescaledExpSizes)
        y4=min(RescaledExpSizes)

        intersectionPointY2<-((x1*y2-y1*x2)*(y3-y4)-(y1-y2)*(x3*y4-y3*x4))/((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4)) 

       

#Calculate area 1 of 3 [above expected line]

         h<-abs(intersectionPointX0-RescaledRank[x-1])
         bE<-intersectionPointY0+RescaledExpSizes[x-1]
         bO<-intersectionPointY0+RescaledObSizes[x-1]
         obsArea<-bO/2*h
         expArea<-bE/2*h
         tmp<-obsArea-expArea
         A1=A1+tmp

        
#Calculate area 2 of 3 [below expected line,above 0]
         h<-abs(intersectionPointX0-intersectionPointX1)
         bE<-intersectionPointY0+intersectionPointY2
         bO<-intersectionPointY0
         obsArea<-bO/2*h
         expArea<-bE/2*h
         tmp=expArea-obsArea
         A2=A2+tmp
        
#Calculate area 3 of 3 [below expected line,below 0] 
        
         h<-abs(intersectionPointX1-RescaledRank[x])
         bE<-intersectionPointY2+RescaledExpSizes[x]
         bO<-abs(RescaledObSizes[x])
         obsArea<-(bO/2*h)
         expArea<-(bE/2*h)
         tmp<-obsArea+expArea
         A2=A2+tmp
        
  }
if(A2<0)
  {
    print(x)
  }

if (A1>1)
  {
    print(x)
  }
    }
      return(list(A1=A1,A2=A2))
    
  }












