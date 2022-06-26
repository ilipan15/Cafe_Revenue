### --- 1.a Daily Revenue FCFS Policy 
mL=50 # Mean Demand for Croissant, Poisson
mH=20 # Mean Demand for Sandwiches , Poisson
pL=1 # Price for Croissant
pH=1.5 # Price for Sandwich
capacity=50 # Capacity
ExpRevenue=rep(0,capacity+1)
for (i in 1:1){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:100){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:100){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pL*soldLowFare+pH*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+RevenueThisIter*dpois(dL,mL)*dpois(dH,mH)
    }
  }
}
RevenueFCFS=ExpRevenue[1]
print(paste("Lower Bound for Expected Revenue (FCFS):", round(RevenueFCFS,1)))




### --- 1.b Optimal protection Level
mL=50          # Mean Demand for Croissant, Poisson
mH=20           # Mean Demand for Sandwiches, Poisson
pL=1           # Price for Croissant
pH=1.5          # Price for Sandwiches
capacity=50   # Capacity 
ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pL*soldLowFare+pH*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mL)*dpois(dH,mH)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest=Protectindexbest-1
OptimalExpRevenue=max(ExpRevenue)
print(paste("The Optimal Protection Level for Sandwiches Demand:", ProtectBest))


### --- 1.c
print(paste("The Optimal Expected Revenue for Sandwiches Demand:", OptimalExpRevenue))


### --- 1.d

# Higher demand for Sandwiches 

mL=50          # Mean Demand for Croissant, Poisson
mH=40           # Mean Demand for Sandwiches, Poisson
pL=1           # Price for Croissant
pH=1.5          # Price for Sandwich 
capacity=50   # Capacity 
ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pL*soldLowFare+pH*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mL)*dpois(dH,mH)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest=Protectindexbest-1
OptimalExpRevenue=max(ExpRevenue)
print(paste("The Optimal Protection Level for High-Fare Demand:", ProtectBest))
print(paste("The Expected Revenue Level for Increased Sandwiches Demand:", OptimalExpRevenue))


### --- Higher price for Croissant 
mL=50          # Mean Demand for Croissant, Poisson
mH=20           # Mean Demand for Sandwiches, Poisson
pL=1.5           # Price for Croissant
pH=1.5          # Price for Sandwich 
capacity=50   # Capacity 
ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pL*soldLowFare+pH*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mL)*dpois(dH,mH)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest=Protectindexbest-1
OptimalExpRevenue=max(ExpRevenue)
print(paste("The Optimal Protection Level for Sandwiches Demand:", ProtectBest))
print(paste("The Expected Revenue Level for incresed price of Croissant:", OptimalExpRevenue))


### --- Higher Price for Sandwiches 

mL=50          # Mean Demand for Croissant, Poisson
mH=20           # Mean Demand for Sandwiches, Poisson
pL=1           # Price for Croissant
pH=2          # Price for Sandwiches
capacity=50   # Capacity 
ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pL*soldLowFare+pH*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mL)*dpois(dH,mH)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest=Protectindexbest-1
OptimalExpRevenue=max(ExpRevenue)
print(paste("The Optimal Protection Level for Sandwiches Demand:", ProtectBest))

print(paste("The Expected Revenue Level for incresed price of Sandwiches:", OptimalExpRevenue))

### --- Change in the capacity of croissant the cafe receives

mL=50          # Mean Demand for Croissant, Poisson
mH=20           # Mean Demand for Sandwiches, Poisson
pL=1           # Price for Croissant
pH=1.5          # Price for Sandwiches
capacity=70   # Capacity 
ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pL*soldLowFare+pH*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mL)*dpois(dH,mH)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest=Protectindexbest-1
OptimalExpRevenue=max(ExpRevenue)
print(paste("The Optimal Protection Level for Sandwiches Demand:", ProtectBest))
print(paste("The Optimal Expected Revenue for increased capacity:", OptimalExpRevenue))



### --- Change in every variable 

mL=50          # Mean Demand for Croissant, Poisson
mH=40           # Mean Demand for Sandwiches, Poisson
pL=1.5           # Price for Croissant
pH=2          # Price for Sandwiches
capacity=90   # Capacity 
ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pL*soldLowFare+pH*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mL)*dpois(dH,mH)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest=Protectindexbest-1
OptimalExpRevenue=max(ExpRevenue)
print(paste("The Optimal Protection Level for Sandwiches Demand:", ProtectBest))
print(paste("The Optimal Expected Revenue for increased prices,capacity and demand of Sandwiches:", OptimalExpRevenue))
