rm(list=ls())
# READ BODY SIZE AND DIET IN
setwd('C:/Users/a-pierov/Dropbox/FILES_SAPIENZA/GMA/')
data=read.csv('diet bs dens CarnUngu Upd.csv')
#write.csv(data,'diet bs dens CarnUngu Upd.csv',row.names=FALSE)
data=cbind(data,matrix(,nrow=nrow(data),ncol=3))
names_=names(data)
names_[12]="diet_final"
names_[13]="densitySilva97"
names_[14]="formula"
names(data)=c(names_)


nullvalue=data$DietUpdated[1]
data$diet_final=data$Diet
index=which(data$DietUpdated!=nullvalue)
data$diet_final[index]=data$DietUpdated[index]

# GET RID OF SCIENTIFIC NOTATION AND DECIMALS AFTER 2 FOR BODY SIZE IN GRAMS
for (i in 1:nrow(data)){
  data$BS_g[i]=format(round(as.numeric(data$BS_g[i]),2),nsmall=2,scientific=FALSE)
}

# GET RID OF SCIENTIFIC NOTATION AND DECIMALS AFTER 5 FOR DENSITY IN ANIMALS/KMSQ
for (i in 1:nrow(data)){
  data$PD_km2[i]=format(round(as.numeric(data$PD_km2[i]),5),nsmall=5,scientific=FALSE)
}


data$BS_g=as.numeric( data$BS_g)


# APPLY SILVA 1997

for (i in 1:nrow(data)){
  if (is.na(data$BS_g[i])){ # if no body size info
    data$densitySilva97[i]=NA
  } else {
    if(data$diet_final[i]=='H'){
      data$densitySilva97[i]=10^(-0.66*log10(data$BS_g[i])+3.30)
      data$formula[i]='herbivore'
    }
    if(data$diet_final[i]=='O'){
      data$densitySilva97[i]=10^(-0.78*log10(data$BS_g[i])+3.45)
      data$formula[i]='omnivore'
    }
    if(data$diet_final[i]=='C'){
      data$densitySilva97[i]=10^(-1.02*log10(data$BS_g[i])+3.69)
      data$formula[i]='carnivore'
    } 
    if(data$diet_final[i]=='I'){
      data$densitySilva97[i]=10^(-0.69*log10(data$BS_g[i])+2.83)
      data$formula[i]='insectivore'
    } 
    
    if ((data$diet_final[i]==nullvalue)){# if no diet info 
      if (data$BS_g[i]<100) { # if generic <100 grams
        data$densitySilva97[i]=10^(0.434*log10(data$BS_g[i])+1.701)
        data$formula[i]='genericSmall'
      }
      if(data$BS_g[i]<100000 & data$BS_g[i]>100) { # if generic between 100 grams and 100 kg
        data$densitySilva97[i]=10^(-0.899*log10(data$BS_g[i])+3.985)
        data$formula[i]='genericmedium'
      }  
      if (data$BS_g[i]>100000) { # generic mammal >100 kg
        data$densitySilva97[i]=10^(-0.049*log10(data$BS_g[i])-0.451)
        data$formula[i]='genericlarge'
      } # end generic mammal 100
    } # end generic mammal
  } # end has got body size
} 

## GET RID OF SCIENTIFIC NOTATION AND DECIMALS AFTER 5 FOR DENSITY IN ANIMALS/KMSQ
for (i in 1:nrow(data)){
  data$densitySilva97[i]=format(round(as.numeric(data$densitySilva97[i]),5),nsmall=5,scientific=FALSE)
}


# OVERWRITE BASED ON PANTHERIA
density_RLI=matrix(nrow=nrow(data),ncol=3)
for (i in 1:nrow(data)){
  density_RLI[i,1]=data$tax_id[i]
  if (data$PD_km2[i]=='NA'){
    density_RLI[i,2]=data$densitySilva97[i]
    density_RLI[i,3]=data$formula[i]
  }  else{
    density_RLI[i,2]=data$PD_km2[i]
    density_RLI[i,3]="Pantheria"
  }
}


density_RLI=as.data.frame(density_RLI)
names(density_RLI)=c('taxid','density','source')

# PASTE IT ALL TOGETHER
density_RLI=cbind(data,density_RLI[,c(2,3)])


density_upd_piero=read.csv('C:/Users/a-pierov/Dropbox/FILES_SAPIENZA/GMA/DensityUpdatedPiero.csv')
density_upd_piero$source=as.character(levels(density_upd_piero$source))[density_upd_piero$source] 

density_RLI$density=as.numeric(levels(density_RLI$density))[density_RLI$density] 
density_RLI$source=as.character(levels(density_RLI$source))[density_RLI$source] 

for (i in 1:nrow(density_upd_piero)){
  ind=which(data[,1]==density_upd_piero[i,1])
  density_RLI$density[ind]=density_upd_piero$density[i]
  density_RLI[ind,16]=density_upd_piero$source[i]
}

write.csv(density_RLI,'density_RLI.csv',row.names=FALSE,col.names=TRUE)


### filter where we have Pantheria
ind=density_RLI$PD_km2!='NA'
meanobs=mean(as.numeric(density_RLI$PD_km2[ind]))
obs=as.numeric(density_RLI$PD_km2[ind])
mod=as.numeric(density_RLI$densitySilva97[ind])
r2=sum((mod-meanobs)^2)/sum((obs-meanobs)^2) #0.21