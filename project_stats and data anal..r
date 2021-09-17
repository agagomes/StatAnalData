library(tidyverse)
library(tidycomm)
library(readr)
library(ggplot2)
library(car)
library(corrplot)
library(psych)
library("FactoMineR")
library("factoextra")
library("ggpubr")
mydata<- read.csv("F:/FACULDADE/2semestre/stats e analise de dados/countries of the world.csv")
str(mydata)

#TRANSFORM CHAR VARIABLES INTO NUM VARIABLES
mydata$Pop..Density..per.sq..mi..<-as.numeric(sub(",", ".", mydata$Pop..Density..per.sq..mi.. , fixed = TRUE))
mydata$Coastline..coast.area.ratio.<-as.numeric(sub(",", ".", mydata$Coastline..coast.area.ratio. , fixed = TRUE))
mydata$Net.migration<-as.numeric(sub(",", ".", mydata$Net.migration , fixed = TRUE))
mydata$Infant.mortality..per.1000.births.<-as.numeric(sub(",", ".", mydata$Infant.mortality..per.1000.births. , fixed = TRUE))
mydata$Literacy....<-as.numeric(sub(",", ".", mydata$Literacy.... , fixed = TRUE))
mydata$Phones..per.1000.<-as.numeric(sub(",", ".", mydata$Phones..per.1000. , fixed = TRUE))
mydata$Arable....<-as.numeric(sub(",", ".", mydata$Arable.... , fixed = TRUE))
mydata$Crops....<-as.numeric(sub(",", ".", mydata$Crops.... , fixed = TRUE))
mydata$Other....<-as.numeric(sub(",", ".", mydata$Other.... , fixed = TRUE))
mydata$Birthrate<-as.numeric(sub(",", ".", mydata$Birthrate , fixed = TRUE))
mydata$Deathrate<-as.numeric(sub(",", ".", mydata$Deathrate , fixed = TRUE))
mydata$Agriculture<-as.numeric(sub(",", ".", mydata$Agriculture , fixed = TRUE))
mydata$Industry<-as.numeric(sub(",", ".", mydata$Industry , fixed = TRUE))
mydata$Service<-as.numeric(sub(",", ".", mydata$Service , fixed = TRUE))

#CHANGE VARIABLE NAMES
mydata<-mydata %>% 
  rename(
    area_sq=Area..sq..mi..,
    pop_dens=Pop..Density..per.sq..mi..,
    coastline=Coastline..coast.area.ratio.,
    infant_mortal=Infant.mortality..per.1000.births.,
    GDP=GDP....per.capita.,
    literacy=Literacy....,
    phones_per1000=Phones..per.1000.,
    arable=Arable....,
    crops=Crops....,
    other=Other....,
  )

mydata<-select(mydata, -Climate)
# UNIVARIATE ANALYSES
mydata %>% describe()
mydata %>% tab_frequencies(Region)

normalpop<- filter(mydata, mydata$Population<200000000)

hist(mydata$Population, breaks =20, xlab = "Population")
hist(normalpop$Population, xlab = "Population")
hist(mydata$area_sq,breaks = 20, xlab = "Area")
hist(mydata$pop_dens,breaks = 20, xlab = "Population/km2")
hist(mydata$coastline,breaks = 20, xlab = "Coastline ratio")
hist(mydata$Net.migration,breaks = 20, xlab = "Migration")
hist(mydata$infant_mortal,breaks = 20, xlab = "infant mortality")
hist(mydata$GDP,breaks = 20, xlab = "GDP")
hist(mydata$literacy,breaks = 20, xlab = "Literacy")
hist(mydata$phones_per1000,breaks = 20, xlab = "Phones")
hist(mydata$arable,breaks = 20, xlab = "Arable")
hist(mydata$crops,breaks = 20, xlab = "Crops")
hist(mydata$other,breaks = 20, xlab = "other")
hist(mydata$Birthrate,breaks = 20, xlab = "Birthrate")
hist(mydata$Deathrate,breaks = 20, xlab = "Deathrate")
hist(mydata$Agriculture,breaks = 20, xlab = "Agriculture")
hist(mydata$Industry,breaks = 20, xlab = "Industry")
hist(mydata$Service,breaks = 20, xlab = "Service")

barplot(table(mydata$Region), names.arg=c("AS","BA","CWIS","EE","LA&C","NE","NAf","NAm","OC","SSA","WE"))




# BIVARIATE ANALYSES




    #correlações entre tdas as variaveis numericas
cor(mydata$Population, mydata$area_sq,  method = "pearson", use = "complete.obs") # 0.47
cor(mydata$Population, mydata$pop_dens,  method = "pearson", use = "complete.obs") #-0.03
cor(mydata$Population, mydata$coastline,  method = "pearson", use = "complete.obs") #-0.07
cor(mydata$Population, mydata$Net.migration,  method = "pearson", use = "complete.obs") #0
cor(mydata$Population, mydata$infant_mortal,  method = "pearson", use = "complete.obs") #0.02
cor(mydata$Population, mydata$GDP,  method = "pearson", use = "complete.obs") #-0.04
cor(mydata$Population, mydata$literacy,  method = "pearson", use = "complete.obs") #-0.04
cor(mydata$Population, mydata$phones_per1000,  method = "pearson", use = "complete.obs") #-0.03
cor(mydata$Population, mydata$arable,  method = "pearson", use = "complete.obs") #0.19
cor(mydata$Population, mydata$crops,  method = "pearson", use = "complete.obs") #-0.06
cor(mydata$Population, mydata$other,  method = "pearson", use = "complete.obs") #-0.12
cor(mydata$Population, mydata$Birthrate,  method = "pearson", use = "complete.obs") #-0.05
cor(mydata$Population, mydata$Deathrate,  method = "pearson", use = "complete.obs") #-0.02
cor(mydata$Population, mydata$Agriculture,  method = "pearson", use = "complete.obs") #0
cor(mydata$Population, mydata$Industry,  method = "pearson", use = "complete.obs") #0.1
cor(mydata$Population, mydata$Service,  method = "pearson", use = "complete.obs") #-0.08

cor(mydata$area_sq, mydata$pop_dens,  method = "pearson", use = "complete.obs") #-0.07
cor(mydata$area_sq, mydata$coastline,  method = "pearson", use = "complete.obs") #-0.1
cor(mydata$area_sq, mydata$Net.migration,  method = "pearson", use = "complete.obs")#0.05 
cor(mydata$area_sq, mydata$infant_mortal,  method = "pearson", use = "complete.obs") #0
cor(mydata$area_sq, mydata$GDP,  method = "pearson", use = "complete.obs") #0.07
cor(mydata$area_sq, mydata$literacy,  method = "pearson", use = "complete.obs") #0.04
cor(mydata$area_sq, mydata$phones_per1000,  method = "pearson", use = "complete.obs") # 0.05
cor(mydata$area_sq, mydata$arable,  method = "pearson", use = "complete.obs") #-0.08
cor(mydata$area_sq, mydata$crops,  method = "pearson", use = "complete.obs") #-0.14
cor(mydata$area_sq, mydata$other,  method = "pearson", use = "complete.obs") #0.14
cor(mydata$area_sq, mydata$Birthrate,  method = "pearson", use = "complete.obs") #-0.07
cor(mydata$area_sq, mydata$Deathrate,  method = "pearson", use = "complete.obs") #0.04
cor(mydata$area_sq, mydata$Agriculture,  method = "pearson", use = "complete.obs") #-0.05
cor(mydata$area_sq, mydata$Industry,  method = "pearson", use = "complete.obs") #0.12
cor(mydata$area_sq, mydata$Service,  method = "pearson", use = "complete.obs") #-0.05

cor(mydata$pop_dens, mydata$coastline,  method = "pearson", use = "complete.obs") #0.24
cor(mydata$pop_dens, mydata$Net.migration,  method = "pearson", use = "complete.obs") #0.18
cor(mydata$pop_dens, mydata$infant_mortal,  method = "pearson", use = "complete.obs") #-0.14
cor(mydata$pop_dens, mydata$GDP,  method = "pearson", use = "complete.obs") #0.2
cor(mydata$pop_dens, mydata$literacy,  method = "pearson", use = "complete.obs") #0.1
cor(mydata$pop_dens, mydata$phones_per1000,  method = "pearson", use = "complete.obs") #0.28
cor(mydata$pop_dens, mydata$arable,  method = "pearson", use = "complete.obs") #-0.08
cor(mydata$pop_dens, mydata$crops,  method = "pearson", use = "complete.obs") #-0.031
cor(mydata$pop_dens, mydata$other,  method = "pearson", use = "complete.obs") #0.08
cor(mydata$pop_dens, mydata$Birthrate,  method = "pearson", use = "complete.obs") #-0.16
cor(mydata$pop_dens, mydata$Deathrate,  method = "pearson", use = "complete.obs") #-0.07
cor(mydata$pop_dens, mydata$Agriculture,  method = "pearson", use = "complete.obs") #-0.1
cor(mydata$pop_dens, mydata$Industry,  method = "pearson", use = "complete.obs") #-0.14
cor(mydata$pop_dens, mydata$Service,  method = "pearson", use = "complete.obs") #0.25

cor(mydata$coastline, mydata$Net.migration,  method = "pearson", use = "complete.obs") #-0.13
cor(mydata$coastline, mydata$infant_mortal,  method = "pearson", use = "complete.obs") #-0.14
cor(mydata$coastline, mydata$GDP,  method = "pearson", use = "complete.obs") #0.05
cor(mydata$coastline, mydata$literacy,  method = "pearson", use = "complete.obs") #0.12
cor(mydata$coastline, mydata$phones_per1000,  method = "pearson", use = "complete.obs") #0.15
cor(mydata$coastline, mydata$arable,  method = "pearson", use = "complete.obs") #-0.12
cor(mydata$coastline, mydata$crops,  method = "pearson", use = "complete.obs") #-0.34
cor(mydata$coastline, mydata$other,  method = "pearson", use = "complete.obs") #-0.08
cor(mydata$coastline, mydata$Birthrate,  method = "pearson", use = "complete.obs") #-0.08
cor(mydata$coastline, mydata$Deathrate,  method = "pearson", use = "complete.obs") #-0.16
cor(mydata$coastline, mydata$Agriculture,  method = "pearson", use = "complete.obs") #-0.03
cor(mydata$coastline, mydata$Industry,  method = "pearson", use = "complete.obs") #-0.2
cor(mydata$coastline, mydata$Service,  method = "pearson", use = "complete.obs") #0.2

cor(mydata$Net.migration, mydata$infant_mortal,  method = "pearson", use = "complete.obs") #-0.03
cor(mydata$Net.migration, mydata$GDP,  method = "pearson", use = "complete.obs") #0.38
cor(mydata$Net.migration, mydata$literacy,  method = "pearson", use = "complete.obs") #0
cor(mydata$Net.migration, mydata$phones_per1000,  method = "pearson", use = "complete.obs") #0.24
cor(mydata$Net.migration, mydata$arable,  method = "pearson", use = "complete.obs") #-0.06
cor(mydata$Net.migration, mydata$crops,  method = "pearson", use = "complete.obs") #-0.35
cor(mydata$Net.migration, mydata$other,  method = "pearson", use = "complete.obs") #0.23
cor(mydata$Net.migration, mydata$Birthrate,  method = "pearson", use = "complete.obs") #-0.07
cor(mydata$Net.migration, mydata$Deathrate,  method = "pearson", use = "complete.obs") #0.03
cor(mydata$Net.migration, mydata$Agriculture,  method = "pearson", use = "complete.obs") #-0.12
cor(mydata$Net.migration, mydata$Industry,  method = "pearson", use = "complete.obs") #-0.03
cor(mydata$Net.migration, mydata$Service,  method = "pearson", use = "complete.obs") #0.13

cor(mydata$infant_mortal, mydata$GDP,  method = "pearson", use = "complete.obs") #-0.6
cor(mydata$infant_mortal, mydata$literacy,  method = "pearson", use = "complete.obs") #-0.77
cor(mydata$infant_mortal, mydata$phones_per1000,  method = "pearson", use = "complete.obs") #-0.67
cor(mydata$infant_mortal, mydata$arable,  method = "pearson", use = "complete.obs") #-0.11
cor(mydata$infant_mortal, mydata$crops,  method = "pearson", use = "complete.obs") #-0.062
cor(mydata$infant_mortal, mydata$other,  method = "pearson", use = "complete.obs") #0.12
cor(mydata$infant_mortal, mydata$Birthrate,  method = "pearson", use = "complete.obs") #0.85
cor(mydata$infant_mortal, mydata$Deathrate,  method = "pearson", use = "complete.obs") #0.66
cor(mydata$infant_mortal, mydata$Agriculture,  method = "pearson", use = "complete.obs") #0.71
cor(mydata$infant_mortal, mydata$Industry,  method = "pearson", use = "complete.obs") #0
cor(mydata$infant_mortal, mydata$Service,  method = "pearson", use = "complete.obs") #-0.63

cor(mydata$GDP, mydata$literacy,  method = "pearson", use = "complete.obs") #0.51
cor(mydata$GDP, mydata$phones_per1000,  method = "pearson", use = "complete.obs") #0.83
cor(mydata$GDP, mydata$arable,  method = "pearson", use = "complete.obs") #0.02
cor(mydata$GDP, mydata$crops,  method = "pearson", use = "complete.obs") #-0.22
cor(mydata$GDP, mydata$other,  method = "pearson", use = "complete.obs") #0.01
cor(mydata$GDP, mydata$Birthrate,  method = "pearson", use = "complete.obs") #-0.65
cor(mydata$GDP, mydata$Deathrate,  method = "pearson", use = "complete.obs") #-0.2
cor(mydata$GDP, mydata$Agriculture,  method = "pearson", use = "complete.obs") #-0.59
cor(mydata$GDP, mydata$Industry,  method = "pearson", use = "complete.obs") #-0.03
cor(mydata$GDP, mydata$Service,  method = "pearson", use = "complete.obs") #0.55

cor(mydata$literacy, mydata$phones_per1000,  method = "pearson", use = "complete.obs") #0.6
cor(mydata$literacy, mydata$arable,  method = "pearson", use = "complete.obs") #0.11
cor(mydata$literacy, mydata$crops,  method = "pearson", use = "complete.obs") #0.04
cor(mydata$literacy, mydata$other,  method = "pearson", use = "complete.obs") #-0.11
cor(mydata$literacy, mydata$Birthrate,  method = "pearson", use = "complete.obs") #-0.79
cor(mydata$literacy, mydata$Deathrate,  method = "pearson", use = "complete.obs") #-0.39
cor(mydata$literacy, mydata$Agriculture,  method = "pearson", use = "complete.obs") #-0.61
cor(mydata$literacy, mydata$Industry,  method = "pearson", use = "complete.obs") #0.06
cor(mydata$literacy, mydata$Service,  method = "pearson", use = "complete.obs") #0.49

cor(mydata$phones_per1000, mydata$arable,  method = "pearson", use = "complete.obs") #0.06
cor(mydata$phones_per1000, mydata$crops,  method = "pearson", use = "complete.obs") #-0.15
cor(mydata$phones_per1000, mydata$other,  method = "pearson", use = "complete.obs") #0.03
cor(mydata$phones_per1000, mydata$Birthrate,  method = "pearson", use = "complete.obs") #-0.72
cor(mydata$phones_per1000, mydata$Deathrate,  method = "pearson", use = "complete.obs") #-0.26
cor(mydata$phones_per1000, mydata$Agriculture,  method = "pearson", use = "complete.obs") #-0.6
cor(mydata$phones_per1000, mydata$Industry,  method = "pearson", use = "complete.obs") #-0.15
cor(mydata$phones_per1000, mydata$Service,  method = "pearson", use = "complete.obs") #0.68

cor(mydata$arable, mydata$crops,  method = "pearson", use = "complete.obs") #0.1
cor(mydata$arable, mydata$other,  method = "pearson", use = "complete.obs") #-0.86
cor(mydata$arable, mydata$Birthrate,  method = "pearson", use = "complete.obs") #-0.19
cor(mydata$arable, mydata$Deathrate,  method = "pearson", use = "complete.obs") #0.05
cor(mydata$arable, mydata$Agriculture,  method = "pearson", use = "complete.obs") #-0.03
cor(mydata$arable, mydata$Industry,  method = "pearson", use = "complete.obs") #-0.06
cor(mydata$arable, mydata$Service,  method = "pearson", use = "complete.obs") #0.09

cor(mydata$crops, mydata$other,  method = "pearson", use = "complete.obs") #-0.59
cor(mydata$crops, mydata$Birthrate,  method = "pearson", use = "complete.obs") #0.12
cor(mydata$crops, mydata$Deathrate,  method = "pearson", use = "complete.obs") #-0.2
cor(mydata$crops, mydata$Agriculture,  method = "pearson", use = "complete.obs") #0.05
cor(mydata$crops, mydata$Industry,  method = "pearson", use = "complete.obs") #-0.11
cor(mydata$crops, mydata$Service,  method = "pearson", use = "complete.obs") #0.05

cor(mydata$other, mydata$Birthrate,  method = "pearson", use = "complete.obs") #0.09
cor(mydata$other, mydata$Deathrate,  method = "pearson", use = "complete.obs") #0.06
cor(mydata$other, mydata$Agriculture,  method = "pearson", use = "complete.obs") #0
cor(mydata$other, mydata$Industry,  method = "pearson", use = "complete.obs") #0.11
cor(mydata$other, mydata$Service,  method = "pearson", use = "complete.obs") #-0.1

cor(mydata$Birthrate, mydata$Deathrate,  method = "pearson", use = "complete.obs") #0.4
cor(mydata$Birthrate, mydata$Agriculture,  method = "pearson", use = "complete.obs") #0.68
cor(mydata$Birthrate, mydata$Industry,  method = "pearson", use = "complete.obs") #-0.06
cor(mydata$Birthrate, mydata$Service,  method = "pearson", use = "complete.obs") #-0.56

cor(mydata$Deathrate, mydata$Agriculture,  method = "pearson", use = "complete.obs") #0.38
cor(mydata$Deathrate, mydata$Industry,  method = "pearson", use = "complete.obs") #0.03
cor(mydata$Deathrate, mydata$Service,  method = "pearson", use = "complete.obs") #-0.36

cor(mydata$Agriculture, mydata$Industry,  method = "pearson", use = "complete.obs") #-0.32
cor(mydata$Agriculture, mydata$Service,  method = "pearson", use = "complete.obs") #-0.62

cor(mydata$Industry, mydata$Service,  method = "pearson", use = "complete.obs") # -0.54

mydatacont<-mydata[-c(1,2)]
useincorrplot<-cor(mydatacont, use = "complete.obs", method = "pearson")
corrplot(useincorrplot, method="color", order = "hclust")


    #num*num
 #vamos ignorar os outliers uma vez que este não permitem fazer uma leitura dos gráficos
#scatterplot(Population ~ area_sq, data = mydata, ylim=c(0,1.750e+07+1.5*17060149))
#scatterplot(Population ~ pop_dens, data = mydata, ylim=c(0,1.750e+07+1.5*17060149))
#scatterplot(Population ~ coastline, data = mydata, ylim=c(0,1.750e+07+1.5*17060149))
#scatterplot(Population ~ Net.migration, data = mydata, ylim=c(0,1.750e+07+1.5*17060149))
#scatterplot(Population ~ infant_mortal, data = mydata, ylim=c(0,1.750e+07+1.5*17060149))
#scatterplot(literacy~phones_per1000, data=mydata)
#scatterplot(GDP~phones_per1000, data=mydata)
 #NÃO SEI SE DEVERIAMOS APAGAR OS OUTLIERS....GRAFICOS DA POPULAÇÃO ESTÃO COM MUITO POUCO SENTIDO ATÉ AGORA


     #contengency table


    #statistical tests
describe_per_region<-mydata %>% group_by(Region) %>% describe()


mydata%>% group_by(Region) %>% mean(mydata$GDP)

meantable<-aggregate(mydata[, 3:20], list(mydata$Region), mean, na.rm=T)
mediantable<-aggregate(mydata[, 3:20], list(mydata$Region), median, na.rm=T)





#### MULTIVARIATE ANYLISES

 scatterplotMatrix(mydata[3:10])
 
 #PCA
 PCA_mydata <-PCA(mydata[,c(3:19)])
 PCA_mydata$svd$V # Eigenvectors
 PCA_mydata$eig # Eigenvalues
 ## plot of the eigenvalues:
 barplot(PCA_mydata$eig[,1],main="Eigenvalues",names.arg=1:nrow
         (PCA_mydata$eig))

 #FA
 KMO(useincorrplot)
 pc_mydata <- principal(useincorrplot,5,rotate="varimax") 
 pc_mydata$communality
 pc_mydata$loadings
 pc_mydata$residual
 #### COM FM="PA" está a dar erro não sei porquê
 pa_mydata <- fa(useincorrplot,nfactors=17,n.obs=227,fm="pa",rotate="varimax",SMC=FALSE) #principal axis 
 pa_bank_2 <- fa(useincorrplot,nfactors=17,n.obs=227,fm="minres",max.iter=100,rotate="varimax",SMC=FALSE) #min residuals 
 ml_bank <- fa(useincorrplot,nfactors=17,n.obs=227,fm="ml",rotate="varimax", SMC=FALSE) #maximum likelihood
 