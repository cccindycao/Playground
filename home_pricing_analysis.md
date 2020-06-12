---
title: "Home Price Analysis"
author: "Cindy Cao"
date: "1/15/2019"
output:
  html_document:
    keep_md: true
    
code_folding: hide
---



### Outline:
1. Problem Statement
2. Feature Engineering
3. Exploratory Data Analysis
4. Linear Regression
5. Gradient Boosting
6. Conclusion
7. Next Step

### 1. Problem Statement
Home price is a popular and well discussed topic, not only among home buyers, but also among investors, real-estate agencies, online listing platforms, etc. A traditional way to predict home prices is by analyzing house characteritics, such as location, square feet, building year, building materials, etc. However, with the increasing data availabilities, more complex and accurate models utilizing unconventional datasets or modeling techniques are emerging. This project aims to explore the relationship between home prices and various weather observations to better predict home prices.

### 2. Feature Engineering
####Load depository path and functions

```r
path<-'/Users/xucao/Google Drive/MyWorkStation/Projects/ODG-Weather-HousingPrice/'
dataloc<-paste0(path,'DATA/')
codeloc<-paste0(path,'CODE/')
output <-paste0(path,'OUTPUT/')
source(paste0(codeloc,"functions.r"))
source(paste0(codeloc,"attr.bivar.R"))             #self defined bivariate plot function
```

####Read in text files \newline

#####read in housing data

```r
housing_data<-fread(paste0(dataloc,'housing_data_ACS_15_5YR_DP04_with_ann.csv'),header=TRUE)
#rename zip code column
colnames(housing_data)[2]<-'ZIP'
#convert zip code column to numeric
housing_data$ZIP=as.numeric((housing_data$ZIP))
#drop the top row with variables descriptions
housing_data<-housing_data[2:nrow(housing_data),]
```

#####read in geographic data

```r
Gaz_zcta_national<-fread(paste0(dataloc,'2015_Gaz_zcta_national.txt'),header=TRUE)
colnames(Gaz_zcta_national)[1]<-'ZIP'
#convert integer64 columns to numeric 
Gaz_zcta_national$ALAND<-as.numeric(Gaz_zcta_national$ALAND)
Gaz_zcta_national$AWATER<-as.numeric(Gaz_zcta_national$AWATER)
```

#####read in location data

```r
weather_allstations<-read_fwf(file=paste0(dataloc,'weather_allstations.txt'),fwf_widths(c(12,9,10,7,3,31,4,4,6)))
#renaming columns
names(weather_allstations)<-c('STATION_ID','LAT','LONG','ELEVATION','STATE','STATION_NAME','SOURCE1','SOURCE2','ZIP_STATION')
#subset useful variables
weather_allstations<-weather_allstations[,c('STATION_ID','ELEVATION','STATE')]
```

#####read in zipcode to weather station mapping table

```r
weather_zipcodes_stations<-read_fwf(file=paste0(dataloc,'weather_zipcodes-normals-stations.txt'),fwf_widths(c(12,6,50)))
#Renaming columns
names(weather_zipcodes_stations)<-c('STATION_ID','ZIP','CITY')
#Formatting column 'CITY' and 'ZIP', all 'ZIP' columns are converted to numeric for matching purposes
weather_zipcodes_stations$CITY<-toupper(weather_zipcodes_stations$CITY)
weather_zipcodes_stations$ZIP<-as.numeric(weather_zipcodes_stations$ZIP)
#Create primary key
weather_zipcodes_stations$STATION<-paste0(formatC(weather_zipcodes_stations$ZIP, width=5, flag="0"),toupper(weather_zipcodes_stations$CITY))
```

#####read in weather data

```r
namelist<-c('STATION_ID','Weat_Jan','Weat_Jan_Type','Weat_Feb','Weat_Feb_Type','Weat_Mar','Weat_Mar_Type','Weat_Apr','Weat_Apr_Type','Weat_May','Weat_May_Type',
                       'Weat_Jun','Weat_Jun_Type','Weat_Jul','Weat_Jul_Type','Weat_Aug','Weat_Aug_Type','Weat_Sep','Weat_Sep_Type','Weat_Oct','Weat_Oct_Type','Weat_Nov','Weat_Nov_Type',
                       'Weat_Dec','Weat_Dec_Type')

weather_prcp<-read_fwf(file=paste0(dataloc,'weather_mly-prcp-normal.txt'),fwf_widths(c(17,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1)))
weather_tavg<-read_fwf(file=paste0(dataloc,'weather_mly-tavg-normal.txt'),fwf_widths(c(17,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1)))
weather_tmax<-read_fwf(file=paste0(dataloc,'weather_mly-tmax-normal.txt'),fwf_widths(c(17,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1)))
weather_tmin<-read_fwf(file=paste0(dataloc,'weather_mly-tmin-normal.txt'),fwf_widths(c(17,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1)))

names(weather_prcp)<-paste0(namelist,'_prcp')
names(weather_prcp)[1]<-'STATION_ID'
names(weather_tavg)<-paste0(namelist,'_tavg')
names(weather_tavg)[1]<-'STATION_ID'
names(weather_tmax)<-paste0(namelist,'_tmax')
names(weather_tmax)[1]<-'STATION_ID'
names(weather_tmin)<-paste0(namelist,'_tmin')
names(weather_tmin)[1]<-'STATION_ID'

#precipitation cannot be negative, so we convert negative values to NA
for (i in 1:ncol(weather_prcp)){
  if (class(weather_prcp[,i])=='numeric'){
    weather_prcp[,i]<-ifelse(weather_prcp[,i]<0,'NA',weather_prcp[,i])
  }
}


clpcdy15<-separate(read_fwf(file=paste0(dataloc,'clpcdy15.txt'),fwf_widths(c(38,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4)),skip=2),col='X1',into=c('STATION','STATE'),sep=',' )
names(clpcdy15)=c('STATION','STATE','YRS','Jan_CL','Jan_PC','Jan_CD','Feb_CL','Feb_PC','Feb_CD','Mar_CL','Mar_PC','Mar_CD','Apr_CL','Apr_PC','Apr_CD','May_CL','May_PC','May_CD','Jun_CL','Jun_PC','Jun_CD','Jul_CL','Jul_PC','Jul_CD','Aug_CL','Aug_PC','Aug_CD','Sep_CL','Sep_PC','Sep_CD','Oct_CL','Oct_PC','Oct_CD','Nov_CL','Nov_PC','Nov_CD','Dec_CL','Dec_PC','Dec_CD','Ann_CL','Ann_PC','Ann_CD')

#Base on Master Location Identifier Database downloaded from 'http://www.weathergraphics.com/identifiers/', WBAN is the weather station identifiersm. The first column of dataset clpcdy15 and pctpos15 consists of WBAN and CITY name.
clpcdy15$WBAN<-as.numeric(substr(clpcdy15$STATION,1,5))
MLID<-fread(file=paste0(dataloc,'MLID.csv'),header=TRUE)
MLID<-MLID%>%select(WBAN, (CITY))
MLID$CITY<-toupper(MLID$CITY)
clpcdy15<-clpcdy15%>%left_join(MLID,by='WBAN')
clpcdy15$CITY<-coalesce(clpcdy15$CITY,substr(clpcdy15$STATION, 6, 50))
clpcdy15$STATION=paste0(clpcdy15$WBAN,clpcdy15$CITY)
   
pctpos15<-separate(read_fwf(file=paste0(dataloc,'pctpos15.txt'),fwf_widths(c(37,16,6,6,6,6,6,6,6,6,6,6,6,6,6)),skip=2),col='X1',into=c('STATION','STATE'),sep=',' )   
names(pctpos15)=c('STATION','STATE','POR','JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC','ANN')
pctpos15$WBAN<-as.numeric(substr(pctpos15$STATION,1,5))
pctpos15$CITY<-substr(pctpos15$STATION, 6, 50)
```

####Check variable datatype

```r
sapply(housing_data, class) 
#We only keep the actual values (HC01), and drop other statistics columns, such as margin of error, percentage, etc.
housing_data<-housing_data%>%select(ZIP,starts_with('HC01_'))
#convert to numerica data
housing=as.data.frame(sapply(housing_data, as.numeric))
sapply(Gaz_zcta_national, class) 
sapply(weather_allstations, class) 
weather_allstations$ELEVATION<-as.numeric(weather_allstations$ELEVATION)
sapply(weather_zipcodes_stations, class) 
sapply(weather_prcp, class) 
sapply(weather_tavg, class) 
#convert character tempurature data into numeric data
weather_tavg$Weat_Jul_tavg<-as.numeric(weather_tavg$Weat_Jul_tavg)
weather_tavg$Weat_Aug_tavg<-as.numeric(weather_tavg$Weat_Aug_tavg)
sapply(weather_tmax, class) 
#convert character tempurature data into numeric data
weather_tmax$Weat_May_tmax<-as.numeric(weather_tmax$Weat_May_tmax)
weather_tmax$Weat_Jun_tmax<-as.numeric(weather_tmax$Weat_Jun_tmax)
weather_tmax$Weat_Jul_tmax<-as.numeric(weather_tmax$Weat_Jul_tmax)
weather_tmax$Weat_Aug_tmax<-as.numeric(weather_tmax$Weat_Aug_tmax)
weather_tmax$Weat_Sep_tmax<-as.numeric(weather_tmax$Weat_Sep_tmax)
sapply(weather_tmin, class) 
sapply(clpcdy15, class) 
clpcdy15<-cbind(clpcdy15[1:3], lapply(clpcdy15[4:42], as.numeric) ,clpcdy15[43:44])
sapply(pctpos15, class) 
```

####Check datasets primary key duplicates

```r
clpcdy15$STATION[duplicated(clpcdy15$STATION)]
#dedup on primary key
clpcdy15<-clpcdy15[!duplicated(clpcdy15$STATION),]
pctpos15$STATION[duplicated(pctpos15$STATION)]
pctpos15<-pctpos15[!duplicated(pctpos15$STATION),]
housing$ZIP[duplicated(housing$ZIP)]
Gaz_zcta_national$ZIP[duplicated(Gaz_zcta_national$ZIP)]
weather_allstations$STATION_ID[duplicated(weather_allstations$STATION_ID)]
weather_zipcodes_stations$STATION_ID[duplicated(weather_zipcodes_stations$STATION_ID)]
weather_prcp$STATION_ID[duplicated(weather_prcp$STATION_ID)]
weather_tavg$STATION_ID[duplicated(weather_tavg$STATION_ID)]
weather_tmax$STATION_ID[duplicated(weather_tmax$STATION_ID)]
weather_tmin$STATION_ID[duplicated(weather_tmin$STATION_ID)]
```

####Merge datasets

```r
data<-weather_zipcodes_stations%>%left_join(weather_allstations,by='STATION_ID')%>%left_join(weather_prcp,by='STATION_ID')%>%left_join(weather_tavg,by='STATION_ID')%>%left_join(weather_tmax,by='STATION_ID')%>%left_join(weather_tmin,by='STATION_ID')%>%left_join(housing,by='ZIP')%>%left_join(Gaz_zcta_national,by='ZIP')%>%left_join(pctpos15,by='STATION')%>%left_join(clpcdy15,by='STATION')
```

####Examine target

```r
summary(data$HC01_VC128)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   13000   88200  123700  159727  181300 1732700     371
```

```r
#drop records with NA target, these records without target informaiton are not useful to our supervised learning analysis.
data<-data[!is.na(data$HC01_VC128),]

#simulate gamma distribution 
theta=(sd(log(data$HC01_VC128)))^2/mean(log(data$HC01_VC128))
k=mean(log(data$HC01_VC128))/theta
gamma<-as.data.frame(rgamma(nrow(data),shape=k,scale=theta))
gamma$source<-'gamma'
names(gamma)=c('value','source')
#simulate normal distribution 
mean<-mean(log(data$HC01_VC128))
sd<-sd(log(data$HC01_VC128))
norm<-as.data.frame(rnorm(nrow(data),mean=mean,sd=sd))
norm$source='norm'
names(norm)=c('value','source')
#plot actual dependent variable distribution 
actual<-as.data.frame(log(data$HC01_VC128))
actual$source='actual'
names(actual)=c('value','source')
dist<-rbind(gamma,norm,actual)
dist$value<-as.numeric(dist$value)
ggplot(dist,aes(value, fill=source))+geom_density(alpha=0.5)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
#based on the above analysis, by comparing the distribution shape, a log normal distribution will be used for the target.
data$Target<-log(data$HC01_VC128)
```

####Split Train/Test/Holdout: 50%/25%/25% \newline

This step is to prepare data for future modeling practice, and we only conduct featue engineering and exploratory analysis on the training set. 

```r
set.seed(1)
inTrainingSet = createDataPartition(log(data$HC01_VC128),p=.5, list = FALSE)
dt = data[inTrainingSet,]
trainset = data[-inTrainingSet,]
set.seed(123)
inTrainingSet = createDataPartition(log(dt$HC01_VC128),p=.5, list = FALSE)
holdout = dt[inTrainingSet,]
testset = dt[-inTrainingSet,]

gbm.trainset<-trainset
gbm.testset<-testset
```

Check dependent variable distributions in different datasets. We are looking for similar dependent variable distributions among train/test/holdout sets to ensure that we do not have biased sample selection.

```r
##train number of records:
nrow(trainset)
```

```
## [1] 4710
```

```r
summary(log(trainset$HC01_VC128))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   9.473  11.387  11.726  11.793  12.108  14.362
```

```r
train_target_dist<-cbind(log(trainset$HC01_VC128),'trainset')
##test number of records:
nrow(testset)
```

```
## [1] 2355
```

```r
summary(log(testset$HC01_VC128))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   10.10   11.38   11.72   11.79   12.11   14.33
```

```r
test_target_dist<-cbind(log(testset$HC01_VC128),'testset')
##holdout number of records:
nrow(holdout)
```

```
## [1] 2358
```

```r
summary(log(holdout$HC01_VC128))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   9.473  11.387  11.726  11.799  12.107  14.365
```

```r
holdout_target_dist<-cbind(log(holdout$HC01_VC128),'holdout')

target_dist<-as.data.frame(rbind(train_target_dist,test_target_dist,holdout_target_dist))
names(target_dist)<-c('value','source')
target_dist$value<-as.numeric(target_dist$value)
ggplot(target_dist,aes(value, fill=source))+geom_density(alpha=0.5)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Before imputing missing values, we create missing flags and cap outliers first, as missing flags carry meaningful information, and outliers can impact missing value imputation.

####Create missing flags


```r
for (i in 1:ncol(trainset)){
  if(colnames(trainset)[i]!='HC01_VC128' & colnames(trainset)[i]!='Target'){
    trainset[,paste0("flag_",colnames(trainset)[i])]<-as.factor((ifelse(is.na(trainset[,i]),1,0)))
  }
}
```

####Cap outliers \newline

Due to the constrained time, here we use 5% and 95% quantiles to cap all the attributes. For future study, we can run chi-square outlier test and treat each attribute separately for outlier capping.\newline

```r
#boxplot shows outliers for dependent variable
ggplot(data=trainset,aes(x=1,y=trainset$HC01_VC128))+geom_boxplot()+xlab('Boxplot')+ylab('')
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
#tranform and cap dependent variable
trainset$Target<-pmin(log(trainset$HC01_VC128),quantile(log(trainset$HC01_VC128),0.99))
trainset<-trainset%>%select(-HC01_VC128)

#cap numeric independent variable
for (i in 1:ncol(trainset)){
if ((substr(colnames(trainset)[i],1,5)=='Weat_'|substr(colnames(trainset)[i],1,5)=='HC01_')&(class(eval(parse(text=paste0('trainset$',colnames(trainset)[i]))))=='numeric'|class(eval(parse(text=paste0('trainset$',colnames(trainset)[i]))))=='integer')){
  trainset[,paste0(colnames(trainset)[i],'_cap')]<-pmin(pmax(eval(parse(text=paste0('trainset$',colnames(trainset)[i]))),quantile(eval(parse(text=paste0('trainset$',colnames(trainset)[i]))),0.05,na.rm=TRUE)),quantile(eval(parse(text=paste0('trainset$',colnames(trainset)[i]))),0.95,na.rm=TRUE))
}
}

#save a capping value table for testset transformation later
trainset.num<-trainset[,sapply(trainset,class)=='numeric'|sapply(trainset,class)=='integer']
capping<-as.matrix(rbind(colnames(trainset.num),sapply(trainset.num,function(x) quantile(x,0.05,na.rm=TRUE)),sapply(trainset.num,function(x) quantile(x,0.95,na.rm=TRUE))))
colnames(capping)<-capping[1,]
capping<-as.data.frame(capping[2:3,])
```

####Impute missing values \newline

Regarding to missing values, we treat variables from different datasets differently. \newline

1. For weather related variables, climate is highy correlated with location, and areas within the same state share similar climate, therefore one way to impute the weather data is to use the state mean, and we have fully populated state information in the dataset, which makes this approach plausible. \newline

2. For house related variables, we can try two different approches, one is to utilize bivariate plot to go through the relationships between each indenpent variable and independent variables, and impute the missing values with user defined statistical method (mean, min, max, zero, etc.), or we can use k nearest neighor to impute the missing values. Here we use k-nearest neighbor to impute our housing variables.\newline

Select useful columns

```r
trainset<-as.data.frame(trainset%>%select(Target,ends_with('_cap'),starts_with('flag_'),contains('_Type'),ELEVATION,STATE.x,ALAND,AWATER))
```

Seperate different data type columns

```r
colclasses <- sapply(trainset,class)
numColumns <- which(colclasses=="numeric"|colclasses=="integer")
JustNumbers<-trainset%>%select(STATE.x,numColumns)
```

Create state average lookup table for weather related attributes imputation

```r
weat_state<-list()
for (i in seq_along(1:(ncol(JustNumbers)))){
  if (i==1){
   weat_state[[i]]<-as.vector((JustNumbers%>%select(STATE.x,colnames(JustNumbers)[i])%>%group_by(STATE.x)%>%summarise(avg=mean(eval(parse(text=colnames(JustNumbers)[i])),na.rm=TRUE)))[,1])  
  } else{
  weat_state[[i]]<-as.vector((JustNumbers%>%select(STATE.x,colnames(JustNumbers)[i])%>%group_by(STATE.x)%>%summarise(avg=mean(eval(parse(text=colnames(JustNumbers)[i])),na.rm=TRUE)))[,2])
  }
}

weat_state_avg<-matrix(nrow=51)
for (i in 1:length(weat_state)){
  weat_state_avg<-cbind(weat_state_avg,(unlist(weat_state[i])))
}
weat_state_avg<-as_tibble((weat_state_avg))[,2:ncol(weat_state_avg)]
weat_state_avg<-cbind(weat_state_avg[,1],sapply(weat_state_avg[,2:ncol(weat_state_avg)],as.numeric))
names(weat_state_avg)=c('STATE.x',paste0('state_avg_',colnames(JustNumbers)[2:ncol(JustNumbers)]))

trainset<-as.data.frame(trainset%>%left_join(weat_state_avg,by='STATE.x'))
```

Attributes imputation

```r
#Impute house characteristics data first with k nearest neighbor
HC_01<-trainset%>%select(starts_with('HC01_'))
HC_01_Imputed<-knnImputation(HC_01,k=3)
names(HC_01_Imputed)<-paste0('Impute_',colnames(HC_01_Imputed))
  
#Imput weather factor data with mode and numeric data with state average
for (i in 1:ncol(trainset)){
  if((class(trainset[,i])=='character'|class(trainset[,i])=='factor')&substr(colnames(trainset)[i],1,5)!='flag_'){   #Impute Weather type data and factors
    trainset[,i][trainset[,i] == 'P'] <- NA
    trainset[,paste0("F_",colnames(trainset)[i])]<-impute(as.factor(trainset[,i]),mode)
  }
  else if((substr(colnames(trainset)[i],1,5)=='Weat_')&(class(trainset[,i])=='numeric'|class(trainset[,i])=='integer')&(substr(colnames(trainset)[i],1,10)!='state_avg_')){ #Impute Weather numeric data
    trainset[,i]<-as.numeric(trainset[,i])
    trainset[,paste0("Impute_",colnames(trainset)[i])]=coalesce(trainset[,i],eval(parse(text=paste0('trainset$state_avg_',colnames(trainset)[i]))))#Impute state average
  }
}

trainset<-cbind(trainset,HC_01_Imputed)
trainset<-as.data.frame(trainset%>%select(Target,starts_with('flag_'),starts_with('Impute_'),starts_with('F_'),ELEVATION, ALAND, AWATER))
```

####Create climate regions \newline

Climate is highly impacted by the geograpic feature of the location. However, how granular our data needs to be regarding to geographic feature is debatable. In this project, we include both State and Climate regions information in our model. 

```r
trainset$Regions<-as.factor(ifelse(trainset$F_STATE.x%in%c('NT','WY','CO','ND','SD','NE','KS'),'northwest',ifelse(trainset$F_STATE.x%in%c('MN','IA','IL','WI','MO','OH','IN','MI'),'midwest',ifelse(trainset$F_STATE.x%in%c('AZ','NM','TX','OK'),'southwest',ifelse(trainset$F_STATE.x%in%c('WA','OR','NV','ID','UT','CA'),'westcoast',ifelse(trainset$F_STATE.x%in%c('AR','LA','MS','TN','AL','GA','FL','SC','NC','VA','WV','KY'),'south',('northeast')))))))

# trainset<-trainset%>%select(-F_STATE.x,)
```

###3. Exploratory Analysis \newline

After feature engineering, now we can look at some of the important weather attributes, and examine their relationships with home price.

#### precipitation

```r
#spring months
bivar.plot(trainset,'Impute_Weat_Apr_prcp_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
bivar.plot(trainset,'Impute_Weat_May_prcp_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-19-2.png)<!-- -->

```r
#summer months
bivar.plot(trainset,'Impute_Weat_Jun_prcp_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-19-3.png)<!-- -->

```r
#fall months
bivar.plot(trainset,'Impute_Weat_Sep_prcp_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-19-4.png)<!-- -->

```r
#winter months
bivar.plot(trainset,'Impute_Weat_Jan_prcp_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-19-5.png)<!-- -->

```r
bivar.plot(trainset,'Impute_Weat_Feb_prcp_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-19-6.png)<!-- -->

From the above charts, we see that higher precipitation areas has lower home price in summer months, however higher home price in winter months. One interesting observation about this attribute is that in September, precipitation has a nonlinear relationship with home price.  

To deal with this relationship, we can create two variables to represent different trends in different range. Here we use WOE to find the cutoff point.

WOE formula:

WOE=ln($\frac{p(non-event)}{p(event)}$)

IV=$\sum_{i=1}^n (DistributionGood-DistributionBad)$*WOE


```r
WOE_numeric_split(x='Impute_Weat_Sep_prcp_cap',y1='Target',data=trainset,group=10)
```

```
## $WOE
## # A tibble: 7 x 14
##     grp n_obs mean_x sum_y1 sum_y0 sum_w mean_y1 mean_y0 LowerBound UpperBound
##   <int> <int>  <dbl>  <dbl>  <dbl> <dbl>   <dbl>   <dbl>      <dbl>      <dbl>
## 1     1   470   56.2  5737.  5541.   470    12.2    11.8         45         88
## 2     2   469  116.   5593.  5530.   469    11.9    11.8         89        137
## 3     3   473  162.   5578.  5577.   473    11.8    11.8        138        198
## 4     4   928  279.  10735. 10941.   928    11.6    11.8        199        323
## 5     5   955  348.  11150. 11260.   955    11.7    11.8        324        374
## 6     6   943  405.  11125. 11118.   943    11.8    11.8        375        449
## 7     7   472  497.   5613.  5565.   472    11.9    11.8        450        518
## # … with 4 more variables: woe <dbl>, ks <dbl>, info <dbl>, plot.width <dbl>
## 
## $Stat
##         MAX_KS     Info.Value Trend.Estimate Trend.Pr(>|t|) 
##   4.698438e-01   2.337355e-04  -5.022986e-05   3.243015e-01 
## 
## $WOE_Code
## [1] "trainset$w_Impute_Weat_Sep_prcp_cap = trainset$Impute_Weat_Sep_prcp_cap"                                                                                            
## [2] "g_Impute_Weat_Sep_prcp_cap = c(45, 89, 138, 199, 324, 375, 450, 518)"                                                                                               
## [3] "trainset$w_Impute_Weat_Sep_prcp_cap[findInterval(trainset$Impute_Weat_Sep_prcp_cap, g_Impute_Weat_Sep_prcp_cap, rightmost.closed=TRUE) == 1] = 0.0347702723323038"  
## [4] "trainset$w_Impute_Weat_Sep_prcp_cap[findInterval(trainset$Impute_Weat_Sep_prcp_cap, g_Impute_Weat_Sep_prcp_cap, rightmost.closed=TRUE) == 2] = 0.0113994413145294"  
## [5] "trainset$w_Impute_Weat_Sep_prcp_cap[findInterval(trainset$Impute_Weat_Sep_prcp_cap, g_Impute_Weat_Sep_prcp_cap, rightmost.closed=TRUE) == 3] = 0.000260558517112001"
## [6] "trainset$w_Impute_Weat_Sep_prcp_cap[findInterval(trainset$Impute_Weat_Sep_prcp_cap, g_Impute_Weat_Sep_prcp_cap, rightmost.closed=TRUE) == 4] = -0.0190688743527721" 
## [7] "trainset$w_Impute_Weat_Sep_prcp_cap[findInterval(trainset$Impute_Weat_Sep_prcp_cap, g_Impute_Weat_Sep_prcp_cap, rightmost.closed=TRUE) == 5] = -0.00979637863295558"
## [8] "trainset$w_Impute_Weat_Sep_prcp_cap[findInterval(trainset$Impute_Weat_Sep_prcp_cap, g_Impute_Weat_Sep_prcp_cap, rightmost.closed=TRUE) == 6] = 0.000641267961459832"
## [9] "trainset$w_Impute_Weat_Sep_prcp_cap[findInterval(trainset$Impute_Weat_Sep_prcp_cap, g_Impute_Weat_Sep_prcp_cap, rightmost.closed=TRUE) == 7] = 0.0086564356766699"  
## 
## $Plot
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

Seen from the above WOE graph, the trend changed from negative to positive at bin 4, and we can find the mean of the independent variable for bin 4 from the table, which is -0.08290098. So we use this value as our cutoff value.


```r
trainset$Impute_Weat_Sep_prcp_g1_cap<-pmax(-0.08290098-trainset$Impute_Weat_Sep_prcp_cap,0)
trainset$Impute_Weat_Sep_prcp_g2_cap<-pmax(trainset$Impute_Weat_Sep_prcp_cap+0.08290098,0)

trainset<-trainset%>%select(-Impute_Weat_Sep_prcp_cap)
```

#### average tempurature

```r
#spring months
bivar.plot(trainset,'Impute_Weat_Apr_tavg_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

```r
#summer months
bivar.plot(trainset,'Impute_Weat_Jun_tavg_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-22-2.png)<!-- -->

```r
bivar.plot(trainset,'Impute_Weat_Jul_tavg_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-22-3.png)<!-- -->

```r
bivar.plot(trainset,'Impute_Weat_Aug_tavg_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-22-4.png)<!-- -->

```r
#winter months
bivar.plot(trainset,'Impute_Weat_Jan_tavg_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-22-5.png)<!-- -->

From the above charts, we see that home price is higher when summer months tempurature is lower, winter months tempurature does not have as big of an impact to the home price as summer tempurater.

#### maximum tempurature

```r
#spring months
bivar.plot(trainset,'Impute_Weat_Apr_tmax_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

```r
bivar.plot(trainset,'Impute_Weat_May_tmax_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-23-2.png)<!-- -->

```r
#summer months
bivar.plot(trainset,'Impute_Weat_Jun_tmax_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-23-3.png)<!-- -->

```r
bivar.plot(trainset,'Impute_Weat_Jul_tmax_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-23-4.png)<!-- -->

```r
bivar.plot(trainset,'Impute_Weat_Aug_tmax_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-23-5.png)<!-- -->

```r
#winter months
bivar.plot(trainset,'Impute_Weat_Dec_tmax_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-23-6.png)<!-- -->

```r
bivar.plot(trainset,'Impute_Weat_Jan_tmax_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-23-7.png)<!-- -->

#### minimum tempurature

```r
#spring months
bivar.plot(trainset,'Impute_Weat_May_tmin_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

```r
#summer months
bivar.plot(trainset,'Impute_Weat_Jun_tmin_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-24-2.png)<!-- -->

```r
bivar.plot(trainset,'Impute_Weat_Jul_tmin_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-24-3.png)<!-- -->

```r
bivar.plot(trainset,'Impute_Weat_Aug_tmin_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-24-4.png)<!-- -->

```r
#winter months
bivar.plot(trainset,'Impute_Weat_Dec_tmin_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-24-5.png)<!-- -->

```r
bivar.plot(trainset,'Impute_Weat_Jan_tmin_cap','Target',n.rank=50)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-24-6.png)<!-- -->

#### regions

```r
trainset%>%group_by(Regions)%>%summarise(avg_home_price=mean((Target)))
```

```
## # A tibble: 6 x 2
##   Regions   avg_home_price
##   <fct>              <dbl>
## 1 midwest             11.7
## 2 northeast           12.2
## 3 northwest           11.6
## 4 south               11.6
## 5 southwest           11.5
## 6 westcoast           12.2
```

Based on the exploratory analysis, we can see that geographic location, house characteristics and weather data are all predictable. The home price varies depending on which state the house is located, the house characteristics, and the location tempurature and the percentage of snowfall.

#### recording type
As for each record, the recording type is the same for weather attributes, so here we only look at one recording type attribute as an example.

```r
trainset%>%group_by(F_Weat_Jan_Type_prcp)%>%summarise(avg_home_price=mean((Target)))
```

```
## # A tibble: 4 x 2
##   F_Weat_Jan_Type_prcp avg_home_price
##   <fct>                         <dbl>
## 1 C                              11.8
## 2 Q                              11.9
## 3 R                              11.8
## 4 S                              11.7
```

```r
trainset%>%group_by(F_Weat_Jan_Type_tavg)%>%summarise(avg_home_price=mean((Target)))
```

```
## # A tibble: 4 x 2
##   F_Weat_Jan_Type_tavg avg_home_price
##   <fct>                         <dbl>
## 1 C                              11.8
## 2 Q                              11.8
## 3 R                              11.8
## 4 S                              11.8
```

```r
trainset%>%group_by(F_Weat_Jan_Type_tmax)%>%summarise(avg_home_price=mean((Target)))
```

```
## # A tibble: 4 x 2
##   F_Weat_Jan_Type_tmax avg_home_price
##   <fct>                         <dbl>
## 1 C                              11.8
## 2 Q                              11.8
## 3 R                              11.8
## 4 S                              11.8
```

```r
trainset%>%group_by(F_Weat_Jan_Type_tmin)%>%summarise(avg_home_price=mean((Target)))
```

```
## # A tibble: 4 x 2
##   F_Weat_Jan_Type_tmin avg_home_price
##   <fct>                         <dbl>
## 1 C                              11.9
## 2 Q                              11.8
## 3 R                              11.8
## 4 S                              11.8
```

###4. Linear Regression
Standardize numeric attributes \newline

As our weather and house data have very different scales, to better compare the coefficients of our models, we standardize the attributes before running models.

```r
colclasses <- sapply(trainset,class)
numColumns <- which(colclasses=="numeric"|colclasses=="integer")
JustNumbers<-trainset%>%select(numColumns)

preObj <- preProcess(JustNumbers[, -1], method=c("center", "scale"))
scaled.JustNumbers <- predict(preObj, JustNumbers[, -1])

othrColumns<-trainset%>%select(-numColumns)

trainset<-cbind(trainset$Target,othrColumns,scaled.JustNumbers)
names(trainset)[1]<-'Target'
```

Before variable reduction, we create dummy variables for all the factor columns first.

```r
trainset<-Filter(function(x)(length(unique(x))>1), trainset)
x <- model.matrix(Target~., trainset)[,-1]
y<-trainset$Target

z<-as.data.frame(x)
colnames(z)<-make.names(names(as.data.frame(z)), unique = FALSE, allow_ = TRUE)
trainset<-cbind(y,as.data.frame(z))
names(trainset)[1]<-'Target'
```

####Variable reduction
Lasso regression

```r
trainset<-Filter(function(x)(length(unique(x))>1), trainset)

set.seed(123)
cv.lasso <- cv.glmnet(x, (y), type.measure='mse',nfolds=5, alpha = 1, family = "gaussian")
cv.lasso$lambda.min
```

```
## [1] 0.0006357814
```

```r
model <- glmnet(x, y, alpha = 1, family = "gaussian",
                lambda = cv.lasso$lambda.min)
print(model)
```

```
## 
## Call:  glmnet(x = x, y = y, family = "gaussian", alpha = 1, lambda = cv.lasso$lambda.min) 
## 
##       Df   %Dev    Lambda
## [1,] 205 0.8522 0.0006358
```

```r
rank<-caret::getModelInfo("glmnet")$glmnet$varImp(model,lambda=cv.lasso$lambda.min)

#check variables with 0 coefficients
c<-coef(cv.lasso,s='lambda.min',exact=TRUE)
inds<-which(c==0)
variables<-row.names(c)[sort(inds,decreasing = FALSE)]
print(variables[1:50])
```

```
##  [1] "flag_Weat_Jan_Type_prcp1" "flag_Weat_Feb_prcp1"     
##  [3] "flag_Weat_Feb_Type_prcp1" "flag_Weat_Mar_prcp1"     
##  [5] "flag_Weat_Mar_Type_prcp1" "flag_Weat_Apr_prcp1"     
##  [7] "flag_Weat_Apr_Type_prcp1" "flag_Weat_May_prcp1"     
##  [9] "flag_Weat_May_Type_prcp1" "flag_Weat_Jun_prcp1"     
## [11] "flag_Weat_Jun_Type_prcp1" "flag_Weat_Jul_prcp1"     
## [13] "flag_Weat_Jul_Type_prcp1" "flag_Weat_Aug_prcp1"     
## [15] "flag_Weat_Aug_Type_prcp1" "flag_Weat_Sep_prcp1"     
## [17] "flag_Weat_Sep_Type_prcp1" "flag_Weat_Oct_prcp1"     
## [19] "flag_Weat_Oct_Type_prcp1" "flag_Weat_Nov_prcp1"     
## [21] "flag_Weat_Nov_Type_prcp1" "flag_Weat_Dec_prcp1"     
## [23] "flag_Weat_Dec_Type_prcp1" "flag_Weat_Jan_Type_tavg1"
## [25] "flag_Weat_Feb_tavg1"      "flag_Weat_Feb_Type_tavg1"
## [27] "flag_Weat_Mar_tavg1"      "flag_Weat_Mar_Type_tavg1"
## [29] "flag_Weat_Apr_tavg1"      "flag_Weat_Apr_Type_tavg1"
## [31] "flag_Weat_May_tavg1"      "flag_Weat_May_Type_tavg1"
## [33] "flag_Weat_Jun_tavg1"      "flag_Weat_Jun_Type_tavg1"
## [35] "flag_Weat_Jul_tavg1"      "flag_Weat_Jul_Type_tavg1"
## [37] "flag_Weat_Aug_tavg1"      "flag_Weat_Aug_Type_tavg1"
## [39] "flag_Weat_Sep_tavg1"      "flag_Weat_Sep_Type_tavg1"
## [41] "flag_Weat_Oct_tavg1"      "flag_Weat_Oct_Type_tavg1"
## [43] "flag_Weat_Nov_tavg1"      "flag_Weat_Nov_Type_tavg1"
## [45] "flag_Weat_Dec_tavg1"      "flag_Weat_Dec_Type_tavg1"
## [47] "flag_Weat_Jan_tmax1"      "flag_Weat_Jan_Type_tmax1"
## [49] "flag_Weat_Feb_tmax1"      "flag_Weat_Feb_Type_tmax1"
```
From Lasso regression, we see that most of the variables with 0 coefficients are flag variables. This gives us a general idea on what variables to drop later.

####Linear regression
#####Backward Selection

```r
trainset=trainset[!is.na(trainset$Target),]
#drop no variation columns
trainset<-Filter(function(x)(length(unique(x))>1), trainset)

#model_1
base.model<-glm(Target~., data=trainset, family = 'gaussian')

#Get variable list with P-value less than 0.05
varslist<-summary(base.model)$coefficients[(summary(base.model)$coef[, "Pr(>|t|)"])<0.05&!is.na((summary(base.model)$coef[, "Pr(>|t|)"])<0.05),1]

#model_2
trainset2<-trainset[,c(names(varslist)[2:length(names(varslist))])]
trainset2<-cbind(trainset$Target,trainset2)
names(trainset2)[1]<-'Target'
base.model2<-glm(Target~., data=trainset2, family = 'gaussian')

#Get variable list with P-value less than 0.05
varslist2<-summary(base.model2)$coefficients[(summary(base.model2)$coef[, "Pr(>|t|)"])<0.05,1]
names(varslist2)
vif(lm(base.model2, data=trainset2))

#model_3
trainset3<-trainset2[,c(names(varslist2)[2:length(names(varslist2))])]
trainset3<-cbind(trainset$Target,trainset3)
names(trainset3)[1]<-'Target'
base.model3<-glm(Target~., data=trainset3, family = 'gaussian')
vif(lm(base.model3, data=trainset3))
```


```r
#model_4
#Drop variables with high vif values in base.model3 
trainset4<-trainset3%>%select(-Impute_HC01_VC03_cap,-Impute_Weat_Oct_tmax_cap,-Impute_HC01_VC141_cap,-flag_HC01_VC691)
base.model4<-glm(Target~., data=trainset4, family = 'gaussian')
summary(base.model4)
```

```
## 
## Call:
## glm(formula = Target ~ ., family = "gaussian", data = trainset4)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -2.28188  -0.10381   0.01252   0.11990   1.72925  
## 
## Coefficients:
##                           Estimate Std. Error  t value Pr(>|t|)    
## (Intercept)              11.794244   0.004107 2871.916  < 2e-16 ***
## flag_HC01_VC091           0.249563   0.056295    4.433 9.50e-06 ***
## flag_HC01_VC701           0.179391   0.039556    4.535 5.90e-06 ***
## flag_HC01_VC1461         -0.112936   0.027384   -4.124 3.79e-05 ***
## flag_HC01_VC1551          0.201619   0.031707    6.359 2.23e-10 ***
## F_STATE.xCO               0.096090   0.022293    4.310 1.66e-05 ***
## F_STATE.xHI               0.247879   0.031399    7.894 3.61e-15 ***
## F_STATE.xKS              -0.161327   0.018551   -8.697  < 2e-16 ***
## F_STATE.xMT               0.089192   0.021167    4.214 2.56e-05 ***
## F_STATE.xSD              -0.194366   0.024675   -7.877 4.14e-15 ***
## F_STATE.xTX              -0.201945   0.016089  -12.552  < 2e-16 ***
## F_STATE.xUT               0.122146   0.025860    4.723 2.39e-06 ***
## F_STATE.xVA               0.129047   0.027445    4.702 2.65e-06 ***
## F_STATE.xWY               0.144247   0.028322    5.093 3.66e-07 ***
## Impute_Weat_Jul_prcp_cap -0.028106   0.004596   -6.115 1.04e-09 ***
## Impute_Weat_Jan_tavg_cap  0.106466   0.009468   11.245  < 2e-16 ***
## Impute_Weat_Sep_tavg_cap -0.108954   0.009212  -11.827  < 2e-16 ***
## Impute_HC01_VC05_cap      0.012284   0.006380    1.925 0.054230 .  
## Impute_HC01_VC08_cap      0.010799   0.003707    2.913 0.003598 ** 
## Impute_HC01_VC21_cap      0.031565   0.006703    4.709 2.57e-06 ***
## Impute_HC01_VC42_cap      0.025022   0.006570    3.809 0.000141 ***
## Impute_HC01_VC50_cap      0.014090   0.004979    2.830 0.004673 ** 
## Impute_HC01_VC70_cap     -0.020904   0.003996   -5.231 1.76e-07 ***
## Impute_HC01_VC85_cap     -0.039027   0.007486   -5.213 1.94e-07 ***
## Impute_HC01_VC94_cap      0.021537   0.004491    4.796 1.67e-06 ***
## Impute_HC01_VC120_cap    -0.150008   0.009165  -16.367  < 2e-16 ***
## Impute_HC01_VC121_cap    -0.068646   0.011326   -6.061 1.46e-09 ***
## Impute_HC01_VC122_cap     0.014258   0.012468    1.144 0.252855    
## Impute_HC01_VC123_cap     0.034687   0.012259    2.829 0.004683 ** 
## Impute_HC01_VC125_cap     0.039963   0.012364    3.232 0.001237 ** 
## Impute_HC01_VC126_cap     0.050687   0.011607    4.367 1.29e-05 ***
## Impute_HC01_VC127_cap     0.023375   0.006624    3.529 0.000421 ***
## Impute_HC01_VC140_cap     0.124565   0.014076    8.849  < 2e-16 ***
## Impute_HC01_VC142_cap    -0.090684   0.013074   -6.936 4.58e-12 ***
## Impute_HC01_VC143_cap    -0.050596   0.014079   -3.594 0.000329 ***
## Impute_HC01_VC144_cap    -0.024033   0.012891   -1.864 0.062352 .  
## Impute_HC01_VC145_cap    -0.040346   0.011992   -3.364 0.000773 ***
## Impute_HC01_VC146_cap     0.328105   0.008051   40.753  < 2e-16 ***
## Impute_HC01_VC155_cap     0.062627   0.006626    9.451  < 2e-16 ***
## Impute_HC01_VC170_cap     0.095805   0.011102    8.630  < 2e-16 ***
## Impute_HC01_VC191_cap     0.075179   0.006644   11.315  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.05424897)
## 
##     Null deviance: 1477.93  on 4709  degrees of freedom
## Residual deviance:  253.29  on 4669  degrees of freedom
## AIC: -316.53
## 
## Number of Fisher Scoring iterations: 2
```

#####Check multicollinearity with variance inflation factor

```r
vif(lm(base.model4, data=trainset4))
```

```
##          flag_HC01_VC091          flag_HC01_VC701         flag_HC01_VC1461 
##                 1.856701                 1.988951                 2.097918 
##         flag_HC01_VC1551              F_STATE.xCO              F_STATE.xHI 
##                 2.043867                 1.106112                 1.306127 
##              F_STATE.xKS              F_STATE.xMT              F_STATE.xSD 
##                 1.109903                 1.160733                 1.077041 
##              F_STATE.xTX              F_STATE.xUT              F_STATE.xVA 
##                 1.566230                 1.171144                 1.051655 
##              F_STATE.xWY Impute_Weat_Jul_prcp_cap Impute_Weat_Jan_tavg_cap 
##                 1.091277                 1.833726                 7.780751 
## Impute_Weat_Sep_tavg_cap     Impute_HC01_VC05_cap     Impute_HC01_VC08_cap 
##                 7.366807                 3.532799                 1.193014 
##     Impute_HC01_VC21_cap     Impute_HC01_VC42_cap     Impute_HC01_VC50_cap 
##                 3.900621                 3.746380                 2.151665 
##     Impute_HC01_VC70_cap     Impute_HC01_VC85_cap     Impute_HC01_VC94_cap 
##                 1.385987                 4.865102                 1.750420 
##    Impute_HC01_VC120_cap    Impute_HC01_VC121_cap    Impute_HC01_VC122_cap 
##                 7.291768                11.135451                13.493400 
##    Impute_HC01_VC123_cap    Impute_HC01_VC125_cap    Impute_HC01_VC126_cap 
##                13.045743                13.270368                11.694692 
##    Impute_HC01_VC127_cap    Impute_HC01_VC140_cap    Impute_HC01_VC142_cap 
##                 3.808437                17.199512                14.837614 
##    Impute_HC01_VC143_cap    Impute_HC01_VC144_cap    Impute_HC01_VC145_cap 
##                17.206585                14.425669                12.483018 
##    Impute_HC01_VC146_cap    Impute_HC01_VC155_cap    Impute_HC01_VC170_cap 
##                 5.626486                 3.811400                10.698985 
##    Impute_HC01_VC191_cap 
##                 3.831942
```

We do not see extremly high vif values. Therefore, our final linear regression model does not have multi-collinearity issue.

####Getting ready for preformance test
#####Testset transformation - the same transformations as trainset  
*For any transformation explanations, refer to previous trainset feature engineering section.

```r
for (i in 1:ncol(testset)){
  if(colnames(testset)[i]!='HC01_VC128' & colnames(testset)[i]!='Target'){
    testset[,paste0("flag_",colnames(testset)[i])]<-as.factor((ifelse(is.na(testset[,i]),1,0)))
  }
}

testset<-as.data.frame(testset%>%select(-HC01_VC128))

#capping variables using trainset capping values
for (i in 1:ncol(testset)){
  if ((substr(colnames(testset)[i],1,5)=='Weat_'|substr(colnames(testset)[i],1,5)=='HC01_')&(class(eval(parse(text=paste0('testset$',colnames(testset)[i]))))=='numeric'|class(eval(parse(text=paste0('testset$',colnames(testset)[i]))))=='integer')){
    testset[,paste0(colnames(testset)[i],'_cap')]<-pmin(pmax(testset[,i],capping[1,colnames(testset)[i]]),capping[2,colnames(testset)[i]])
  }
}

testset<-as.data.frame(testset%>%select(Target,ends_with('_cap'),starts_with('flag_'),contains('_Type'),STATE.x,ELEVATION))

testset<-as.data.frame(testset%>%left_join(weat_state_avg,by='STATE.x'))

#Impute house characteristics data first with k nearest neighbor
HC_01<-testset%>%select(starts_with('HC01_'))
HC_01_Imputed<-knnImputation(HC_01,k=3)
names(HC_01_Imputed)<-paste0('Impute_',colnames(HC_01_Imputed))

#Imput weather data with mode and state average
for (i in 1:ncol(testset)){
  if((class(testset[,i])=='character'|class(testset[,i])=='factor')&substr(colnames(testset)[i],1,5)!='flag_'){   #Impute Weather type data and factors
    testset[,paste0("F_",colnames(testset)[i])]<-impute(as.factor(testset[,i]),mode)
  }
  else if((substr(colnames(testset)[i],1,5)=='Weat_')&(class(testset[,i])=='numeric'|class(testset[,i])=='integer')&(substr(colnames(testset)[i],1,10)!='state_avg_')){ #Impute Weather numeric data
    testset[,i]<-as.numeric(testset[,i])
    testset[,paste0("Impute_",colnames(testset)[i])]=coalesce(testset[,i],eval(parse(text=paste0('testset$state_avg_',colnames(testset)[i]))))#Impute state average
  }
}

testset<-cbind(testset,HC_01_Imputed)
testset<-as.data.frame(testset%>%select(Target,starts_with('flag_'),starts_with('Impute_'),starts_with('F_'),ELEVATION))

testset$Regions<-as.factor(ifelse(testset$F_STATE.x%in%c('NT','WY','CO','ND','SD','NE','KS'),'northwest',ifelse(testset$F_STATE.x%in%c('MN','IA','IL','WI','MO','OH','IN','MI'),'midwest',ifelse(testset$F_STATE.x%in%c('AZ','NM','TX','OK'),'southwest',ifelse(testset$F_STATE.x%in%c('WA','OR','NV','ID','UT','CA'),'westcoast',ifelse(testset$F_STATE.x%in%c('AR','LA','MS','TN','AL','GA','FL','SC','NC','VA','WV','KY'),'south',('northeast')))))))

# testset<-testset%>%select(-F_STATE.x)
testset$Impute_Weat_Sep_prcp_g1_cap<-pmax(-0.08290098-testset$Impute_Weat_Sep_prcp_cap,0)
testset$Impute_Weat_Sep_prcp_g2_cap<-pmax(testset$Impute_Weat_Sep_prcp_cap+0.08290098,0)

testset<-testset%>%select(-Impute_Weat_Sep_prcp_cap)

colclasses <- sapply(testset,class)
numColumns <- which(colclasses=="numeric"|colclasses=="integer")
JustNumbers<-testset%>%select(numColumns)

preObj <- preProcess(JustNumbers[, -1], method=c("center", "scale"))
scaled.JustNumbers <- predict(preObj, JustNumbers[, -1])

OthrNumbers<-testset%>%select(-numColumns)

set<-cbind(testset$Target,OthrNumbers,scaled.JustNumbers)
names(set)[1]<-'Target'

testset<-Filter(function(x)(length(unique(x))>1), testset)
# testset$Target<-log(testset$Target)

x <- model.matrix(Target~., testset)[,-1]
y<-testset$Target

x<-as.data.frame(x)
colnames(x)<-make.names(names(as.data.frame(x)), unique = FALSE, allow_ = TRUE)
testset<-cbind(y,as.data.frame(x))
names(testset)[1]<-'Target'
```

#####Predict home price on testset

```r
testset$glm_pred<-round(predict(base.model4,newdata=testset))
```

#####Rescale prediction to the dependent variable mean \newline

As we standardized our independent variables to mean=0, we need to rescale our prediction back to the actual dependent variable mean level for the next step.

```r
testset$glm_pred_rescaled<-(testset$glm_pred)*(mean(exp(testset$Target))/mean(testset$glm_pred))
```

#####Plot Gains Chart \newline

*GAINS.CHART function is a self defined function. The function uses predicted dependent variables quantiles as x-axis, and plot the actual dependent variable on y-axis. A good gains chart should show a nice upward trend with good KS value, and good percentage gains between the highest group and the lowest group.

```r
GAINS.CHART((exp(testset$Target)),(testset$glm_pred_rescaled),n.rank=20)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-36-1.png)<!-- -->

```
## $gainschart
##    Rank Actual_Sum Actual_Mean Pred_Sum Pred_Mean Count
## 1     1    7845200    65926.05  9025484  75844.40   119
## 2     2    8409900    71879.49 11009493  94098.23   117
## 3     3    9184900    77838.14 11952935 101296.06   118
## 4     4   10491600    85996.72 13119060 107533.28   122
## 5     5   10599900    93804.42 12762194 112939.77   113
## 6     6   11522100    95223.97 14223204 117547.14   121
## 7     7   12095600   105179.13 14134706 122910.49   115
## 8     8   13959100   115364.46 15574630 128715.95   121
## 9     9   13865000   118504.27 15754182 134651.13   117
## 10   10   15564000   126536.59 17238195 140147.93   123
## 11   11   14659800   133270.91 16065361 146048.74   110
## 12   12   16702800   141549.15 18075568 153182.78   118
## 13   13   18288600   153685.71 19249041 161756.64   119
## 14   14   19780200   169061.54 19985776 170818.60   117
## 15   15   20999100   181026.72 20935289 180476.63   116
## 16   16   22843200   193586.44 22540707 191022.94   118
## 17   17   25399000   209909.09 24807141 205017.70   121
## 18   18   26717600   234364.91 25620873 224744.50   114
## 19   19   33790400   283952.94 30497190 256278.91   119
## 20   20   60595500   517910.26 40742472 348226.25   117
## 
## $ks
## [1] 21.29
## 
## $gini
## [1] 58.76
```

####Lorenz Curve, Train vs Test

Lorenz Curve is commonly used to check model overfitting

```r
#predict and rescale on trainset
trainset4$glm_pred<-round(predict(base.model4,newdata=trainset4))
trainset4$glm_pred_rescaled<-(trainset4$glm_pred)*(mean(exp(trainset4$Target))/mean(trainset4$glm_pred))

##Lorenz Curve
trainCheck<- OrderedCDF(Score = trainset4$glm_pred, Loss = exp(trainset4$Target), Weight =rep(1,nrow(trainset4)), NBins = 1000)
ValidationCheck<- OrderedCDF(Score = testset$glm_pred, Loss = exp(testset$Target), Weight =rep(1,nrow(testset)), NBins = 1000)

ValidationWeight <- ValidationCheck$WeightPts
ValidationLoss <- ValidationCheck$LossPts
TrainWeight <- trainCheck$WeightPts
TrainLoss <- trainCheck$LossPts

ModelComparisonData <- data.frame(ValidationWeight, ValidationLoss, TrainWeight, TrainLoss)
```


```r
ModelComparisonData %>% ggvis(~ValidationWeight, ~ValidationLoss) %>% layer_paths(stroke := "blue") %>% 
  layer_paths(data = ModelComparisonData, x = ~ValidationWeight, y = ~ValidationWeight, stroke := "black") %>%
  layer_paths(data = ModelComparisonData, x = ~TrainWeight, y = ~TrainLoss, stroke := "red")
```

<!--html_preserve--><div id="plot_id760492419-container" class="ggvis-output-container">
<div id="plot_id760492419" class="ggvis-output"></div>
<div class="plot-gear-icon">
<nav class="ggvis-control">
<a class="ggvis-dropdown-toggle" title="Controls" onclick="return false;"></a>
<ul class="ggvis-dropdown">
<li>
Renderer: 
<a id="plot_id760492419_renderer_svg" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id760492419" data-renderer="svg">SVG</a>
 | 
<a id="plot_id760492419_renderer_canvas" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id760492419" data-renderer="canvas">Canvas</a>
</li>
<li>
<a id="plot_id760492419_download" class="ggvis-download" data-plot-id="plot_id760492419">Download</a>
</li>
</ul>
</nav>
</div>
</div>
<script type="text/javascript">
var plot_id760492419_spec = {
  "data": [
    {
      "name": ".0",
      "format": {
        "type": "csv",
        "parse": {
          "ValidationWeight": "number",
          "ValidationLoss": "number"
        }
      },
      "values": "\"ValidationWeight\",\"ValidationLoss\"\n0,0\n0.000849256900212314,0.00029760509598501\n0.00169851380042463,0.000538421460782961\n0.0029723991507431,0.00105621682580459\n0.00382165605095541,0.00126890669638253\n0.00467091295116773,0.00153784955540049\n0.0059447983014862,0.00204573367960173\n0.00679405520169851,0.00233583837712807\n0.00764331210191083,0.00259781658043441\n0.0089171974522293,0.00301033849566115\n0.00976645435244161,0.00334946365454236\n0.0106157112526539,0.00370841129506434\n0.0118895966029724,0.00418816892504557\n0.0127388535031847,0.00449729249009211\n0.013588110403397,0.0048227562089236\n0.0148619957537155,0.00532528290565436\n0.0157112526539278,0.0057107498121552\n0.0169851380042463,0.00626149335612026\n0.0178343949044586,0.00660169000049556\n0.0186836518046709,0.00690679549493924\n0.0199575371549894,0.00741655471875515\n0.0208067940552017,0.00776907344631255\n0.021656050955414,0.00808114359646785\n0.0229299363057325,0.00857590202336642\n0.0237791932059448,0.00892038460971811\n0.0246284501061571,0.00923781218734388\n0.0259023354564756,0.00978373404658551\n0.0267515923566879,0.0101611648118806\n0.0276008492569002,0.0105594895443106\n0.0288747346072187,0.0111163941298667\n0.029723991507431,0.0114367682926013\n0.0309978768577495,0.0118463436227192\n0.0318471337579618,0.0121648426858391\n0.0326963906581741,0.0124386072295805\n0.0339702760084926,0.01302845999408\n0.0348195329087049,0.0134174092284367\n0.0356687898089172,0.0138347528283869\n0.0369426751592357,0.0143086172881506\n0.037791932059448,0.0146927448377838\n0.0386411889596603,0.015177592023862\n0.0399150743099788,0.0156594926248314\n0.0407643312101911,0.0161930924008909\n0.0416135881104034,0.0164888223972613\n0.0428874734607219,0.0172021638649553\n0.0437367303609342,0.0175493251650423\n0.0445859872611465,0.0179347920715431\n0.045859872611465,0.0187501925325497\n0.0467091295116773,0.0191442313230033\n0.0479830148619958,0.0197236371039354\n0.0488322717622081,0.0201455345172355\n0.0496815286624204,0.0206309174460607\n0.0509554140127389,0.021217555754078\n0.0518046709129512,0.0215582881412004\n0.0526539278131635,0.0218912522584905\n0.053927813163482,0.0223648488468807\n0.0547770700636943,0.0227154924748234\n0.0556263269639066,0.0230674754596338\n0.0569002123142251,0.0236104507337667\n0.0577494692144374,0.0239977927398822\n0.0585987261146497,0.0245046053785893\n0.0598726114649682,0.0251030300270416\n0.0607218683651805,0.0254448338996581\n0.0619957537154989,0.0260212930954814\n0.0628450106157113,0.0264161355000556\n0.0636942675159236,0.0268109779046298\n0.064968152866242,0.0274096704244556\n0.0658174097664544,0.0278262104102852\n0.0666666666666667,0.028284806201758\n0.0679405520169851,0.0288441216296759\n0.0687898089171975,0.0292652154288554\n0.0696390658174098,0.0296008582598808\n0.0709129511677282,0.0302346419296382\n0.0717622080679405,0.0305405510382025\n0.0726114649681529,0.0308038685983764\n0.0738853503184713,0.0314598855921364\n0.0747346072186836,0.0318212440750201\n0.075583864118896,0.0322787683809988\n0.0768577494692144,0.0327796878494884\n0.0777070063694267,0.0331579222289041\n0.0789808917197452,0.0337266131548953\n0.0798301486199575,0.0341562788380276\n0.0806794055201698,0.0344295076390219\n0.0819532908704883,0.0350022166356159\n0.0828025477707006,0.0353922373554666\n0.0836518046709129,0.0357822580753174\n0.0849256900212314,0.0363664855409729\n0.0857749469214437,0.0367768644852115\n0.086624203821656,0.0371009888471754\n0.0878980891719745,0.0377211110768831\n0.0887473460721868,0.0381400619050744\n0.0895966029723991,0.0384612396819295\n0.0908704883227176,0.0391279715306304\n0.0917197452229299,0.0394515601498472\n0.0929936305732484,0.0399859635400273\n0.0938428874734607,0.0405021516768078\n0.094692144373673,0.040896994081382\n0.0959660297239915,0.0415058657134017\n0.0968152866242038,0.0419376743675222\n0.0976645435244161,0.0423520713823636\n0.0989384288747346,0.0429879980231093\n0.0997876857749469,0.0432989966877705\n0.100636942675159,0.0437018752335504\n0.101910828025478,0.0443069966663408\n0.10276008492569,0.0447779145409957\n0.103609341825902,0.0452059729958868\n0.104883227176221,0.0458290418107033\n0.105732484076433,0.0461807569241402\n0.106581740976645,0.0466200659767193\n0.107855626326964,0.047307156049808\n0.108704883227176,0.0476816402299944\n0.109978768577495,0.0482042572797394\n0.110828025477707,0.0485664193767437\n0.111677282377919,0.0488712569998138\n0.112951167728238,0.0494356619838286\n0.11380042462845,0.0498554164261405\n0.114649681528662,0.0502912431508638\n0.115923566878981,0.0510016380334491\n0.116772823779193,0.051404516579229\n0.117622080679406,0.0517763220456801\n0.118895966029724,0.0524020695742318\n0.119745222929936,0.0527521774594275\n0.120594479830149,0.0531914865120067\n0.121868365180467,0.0539492946277057\n0.122717622080679,0.0544370883988926\n0.123991507430998,0.0550333700763567\n0.12484076433121,0.0554416060496071\n0.125690021231423,0.0558262693419874\n0.126963906581741,0.0564123719072576\n0.127813163481953,0.0567911420294203\n0.128662420382166,0.0574077819312722\n0.129936305732484,0.0580758531368408\n0.130785562632696,0.0584514088025212\n0.131634819532909,0.058869556016592\n0.132908704883227,0.0593664574144787\n0.133757961783439,0.0597979981972257\n0.134607218683652,0.0602654337440248\n0.13588110403397,0.0607832291090464\n0.136730360934183,0.0613382585949879\n0.137579617834395,0.0617813178467963\n0.138853503184713,0.0623909930929366\n0.139702760084926,0.062866196909568\n0.140976645435244,0.0633963143577717\n0.141825902335456,0.0638683037179207\n0.142675159235669,0.0642170722462488\n0.143949044585987,0.0648227294217862\n0.1447983014862,0.0652446268350863\n0.145647558386412,0.0656354511690576\n0.14692144373673,0.0663758476454776\n0.147770700636943,0.067154817599685\n0.148619957537155,0.0674639411647315\n0.149893842887473,0.0679246799271926\n0.150743099787686,0.0682881813810644\n0.151592356687898,0.0687001675535441\n0.152866242038217,0.0694073479796471\n0.153715498938429,0.0697788855747247\n0.154989384288747,0.0704482961371609\n0.15583864118896,0.0709757348716293\n0.156687898089172,0.0714579033439723\n0.15796178343949,0.0719438220155446\n0.158811040339703,0.0724442057412872\n0.159660297239915,0.0728093144234002\n0.160934182590234,0.0734881004839096\n0.161783439490446,0.0739627685577939\n0.162632696390658,0.0744304719759666\n0.163906581740977,0.0751221158624052\n0.164755838641189,0.0755354413917525\n0.165605095541401,0.0760213600633248\n0.16687898089172,0.0768164022999436\n0.167728237791932,0.0772798197761399\n0.168577494692144,0.0776931453054872\n0.169851380042463,0.0782757655429016\n0.170700636942675,0.0788096331903347\n0.171974522292994,0.0794230586357043\n0.172823779193206,0.0797560227529945\n0.173673036093418,0.0801369358461454\n0.174946921443737,0.0807562544617326\n0.175796178343949,0.0813466429689792\n0.176645435244161,0.0817744335524968\n0.17791932059448,0.0824966147755171\n0.178768577494692,0.0828986897071764\n0.179617834394904,0.0835247051071017\n0.180891719745223,0.084157417291365\n0.181740976645435,0.0846160130828379\n0.182590233545648,0.0850263920270764\n0.183864118895966,0.0857874145992577\n0.184713375796178,0.0861653811072999\n0.185987261146497,0.0870370345567465\n0.186836518046709,0.0874573247418055\n0.187685774946921,0.0878655607150558\n0.18895966029724,0.0885681873278089\n0.189808917197452,0.0890661602111898\n0.190658174097665,0.0895349351148565\n0.191932059447983,0.0903372098785605\n0.192781316348195,0.0907725008605368\n0.193630573248408,0.0912643127023266\n0.194904458598726,0.0919283658372922\n0.195753715498938,0.0925241117720093\n0.196602972399151,0.0929738678081559\n0.197876857749469,0.0935315760078326\n0.198726114649682,0.0939320437112507\n0.199575371549894,0.0943933182164588\n0.200849256900212,0.0955467723508526\n0.201698513800425,0.0959622408511881\n0.202972399150743,0.0967232634233694\n0.203821656050955,0.097207842738074\n0.204670912951168,0.0976088461842392\n0.205944798301486,0.098207002961318\n0.206794055201699,0.0986492585990059\n0.207643312101911,0.0990947286931761\n0.208917197452229,0.0997906585215911\n0.209766454352442,0.100293988832442\n0.210615711252654,0.100722850901454\n0.211889596602972,0.10136333135555\n0.212738853503185,0.10188862711903\n0.213588110403397,0.102267129369819\n0.214861995753716,0.103085476415935\n0.215711252653928,0.103626040847706\n0.216985138004246,0.104314202406289\n0.217834394904459,0.104802264048849\n0.218683651804671,0.105151568319924\n0.219957537154989,0.105850176862074\n0.220806794055202,0.106253323279228\n0.221656050955414,0.106924876812652\n0.222929936305732,0.107660451604349\n0.223779193205945,0.108160299587344\n0.224628450106157,0.108623181320793\n0.225902335456476,0.109334379817499\n0.226751592356688,0.109689845130165\n0.2276008492569,0.110162370233061\n0.228874734607219,0.111353862102496\n0.229723991507431,0.111818351064186\n0.230997876857749,0.112589017005814\n0.231847133757962,0.113560854348959\n0.232696390658174,0.114032040094987\n0.233970276008493,0.114711361898244\n0.234819532908705,0.115195137598828\n0.235668789808917,0.115673020129194\n0.236942675159236,0.116296088944011\n0.237791932059448,0.116678609265403\n0.23864118895966,0.11712916891567\n0.239915074309979,0.117858046923028\n0.240764331210191,0.118331375640045\n0.241613588110403,0.118712020861823\n0.242887473460722,0.120754807956316\n0.243736730360934,0.121194384880268\n0.244585987261146,0.121738163768522\n0.245859872611465,0.122347303271915\n0.246709129511677,0.122808309905749\n0.247983014861996,0.123658801516688\n0.248832271762208,0.123930690960814\n0.24968152866242,0.124424913644966\n0.250955414012739,0.125441217636115\n0.251804670912951,0.125796950820155\n0.252653927813163,0.126353855405711\n0.253927813163482,0.127006390071615\n0.254777070063694,0.127776252399123\n0.255626326963907,0.128214222094834\n0.256900212314225,0.128974976795642\n0.257749469214437,0.129450180612274\n0.25859872611465,0.129911455117482\n0.259872611464968,0.130528362890707\n0.26072186836518,0.131063034152261\n0.261995753715499,0.131748249125735\n0.262845010615711,0.132273544889215\n0.263694267515924,0.132768839058861\n0.264968152866242,0.133398872529389\n0.265817409766454,0.133895773927276\n0.266666666666667,0.134318207083323\n0.267940552016985,0.135081104755119\n0.268789808917197,0.135559255156859\n0.26963906581741,0.136024011989923\n0.270912951167728,0.136827358239121\n0.271762208067941,0.137298811856523\n0.272611464968153,0.137760889975851\n0.273885350318471,0.138929344907162\n0.274734607218684,0.139473391666789\n0.275583864118896,0.139944041670071\n0.276857749469214,0.140745512819654\n0.277707006369427,0.14118991142833\n0.278980891719745,0.142008258474446\n0.279830148619958,0.142391046667211\n0.28067940552017,0.142931075356235\n0.281953290870488,0.143719152937143\n0.282802547770701,0.144243109343755\n0.283651804670913,0.144770012335477\n0.284925690021231,0.145521927280958\n0.285774946921444,0.146081242708876\n0.286624203821656,0.146573858164786\n0.287898089171975,0.147288003246601\n0.288747346072187,0.148286359855724\n0.289596602972399,0.148805494577614\n0.290870488322718,0.149494995493064\n0.29171974522293,0.150009576401603\n0.292993630573248,0.150620055261864\n0.293842887473461,0.151178031332915\n0.294692144373673,0.15163984158087\n0.295966029723991,0.152378630829049\n0.296815286624204,0.15291919526082\n0.297664543524416,0.153352343271808\n0.298938428874735,0.154151671450403\n0.299787685774947,0.154628750366649\n0.300636942675159,0.155111186710365\n0.301910828025478,0.155766935832752\n0.30276008492569,0.156353574140769\n0.303609341825902,0.156854761480632\n0.304883227176221,0.157692127394268\n0.305732484076433,0.158413772874541\n0.306581740976645,0.159004161381788\n0.307855626326964,0.159877957802223\n0.308704883227176,0.160349411419625\n0.309978768577495,0.161344553572266\n0.310828025477707,0.161775826483639\n0.311677282377919,0.162111469314664\n0.312951167728238,0.16304232233766\n0.31380042462845,0.163483506489854\n0.314649681528662,0.164108450404285\n0.315923566878981,0.164799290676603\n0.316772823779193,0.165359677590015\n0.317622080679406,0.165950869711382\n0.318895966029724,0.166628316415024\n0.319745222929936,0.167471575498877\n0.320594479830149,0.168017229486745\n0.321868365180467,0.168812271723364\n0.322717622080679,0.169585884250101\n0.323991507430998,0.170360300390958\n0.32484076433121,0.170890149967788\n0.325690021231422,0.171329994763115\n0.326963906581741,0.172129858684457\n0.327813163481953,0.172815073657931\n0.328662420382166,0.173476983821908\n0.329936305732484,0.174373549309093\n0.330785562632696,0.174851699710833\n0.331634819532909,0.175603078913567\n0.332908704883227,0.176285079430559\n0.333757961783439,0.176892075962964\n0.334607218683652,0.177446301834785\n0.33588110403397,0.178246165756127\n0.336730360934183,0.17874199566852\n0.337579617834395,0.179291935598364\n0.338853503184713,0.180183411529452\n0.339702760084926,0.180692635010521\n0.340976645435244,0.181587057526717\n0.341825902335456,0.182250842790309\n0.342675159235669,0.182640059896039\n0.343949044585987,0.183546000881297\n0.3447983014862,0.184192642376983\n0.345647558386412,0.184835533673441\n0.34692144373673,0.185585305647934\n0.347770700636943,0.186267574036299\n0.348619957537155,0.186881535224416\n0.349893842887473,0.187653272651538\n0.350743099787686,0.18812472626894\n0.351592356687898,0.18871538264756\n0.352866242038217,0.18944131406981\n0.353715498938429,0.189960448791699\n0.354989384288747,0.190941661632917\n0.35583864118896,0.191507941716547\n0.356687898089172,0.192163422967559\n0.35796178343949,0.193029183246789\n0.358811040339703,0.193595999073165\n0.359660297239915,0.194137902861804\n0.360934182590234,0.195032593249374\n0.361783439490446,0.195555746041866\n0.362632696390658,0.19608211329084\n0.363906581740977,0.197078594800349\n0.364755838641189,0.197723896939168\n0.365605095541401,0.198383664132157\n0.36687898089172,0.19926415733693\n0.367728237791932,0.199973212862648\n0.368577494692144,0.202023500355599\n0.369851380042463,0.203485810183666\n0.370700636942675,0.203992087079626\n0.371974522292994,0.204829185121888\n0.372823779193206,0.205338408602957\n0.373673036093418,0.205802897564647\n0.374946921443737,0.206539811713212\n0.375796178343949,0.20702010508594\n0.376645435244161,0.207548079563155\n0.37791932059448,0.208387052705032\n0.378768577494692,0.208993245623317\n0.379617834394904,0.209406035409917\n0.380891719745223,0.210459573521986\n0.381740976645435,0.21106496282615\n0.382590233545648,0.211729283832489\n0.383864118895966,0.212730319155348\n0.384713375796178,0.213321243405342\n0.385987261146497,0.213985832283054\n0.386836518046709,0.214627652094017\n0.387685774946921,0.215224737385602\n0.38895966029724,0.216098533806037\n0.389808917197452,0.216687315085042\n0.390658174097665,0.21724663051296\n0.391932059447983,0.218477499474302\n0.392781316348195,0.219098425318131\n0.393630573248408,0.219788729847702\n0.394904458598726,0.220535555237086\n0.395753715498938,0.221250503933021\n0.396602972399151,0.221916164296228\n0.397876857749469,0.222853178360815\n0.398726114649682,0.22342749458565\n0.399575371549894,0.224079493508807\n0.400849256900212,0.224811853844021\n0.401698513800425,0.22530205845757\n0.402972399150743,0.226203981372225\n0.403821656050955,0.226763564671516\n0.404670912951168,0.227314576086855\n0.405944798301486,0.228271948375829\n0.406794055201699,0.228921804327998\n0.407643312101911,0.229394865173641\n0.408917197452229,0.230386257127053\n0.409766454352442,0.230933518343162\n0.410615711252654,0.231563819685064\n0.411889596602972,0.232556015252596\n0.412738853503185,0.233110241124417\n0.413588110403397,0.233690986262217\n0.414861995753716,0.234437275908854\n0.415711252653928,0.235135884451004\n0.416985138004246,0.236150313342539\n0.417834394904459,0.236761060074174\n0.418683651804671,0.237269747812495\n0.419957537154989,0.238466061366653\n0.420806794055202,0.239002607727821\n0.421656050955414,0.239643891796037\n0.422929936305733,0.240488490236758\n0.423779193205945,0.24100173178843\n0.424628450106157,0.24154149260608\n0.425902335456476,0.242455737603917\n0.426751592356688,0.243218099532966\n0.4276008492569,0.243749556338038\n0.428874734607219,0.244753538246005\n0.429723991507431,0.245402054841306\n0.430997876857749,0.246328354050952\n0.431847133757962,0.246996960999267\n0.432696390658174,0.247551454742462\n0.433970276008493,0.24842096522092\n0.434819532908705,0.249157075755364\n0.435668789808917,0.249756839760684\n0.436942675159236,0.250652869505121\n0.437791932059448,0.251469609322995\n0.43864118895966,0.252061337187109\n0.439915074309979,0.253392122170776\n0.440764331210191,0.253996975732193\n0.441613588110403,0.254549058633026\n0.442887473460722,0.255554112026487\n0.443736730360934,0.256189770795859\n0.444585987261147,0.256951061239414\n0.445859872611465,0.257843608655996\n0.446709129511677,0.258501232877997\n0.447983014861996,0.259579147285057\n0.448832271762208,0.260186679560209\n0.44968152866242,0.260873501761924\n0.450955414012739,0.261998561530724\n0.451804670912951,0.262832712987877\n0.452653927813163,0.263533732372389\n0.453927813163482,0.264464317524011\n0.454777070063694,0.265200963801202\n0.455626326963907,0.265891536202146\n0.456900212314225,0.267034543352973\n0.457749469214437,0.268182640059896\n0.45859872611465,0.268800083575869\n0.459872611464968,0.269859782729529\n0.46072186836518,0.270491423428298\n0.461995753715499,0.27140379332652\n0.462845010615711,0.272075346859945\n0.463694267515924,0.272690111662182\n0.464968152866242,0.273704272682343\n0.465817409766454,0.274264659595755\n0.466666666666667,0.274974518735594\n0.467940552016985,0.276121276085649\n0.468789808917197,0.276674430471976\n0.46963906581741,0.277257854323511\n0.470912951167728,0.278079415826109\n0.471762208067941,0.278791417936935\n0.472611464968153,0.279780666919359\n0.473885350318471,0.28077125525865\n0.474734607218684,0.281410664227251\n0.475583864118896,0.281963014999458\n0.476857749469214,0.283002891671477\n0.477707006369427,0.283518276194137\n0.478980891719745,0.284604226742403\n0.479830148619958,0.285239349769028\n0.48067940552017,0.28575339493482\n0.481953290870488,0.286825684043036\n0.482802547770701,0.287611618652955\n0.483651804670913,0.288231740882663\n0.484925690021231,0.289471181727958\n0.485774946921444,0.290248544453924\n0.486624203821656,0.290831164691339\n0.487898089171975,0.291669334219095\n0.488747346072187,0.292389372471127\n0.489596602972399,0.292939848143718\n0.490870488322718,0.294017494679405\n0.49171974522293,0.294547879998982\n0.492993630573248,0.295689547792941\n0.493842887473461,0.296233594552568\n0.494692144373673,0.297054620312418\n0.495966029723992,0.298189859193413\n0.496815286624204,0.298882574565345\n0.497664543524416,0.299643061394779\n0.498938428874735,0.300589986700186\n0.499787685774947,0.301226984826426\n0.500636942675159,0.301935236738023\n0.501910828025478,0.302680990641914\n0.50276008492569,0.303557733647457\n0.503609341825902,0.304133924971907\n0.504883227176221,0.305044151899141\n0.505732484076433,0.30613867433136\n0.506581740976645,0.306613878147991\n0.507855626326964,0.307482317140955\n0.508704883227176,0.308187890338817\n0.509978768577495,0.309348309128922\n0.510828025477707,0.309994147010488\n0.511677282377919,0.310734007744161\n0.512951167728238,0.311616376048549\n0.51380042462845,0.312374452035622\n0.514649681528662,0.313091811573918\n0.515923566878981,0.314129009532203\n0.516772823779193,0.3148463690705\n0.517622080679406,0.315537745085565\n0.518895966029724,0.316589675969393\n0.519745222929936,0.317782507195695\n0.520594479830149,0.318537368726285\n0.521868365180467,0.31956626267199\n0.522717622080679,0.320240762790523\n0.523991507430998,0.321584405600119\n0.52484076433121,0.322180955148957\n0.525690021231422,0.322751789045936\n0.526963906581741,0.323794880174438\n0.527813163481953,0.324413395175904\n0.528662420382166,0.324894492162753\n0.529936305732484,0.325966245528222\n0.530785562632696,0.326699945220304\n0.531634819532909,0.327316317250783\n0.532908704883227,0.328245295174163\n0.533757961783439,0.328956225799496\n0.534607218683652,0.329490629189676\n0.53588110403397,0.330674888532025\n0.536730360934183,0.33147421671062\n0.537579617834395,0.33222827462709\n0.538853503184713,0.333113053773839\n0.539702760084926,0.333880773130358\n0.540976645435244,0.33502645899492\n0.541825902335456,0.335722924566082\n0.542675159235669,0.336370369675889\n0.543949044585987,0.337460874037505\n0.5447983014862,0.33830734757784\n0.545647558386412,0.338908183068654\n0.54692144373673,0.340495053085409\n0.547770700636943,0.341224466835515\n0.548619957537155,0.342032099026689\n0.549893842887473,0.343141890127199\n0.550743099787686,0.343694240899405\n0.551592356687898,0.344402492811002\n0.552866242038217,0.345305755082524\n0.553715498938429,0.346153835851101\n0.554989384288747,0.347190230195265\n0.55583864118896,0.34793785919877\n0.556687898089172,0.348577268167371\n0.55796178343949,0.349562499079192\n0.558811040339703,0.35031869996665\n0.559660297239915,0.350978199288266\n0.560934182590234,0.352149868676059\n0.561783439490446,0.353023932967867\n0.562632696390658,0.354101043760807\n0.563906581740977,0.355136098748103\n0.564755838641189,0.35577363261709\n0.565605095541401,0.356664840676804\n0.56687898089172,0.357702842249209\n0.567728237791932,0.358456364422931\n0.568577494692144,0.359440791720632\n0.569851380042463,0.360616211307654\n0.570700636942675,0.361465363561725\n0.571974522292994,0.362300586504372\n0.572823779193206,0.36317304356794\n0.573673036093418,0.363937280596603\n0.574946921443737,0.365288423804657\n0.575796178343949,0.366091770053856\n0.576645435244161,0.366820915932587\n0.57791932059448,0.367966601797149\n0.578768577494692,0.369070499727441\n0.579617834394904,0.3698944720724\n0.580891719745223,0.371014442285104\n0.581740976645435,0.371645011498379\n0.582590233545648,0.372389961788149\n0.583864118895966,0.373255989938751\n0.584713375796178,0.373968259920951\n0.585987261146497,0.375025816103623\n0.586836518046709,0.375754426239608\n0.587685774946921,0.376684743519857\n0.58895966029724,0.377813017745139\n0.589808917197452,0.378436622302703\n0.590658174097665,0.379171929223026\n0.591932059447983,0.380138141267326\n0.592781316348195,0.380781032563784\n0.593630573248408,0.381591343468693\n0.594904458598726,0.382595057505287\n0.595753715498938,0.383276522279532\n0.596602972399151,0.384230947983397\n0.597876857749469,0.385563340195305\n0.598726114649682,0.386376865556697\n0.599575371549894,0.387081903011812\n0.600849256900212,0.388382150658897\n0.601698513800425,0.389164870812333\n0.602972399150743,0.390592893104589\n0.603821656050955,0.391511691915776\n0.604670912951168,0.392238159080773\n0.605944798301486,0.394239158240996\n0.606794055201698,0.394924908957217\n0.607643312101911,0.395720219065209\n0.608917197452229,0.396613570095911\n0.609766454352442,0.397333876219317\n0.610615711252654,0.398114453401766\n0.611889596602972,0.399269782635774\n0.612738853503185,0.399972141377154\n0.613588110403397,0.400635926640746\n0.614861995753715,0.401700983221877\n0.615711252653928,0.402736841823293\n0.616985138004246,0.403937709190801\n0.617834394904459,0.404724715286214\n0.618683651804671,0.405637888798557\n0.619957537154989,0.406895009154504\n0.620806794055202,0.407552365505132\n0.621656050955414,0.40845000247781\n0.622929936305732,0.409656763015535\n0.623779193205945,0.410384301666026\n0.624628450106157,0.411088267635647\n0.625902335456476,0.412255115338717\n0.626751592356688,0.413088998924496\n0.6276008492569,0.413727872150351\n0.628874734607219,0.414865254002333\n0.629723991507431,0.415680386591966\n0.630997876857749,0.416701244396466\n0.631847133757962,0.417417800320642\n0.632696390658174,0.418257844948013\n0.633970276008493,0.419668187729616\n0.634819532908705,0.420635739130784\n0.635668789808917,0.421315328805414\n0.636942675159236,0.422599772041461\n0.637791932059448,0.423435798598229\n0.63864118895966,0.424328346014811\n0.639915074309979,0.425895929292672\n0.640764331210191,0.426845801183188\n0.641613588110403,0.427588340630596\n0.642887473460722,0.428951538050459\n0.643736730360934,0.43034955874888\n0.644585987261146,0.430995128759072\n0.645859872611465,0.432109473672932\n0.646709129511677,0.432872639216101\n0.647983014861996,0.434058773658065\n0.648832271762208,0.434954535531129\n0.64968152866242,0.435932801787238\n0.650955414012739,0.437135276382986\n0.651804670912951,0.437785132335155\n0.652653927813163,0.438662678954819\n0.653927813163482,0.439788274466367\n0.654777070063694,0.440558136793874\n0.655626326963907,0.441334963777094\n0.656900212314225,0.442547349613662\n0.657749469214437,0.44334989224874\n0.65859872611465,0.444153238497938\n0.659872611464968,0.445482416253363\n0.66072186836518,0.446172988654308\n0.661995753715499,0.447170273777937\n0.662845010615711,0.448322388555463\n0.663694267515924,0.449257527520435\n0.664968152866242,0.450892078641678\n0.665817409766454,0.451781947344524\n0.666666666666667,0.453559541779228\n0.667940552016985,0.455302848678122\n0.668789808917197,0.455919756451347\n0.66963906581741,0.456815786195785\n0.670912951167728,0.458138802909619\n0.671762208067941,0.459099925397822\n0.672611464968153,0.460069619769979\n0.673885350318471,0.461365849346461\n0.674734607218684,0.462220091156628\n0.675583864118896,0.463047010086697\n0.676857749469214,0.464154122473471\n0.677707006369427,0.464958808079536\n0.678980891719745,0.466298968561276\n0.679830148619957,0.467009095572488\n0.68067940552017,0.467881820507429\n0.681953290870488,0.46927609100662\n0.682802547770701,0.470470529461163\n0.683651804670913,0.471287001407664\n0.684925690021231,0.472577337813929\n0.685774946921444,0.473438544279808\n0.686624203821656,0.47422019294775\n0.687898089171974,0.475907782600951\n0.688747346072187,0.47666184051742\n0.689596602972399,0.477251157539173\n0.690870488322718,0.478836420327687\n0.69171974522293,0.480080682857705\n0.692993630573248,0.48153067060259\n0.693842887473461,0.482465541696188\n0.694692144373673,0.483145399242192\n0.695966029723991,0.484345462995579\n0.696815286624204,0.485393375808804\n0.697664543524416,0.486522185776834\n0.698938428874735,0.48848541507339\n0.699787685774947,0.489227150906678\n0.700636942675159,0.490120769808753\n0.701910828025478,0.492060158553066\n0.70276008492569,0.492800287158112\n0.703609341825902,0.493758730932581\n0.704883227176221,0.494838788310629\n0.705732484076433,0.495686333336459\n0.706581740976645,0.496707726883705\n0.707855626326964,0.498140302989311\n0.708704883227176,0.498999902226949\n0.709978768577495,0.500308721757986\n0.710828025477707,0.501509589125494\n0.711677282377919,0.502442317248104\n0.712951167728238,0.503797746398135\n0.71380042462845,0.504604842846562\n0.714649681528662,0.50562784362205\n0.715923566878981,0.506897286061179\n0.716772823779193,0.508162442558332\n0.717622080679405,0.508768367605243\n0.718895966029724,0.510149244535759\n0.719745222929936,0.511121349750277\n0.720594479830149,0.511981484730662\n0.721868365180467,0.513448884114826\n0.722717622080679,0.514237765309854\n0.723991507430998,0.515398719842706\n0.72484076433121,0.516210637975857\n0.725690021231422,0.517078809097448\n0.726963906581741,0.518798811186844\n0.727813163481953,0.52008084358053\n0.728662420382166,0.520983570109305\n0.729936305732484,0.522468649004121\n0.730785562632696,0.523488971065874\n0.731634819532909,0.524367321299658\n0.732908704883227,0.525764002641212\n0.733757961783439,0.526838702591789\n0.734607218683652,0.527606421948309\n0.73588110403397,0.529186327309353\n0.736730360934183,0.530126823701795\n0.737579617834395,0.530910883212099\n0.738853503184713,0.53255748854515\n0.739702760084926,0.533697816982242\n0.740976645435244,0.535468446761234\n0.741825902335456,0.536522520616051\n0.742675159235669,0.537448284082949\n0.743949044585987,0.538830232498959\n0.7447983014862,0.539956363753253\n0.745647558386412,0.541192054399319\n0.74692144373673,0.543027776922078\n0.747770700636943,0.543830319557155\n0.748619957537155,0.544803496257167\n0.749893842887473,0.546371347406402\n0.750743099787686,0.547378811642226\n0.751592356687898,0.548220999240585\n0.752866242038217,0.549953859155911\n0.753715498938429,0.550946322594816\n0.754989384288747,0.552440241244959\n0.75583864118896,0.553783884054555\n0.756687898089172,0.554478742397476\n0.75796178343949,0.555901407262261\n0.758811040339703,0.556800919334554\n0.759660297239915,0.557668018970651\n0.760934182590234,0.559338196984572\n0.761783439490446,0.560455220612166\n0.762632696390658,0.561581083995087\n0.763906581740977,0.563113576123017\n0.764755838641189,0.564043357660519\n0.765605095541401,0.564882062931022\n0.76687898089172,0.566450985565751\n0.767728237791932,0.567613011584097\n0.768577494692144,0.568860220699225\n0.769851380042463,0.570191809297012\n0.770700636942675,0.571049801306409\n0.771974522292994,0.572720247191703\n0.772823779193206,0.573430909945662\n0.773673036093418,0.574245238921175\n0.774946921443737,0.575806929028819\n0.775796178343949,0.57700886788182\n0.776645435244161,0.577867931376712\n0.77791932059448,0.57953864513338\n0.778768577494692,0.580564592493976\n0.779617834394905,0.581796532940812\n0.780891719745223,0.582925342908842\n0.781740976645435,0.584135050031676\n0.782590233545648,0.585271360398164\n0.783864118895966,0.587314415364031\n0.784713375796178,0.588444564688928\n0.785987261146497,0.590064382884626\n0.786836518046709,0.591047738696833\n0.787685774946921,0.592657109908964\n0.78895966029724,0.594057541449747\n0.789808917197452,0.595222781924575\n0.790658174097665,0.595958892459019\n0.791932059447983,0.597650232311449\n0.792781316348195,0.598566620280274\n0.793630573248408,0.599751951108117\n0.794904458598726,0.601242923173151\n0.795753715498938,0.602312265696258\n0.796602972399151,0.603304729135164\n0.797876857749469,0.604609530595599\n0.798726114649682,0.605668426135138\n0.799575371549894,0.607000014732926\n0.800849256900212,0.608620636542745\n0.801698513800425,0.6097947167729\n0.802972399150743,0.611579811606063\n0.803821656050955,0.612858629543266\n0.804670912951168,0.614136911737722\n0.805944798301486,0.615817804606584\n0.806794055201699,0.616674457259113\n0.807643312101911,0.617596470526782\n0.808917197452229,0.619507464905502\n0.809766454352442,0.620531001423736\n0.810615711252654,0.621908128155022\n0.811889596602972,0.62349365881491\n0.812738853503185,0.624264592627912\n0.813588110403397,0.625560286461647\n0.814861995753716,0.626985362168794\n0.815711252653928,0.627987468977147\n0.816985138004246,0.629767474254213\n0.817834394904459,0.633343289219383\n0.818683651804671,0.634261552287822\n0.819957537154989,0.63592235480367\n0.820806794055202,0.636828027917555\n0.821656050955414,0.637622534411426\n0.822929936305733,0.639034752292644\n0.823779193205945,0.640204814452196\n0.824628450106157,0.641475596248194\n0.825902335456476,0.643146845747609\n0.826751592356688,0.64423092119626\n0.8276008492569,0.645277762523991\n0.828874734607219,0.646782931771822\n0.829723991507431,0.647881740146017\n0.83099787685775,0.649042962550243\n0.831847133757962,0.649970868988129\n0.832696390658174,0.651226649987209\n0.833970276008493,0.652772803555189\n0.834819532908705,0.653858754103455\n0.835668789808917,0.654800054110017\n0.836942675159236,0.656949721882546\n0.837791932059448,0.658512483475685\n0.83864118895966,0.65952316216799\n0.839915074309979,0.661076012520308\n0.840764331210191,0.661968827808263\n0.841613588110403,0.663141300810177\n0.842887473460722,0.664355561746361\n0.843736730360934,0.665357936426087\n0.844585987261147,0.666372097446248\n0.845859872611465,0.668127994299697\n0.846709129511677,0.669197872565552\n0.847983014861996,0.670752062274737\n0.848832271762208,0.672032755311554\n0.84968152866242,0.673215139554289\n0.850955414012739,0.675043629549963\n0.851804670912951,0.676110025487961\n0.852653927813164,0.677535369066482\n0.853927813163482,0.678950533532808\n0.854777070063694,0.680309980753442\n0.855626326963907,0.681466649344318\n0.856900212314225,0.683949013362763\n0.857749469214437,0.685196222477891\n0.85859872611465,0.686901759513117\n0.859872611464968,0.688479521903173\n0.86072186836518,0.689402874527709\n0.861995753715499,0.69139530180398\n0.862845010615711,0.692666887214098\n0.863694267515924,0.693826502390082\n0.864968152866242,0.695953133224488\n0.865817409766454,0.697397763541902\n0.866666666666667,0.698773283044947\n0.867940552016985,0.700935808643406\n0.868789808917197,0.701892913061006\n0.86963906581741,0.703302184357115\n0.870912951167728,0.704914770025729\n0.871762208067941,0.706380562181652\n0.872611464968153,0.707765189311396\n0.873885350318471,0.70936786373919\n0.874734607218684,0.710414437195547\n0.875583864118896,0.711539764835721\n0.876857749469214,0.713479689322781\n0.877707006369427,0.714582783638952\n0.878980891719745,0.716746380722905\n0.879830148619958,0.717936533235471\n0.88067940552017,0.718832295108535\n0.881953290870488,0.720613907613842\n0.882802547770701,0.721872635198031\n0.883651804670913,0.723337087997086\n0.884925690021231,0.725357373896202\n0.885774946921444,0.726431002361286\n0.886624203821656,0.727909920214511\n0.887898089171975,0.730301207965959\n0.888747346072187,0.731819235039719\n0.889596602972399,0.732780089656549\n0.890870488322718,0.734403658051477\n0.89171974522293,0.735781052654137\n0.892993630573248,0.737406496148679\n0.893842887473461,0.738326098573987\n0.894692144373673,0.739530716140724\n0.895966029723992,0.741317150330754\n0.896815286624204,0.742635077488492\n0.897664543524416,0.744445084359392\n0.898938428874735,0.746014542736869\n0.899787685774947,0.747167193257142\n0.900636942675159,0.748656022351187\n0.901910828025478,0.750945251109322\n0.90276008492569,0.752090133359763\n0.903609341825902,0.753663074065095\n0.904883227176221,0.755455669296717\n0.905732484076433,0.756790472350987\n0.906581740976645,0.758021341312329\n0.907855626326964,0.759703841409432\n0.908704883227176,0.76112597053147\n0.909978768577495,0.763128041177188\n0.910828025477707,0.764613120072004\n0.911677282377919,0.765706838890102\n0.912951167728238,0.767811236400505\n0.91380042462845,0.769480342928932\n0.914649681528662,0.770942652756999\n0.915923566878981,0.773047318138776\n0.916772823779193,0.774490876970696\n0.917622080679406,0.776074532530969\n0.918895966029724,0.778341260093728\n0.919745222929936,0.779685170774697\n0.920594479830149,0.780930236918836\n0.921868365180467,0.783132943223323\n0.922717622080679,0.784466674792098\n0.923991507430998,0.786370972386479\n0.92484076433121,0.787418885199705\n0.925690021231423,0.789039774880898\n0.926963906581741,0.791566069804601\n0.927813163481953,0.79362680428112\n0.928662420382166,0.794998573584936\n0.929936305732484,0.7974686155202\n0.930785562632696,0.799993838958409\n0.931634819532909,0.801745449869881\n0.932908704883227,0.804234778544039\n0.93375796178344,0.806151398221602\n0.934607218683652,0.807696212432714\n0.93588110403397,0.810237508153335\n0.936730360934183,0.811876613087927\n0.937579617834395,0.813802072520817\n0.938853503184713,0.81617326991925\n0.939702760084926,0.817863538286186\n0.940976645435244,0.820281077432239\n0.941825902335456,0.821321489847005\n0.942675159235669,0.822229841674625\n0.943949044585987,0.824694526182418\n0.9447983014862,0.826651594437383\n0.945647558386412,0.828736973080266\n0.94692144373673,0.831674182690955\n0.947770700636943,0.833227836657394\n0.948619957537155,0.835232318145473\n0.949893842887473,0.836782757655429\n0.950743099787686,0.838107381597505\n0.951592356687898,0.839980606112557\n0.952866242038217,0.842762450326602\n0.953715498938429,0.844740680420076\n0.954989384288747,0.847400911030541\n0.95583864118896,0.848976262578235\n0.956687898089172,0.851106911483244\n0.95796178343949,0.854280651516755\n0.958811040339703,0.856115570425393\n0.959660297239915,0.857975937114516\n0.960934182590234,0.860365885509096\n0.961783439490446,0.861165213687691\n0.962632696390658,0.863184963844061\n0.963906581740977,0.865946717705092\n0.964755838641189,0.868364524722519\n0.965605095541401,0.870232123938727\n0.96687898089172,0.873334878058254\n0.967728237791932,0.875230335897309\n0.968577494692144,0.87777163161793\n0.969851380042463,0.880948853979296\n0.970700636942675,0.883875080863671\n0.971974522292994,0.888120306391277\n0.972823779193206,0.890383015883433\n0.973673036093418,0.892769481950157\n0.974946921443737,0.896192074489672\n0.975796178343949,0.898725869811834\n0.976645435244161,0.901652364567582\n0.97791932059448,0.905171926544312\n0.978768577494692,0.907471602286014\n0.979617834394904,0.910228534462322\n0.980891719745223,0.913648448288101\n0.981740976645435,0.916270105420779\n0.982590233545648,0.919188296163948\n0.983864118895966,0.923467273484618\n0.984713375796178,0.926406358194922\n0.985987261146497,0.931760571208917\n0.986836518046709,0.936421800979606\n0.987685774946921,0.938815499573415\n0.98895966029724,0.94389835888603\n0.989808917197452,0.946748778171698\n0.990658174097665,0.949899749138459\n0.991932059447983,0.954675092114269\n0.992781316348195,0.958509938697636\n0.993630573248408,0.962253441142632\n0.994904458598726,0.967768376980741\n0.995753715498938,0.972670155244855\n0.996602972399151,0.976607864435655\n0.997876857749469,0.984152193799581\n0.998726114649682,0.988930751231873\n0.999575371549894,0.996937694457875"
    },
    {
      "name": "ModelComparisonData1",
      "format": {
        "type": "csv",
        "parse": {
          "ValidationWeight": "number"
        }
      },
      "values": "\"ValidationWeight\"\n0\n0.000849256900212314\n0.00169851380042463\n0.0029723991507431\n0.00382165605095541\n0.00467091295116773\n0.0059447983014862\n0.00679405520169851\n0.00764331210191083\n0.0089171974522293\n0.00976645435244161\n0.0106157112526539\n0.0118895966029724\n0.0127388535031847\n0.013588110403397\n0.0148619957537155\n0.0157112526539278\n0.0169851380042463\n0.0178343949044586\n0.0186836518046709\n0.0199575371549894\n0.0208067940552017\n0.021656050955414\n0.0229299363057325\n0.0237791932059448\n0.0246284501061571\n0.0259023354564756\n0.0267515923566879\n0.0276008492569002\n0.0288747346072187\n0.029723991507431\n0.0309978768577495\n0.0318471337579618\n0.0326963906581741\n0.0339702760084926\n0.0348195329087049\n0.0356687898089172\n0.0369426751592357\n0.037791932059448\n0.0386411889596603\n0.0399150743099788\n0.0407643312101911\n0.0416135881104034\n0.0428874734607219\n0.0437367303609342\n0.0445859872611465\n0.045859872611465\n0.0467091295116773\n0.0479830148619958\n0.0488322717622081\n0.0496815286624204\n0.0509554140127389\n0.0518046709129512\n0.0526539278131635\n0.053927813163482\n0.0547770700636943\n0.0556263269639066\n0.0569002123142251\n0.0577494692144374\n0.0585987261146497\n0.0598726114649682\n0.0607218683651805\n0.0619957537154989\n0.0628450106157113\n0.0636942675159236\n0.064968152866242\n0.0658174097664544\n0.0666666666666667\n0.0679405520169851\n0.0687898089171975\n0.0696390658174098\n0.0709129511677282\n0.0717622080679405\n0.0726114649681529\n0.0738853503184713\n0.0747346072186836\n0.075583864118896\n0.0768577494692144\n0.0777070063694267\n0.0789808917197452\n0.0798301486199575\n0.0806794055201698\n0.0819532908704883\n0.0828025477707006\n0.0836518046709129\n0.0849256900212314\n0.0857749469214437\n0.086624203821656\n0.0878980891719745\n0.0887473460721868\n0.0895966029723991\n0.0908704883227176\n0.0917197452229299\n0.0929936305732484\n0.0938428874734607\n0.094692144373673\n0.0959660297239915\n0.0968152866242038\n0.0976645435244161\n0.0989384288747346\n0.0997876857749469\n0.100636942675159\n0.101910828025478\n0.10276008492569\n0.103609341825902\n0.104883227176221\n0.105732484076433\n0.106581740976645\n0.107855626326964\n0.108704883227176\n0.109978768577495\n0.110828025477707\n0.111677282377919\n0.112951167728238\n0.11380042462845\n0.114649681528662\n0.115923566878981\n0.116772823779193\n0.117622080679406\n0.118895966029724\n0.119745222929936\n0.120594479830149\n0.121868365180467\n0.122717622080679\n0.123991507430998\n0.12484076433121\n0.125690021231423\n0.126963906581741\n0.127813163481953\n0.128662420382166\n0.129936305732484\n0.130785562632696\n0.131634819532909\n0.132908704883227\n0.133757961783439\n0.134607218683652\n0.13588110403397\n0.136730360934183\n0.137579617834395\n0.138853503184713\n0.139702760084926\n0.140976645435244\n0.141825902335456\n0.142675159235669\n0.143949044585987\n0.1447983014862\n0.145647558386412\n0.14692144373673\n0.147770700636943\n0.148619957537155\n0.149893842887473\n0.150743099787686\n0.151592356687898\n0.152866242038217\n0.153715498938429\n0.154989384288747\n0.15583864118896\n0.156687898089172\n0.15796178343949\n0.158811040339703\n0.159660297239915\n0.160934182590234\n0.161783439490446\n0.162632696390658\n0.163906581740977\n0.164755838641189\n0.165605095541401\n0.16687898089172\n0.167728237791932\n0.168577494692144\n0.169851380042463\n0.170700636942675\n0.171974522292994\n0.172823779193206\n0.173673036093418\n0.174946921443737\n0.175796178343949\n0.176645435244161\n0.17791932059448\n0.178768577494692\n0.179617834394904\n0.180891719745223\n0.181740976645435\n0.182590233545648\n0.183864118895966\n0.184713375796178\n0.185987261146497\n0.186836518046709\n0.187685774946921\n0.18895966029724\n0.189808917197452\n0.190658174097665\n0.191932059447983\n0.192781316348195\n0.193630573248408\n0.194904458598726\n0.195753715498938\n0.196602972399151\n0.197876857749469\n0.198726114649682\n0.199575371549894\n0.200849256900212\n0.201698513800425\n0.202972399150743\n0.203821656050955\n0.204670912951168\n0.205944798301486\n0.206794055201699\n0.207643312101911\n0.208917197452229\n0.209766454352442\n0.210615711252654\n0.211889596602972\n0.212738853503185\n0.213588110403397\n0.214861995753716\n0.215711252653928\n0.216985138004246\n0.217834394904459\n0.218683651804671\n0.219957537154989\n0.220806794055202\n0.221656050955414\n0.222929936305732\n0.223779193205945\n0.224628450106157\n0.225902335456476\n0.226751592356688\n0.2276008492569\n0.228874734607219\n0.229723991507431\n0.230997876857749\n0.231847133757962\n0.232696390658174\n0.233970276008493\n0.234819532908705\n0.235668789808917\n0.236942675159236\n0.237791932059448\n0.23864118895966\n0.239915074309979\n0.240764331210191\n0.241613588110403\n0.242887473460722\n0.243736730360934\n0.244585987261146\n0.245859872611465\n0.246709129511677\n0.247983014861996\n0.248832271762208\n0.24968152866242\n0.250955414012739\n0.251804670912951\n0.252653927813163\n0.253927813163482\n0.254777070063694\n0.255626326963907\n0.256900212314225\n0.257749469214437\n0.25859872611465\n0.259872611464968\n0.26072186836518\n0.261995753715499\n0.262845010615711\n0.263694267515924\n0.264968152866242\n0.265817409766454\n0.266666666666667\n0.267940552016985\n0.268789808917197\n0.26963906581741\n0.270912951167728\n0.271762208067941\n0.272611464968153\n0.273885350318471\n0.274734607218684\n0.275583864118896\n0.276857749469214\n0.277707006369427\n0.278980891719745\n0.279830148619958\n0.28067940552017\n0.281953290870488\n0.282802547770701\n0.283651804670913\n0.284925690021231\n0.285774946921444\n0.286624203821656\n0.287898089171975\n0.288747346072187\n0.289596602972399\n0.290870488322718\n0.29171974522293\n0.292993630573248\n0.293842887473461\n0.294692144373673\n0.295966029723991\n0.296815286624204\n0.297664543524416\n0.298938428874735\n0.299787685774947\n0.300636942675159\n0.301910828025478\n0.30276008492569\n0.303609341825902\n0.304883227176221\n0.305732484076433\n0.306581740976645\n0.307855626326964\n0.308704883227176\n0.309978768577495\n0.310828025477707\n0.311677282377919\n0.312951167728238\n0.31380042462845\n0.314649681528662\n0.315923566878981\n0.316772823779193\n0.317622080679406\n0.318895966029724\n0.319745222929936\n0.320594479830149\n0.321868365180467\n0.322717622080679\n0.323991507430998\n0.32484076433121\n0.325690021231422\n0.326963906581741\n0.327813163481953\n0.328662420382166\n0.329936305732484\n0.330785562632696\n0.331634819532909\n0.332908704883227\n0.333757961783439\n0.334607218683652\n0.33588110403397\n0.336730360934183\n0.337579617834395\n0.338853503184713\n0.339702760084926\n0.340976645435244\n0.341825902335456\n0.342675159235669\n0.343949044585987\n0.3447983014862\n0.345647558386412\n0.34692144373673\n0.347770700636943\n0.348619957537155\n0.349893842887473\n0.350743099787686\n0.351592356687898\n0.352866242038217\n0.353715498938429\n0.354989384288747\n0.35583864118896\n0.356687898089172\n0.35796178343949\n0.358811040339703\n0.359660297239915\n0.360934182590234\n0.361783439490446\n0.362632696390658\n0.363906581740977\n0.364755838641189\n0.365605095541401\n0.36687898089172\n0.367728237791932\n0.368577494692144\n0.369851380042463\n0.370700636942675\n0.371974522292994\n0.372823779193206\n0.373673036093418\n0.374946921443737\n0.375796178343949\n0.376645435244161\n0.37791932059448\n0.378768577494692\n0.379617834394904\n0.380891719745223\n0.381740976645435\n0.382590233545648\n0.383864118895966\n0.384713375796178\n0.385987261146497\n0.386836518046709\n0.387685774946921\n0.38895966029724\n0.389808917197452\n0.390658174097665\n0.391932059447983\n0.392781316348195\n0.393630573248408\n0.394904458598726\n0.395753715498938\n0.396602972399151\n0.397876857749469\n0.398726114649682\n0.399575371549894\n0.400849256900212\n0.401698513800425\n0.402972399150743\n0.403821656050955\n0.404670912951168\n0.405944798301486\n0.406794055201699\n0.407643312101911\n0.408917197452229\n0.409766454352442\n0.410615711252654\n0.411889596602972\n0.412738853503185\n0.413588110403397\n0.414861995753716\n0.415711252653928\n0.416985138004246\n0.417834394904459\n0.418683651804671\n0.419957537154989\n0.420806794055202\n0.421656050955414\n0.422929936305733\n0.423779193205945\n0.424628450106157\n0.425902335456476\n0.426751592356688\n0.4276008492569\n0.428874734607219\n0.429723991507431\n0.430997876857749\n0.431847133757962\n0.432696390658174\n0.433970276008493\n0.434819532908705\n0.435668789808917\n0.436942675159236\n0.437791932059448\n0.43864118895966\n0.439915074309979\n0.440764331210191\n0.441613588110403\n0.442887473460722\n0.443736730360934\n0.444585987261147\n0.445859872611465\n0.446709129511677\n0.447983014861996\n0.448832271762208\n0.44968152866242\n0.450955414012739\n0.451804670912951\n0.452653927813163\n0.453927813163482\n0.454777070063694\n0.455626326963907\n0.456900212314225\n0.457749469214437\n0.45859872611465\n0.459872611464968\n0.46072186836518\n0.461995753715499\n0.462845010615711\n0.463694267515924\n0.464968152866242\n0.465817409766454\n0.466666666666667\n0.467940552016985\n0.468789808917197\n0.46963906581741\n0.470912951167728\n0.471762208067941\n0.472611464968153\n0.473885350318471\n0.474734607218684\n0.475583864118896\n0.476857749469214\n0.477707006369427\n0.478980891719745\n0.479830148619958\n0.48067940552017\n0.481953290870488\n0.482802547770701\n0.483651804670913\n0.484925690021231\n0.485774946921444\n0.486624203821656\n0.487898089171975\n0.488747346072187\n0.489596602972399\n0.490870488322718\n0.49171974522293\n0.492993630573248\n0.493842887473461\n0.494692144373673\n0.495966029723992\n0.496815286624204\n0.497664543524416\n0.498938428874735\n0.499787685774947\n0.500636942675159\n0.501910828025478\n0.50276008492569\n0.503609341825902\n0.504883227176221\n0.505732484076433\n0.506581740976645\n0.507855626326964\n0.508704883227176\n0.509978768577495\n0.510828025477707\n0.511677282377919\n0.512951167728238\n0.51380042462845\n0.514649681528662\n0.515923566878981\n0.516772823779193\n0.517622080679406\n0.518895966029724\n0.519745222929936\n0.520594479830149\n0.521868365180467\n0.522717622080679\n0.523991507430998\n0.52484076433121\n0.525690021231422\n0.526963906581741\n0.527813163481953\n0.528662420382166\n0.529936305732484\n0.530785562632696\n0.531634819532909\n0.532908704883227\n0.533757961783439\n0.534607218683652\n0.53588110403397\n0.536730360934183\n0.537579617834395\n0.538853503184713\n0.539702760084926\n0.540976645435244\n0.541825902335456\n0.542675159235669\n0.543949044585987\n0.5447983014862\n0.545647558386412\n0.54692144373673\n0.547770700636943\n0.548619957537155\n0.549893842887473\n0.550743099787686\n0.551592356687898\n0.552866242038217\n0.553715498938429\n0.554989384288747\n0.55583864118896\n0.556687898089172\n0.55796178343949\n0.558811040339703\n0.559660297239915\n0.560934182590234\n0.561783439490446\n0.562632696390658\n0.563906581740977\n0.564755838641189\n0.565605095541401\n0.56687898089172\n0.567728237791932\n0.568577494692144\n0.569851380042463\n0.570700636942675\n0.571974522292994\n0.572823779193206\n0.573673036093418\n0.574946921443737\n0.575796178343949\n0.576645435244161\n0.57791932059448\n0.578768577494692\n0.579617834394904\n0.580891719745223\n0.581740976645435\n0.582590233545648\n0.583864118895966\n0.584713375796178\n0.585987261146497\n0.586836518046709\n0.587685774946921\n0.58895966029724\n0.589808917197452\n0.590658174097665\n0.591932059447983\n0.592781316348195\n0.593630573248408\n0.594904458598726\n0.595753715498938\n0.596602972399151\n0.597876857749469\n0.598726114649682\n0.599575371549894\n0.600849256900212\n0.601698513800425\n0.602972399150743\n0.603821656050955\n0.604670912951168\n0.605944798301486\n0.606794055201698\n0.607643312101911\n0.608917197452229\n0.609766454352442\n0.610615711252654\n0.611889596602972\n0.612738853503185\n0.613588110403397\n0.614861995753715\n0.615711252653928\n0.616985138004246\n0.617834394904459\n0.618683651804671\n0.619957537154989\n0.620806794055202\n0.621656050955414\n0.622929936305732\n0.623779193205945\n0.624628450106157\n0.625902335456476\n0.626751592356688\n0.6276008492569\n0.628874734607219\n0.629723991507431\n0.630997876857749\n0.631847133757962\n0.632696390658174\n0.633970276008493\n0.634819532908705\n0.635668789808917\n0.636942675159236\n0.637791932059448\n0.63864118895966\n0.639915074309979\n0.640764331210191\n0.641613588110403\n0.642887473460722\n0.643736730360934\n0.644585987261146\n0.645859872611465\n0.646709129511677\n0.647983014861996\n0.648832271762208\n0.64968152866242\n0.650955414012739\n0.651804670912951\n0.652653927813163\n0.653927813163482\n0.654777070063694\n0.655626326963907\n0.656900212314225\n0.657749469214437\n0.65859872611465\n0.659872611464968\n0.66072186836518\n0.661995753715499\n0.662845010615711\n0.663694267515924\n0.664968152866242\n0.665817409766454\n0.666666666666667\n0.667940552016985\n0.668789808917197\n0.66963906581741\n0.670912951167728\n0.671762208067941\n0.672611464968153\n0.673885350318471\n0.674734607218684\n0.675583864118896\n0.676857749469214\n0.677707006369427\n0.678980891719745\n0.679830148619957\n0.68067940552017\n0.681953290870488\n0.682802547770701\n0.683651804670913\n0.684925690021231\n0.685774946921444\n0.686624203821656\n0.687898089171974\n0.688747346072187\n0.689596602972399\n0.690870488322718\n0.69171974522293\n0.692993630573248\n0.693842887473461\n0.694692144373673\n0.695966029723991\n0.696815286624204\n0.697664543524416\n0.698938428874735\n0.699787685774947\n0.700636942675159\n0.701910828025478\n0.70276008492569\n0.703609341825902\n0.704883227176221\n0.705732484076433\n0.706581740976645\n0.707855626326964\n0.708704883227176\n0.709978768577495\n0.710828025477707\n0.711677282377919\n0.712951167728238\n0.71380042462845\n0.714649681528662\n0.715923566878981\n0.716772823779193\n0.717622080679405\n0.718895966029724\n0.719745222929936\n0.720594479830149\n0.721868365180467\n0.722717622080679\n0.723991507430998\n0.72484076433121\n0.725690021231422\n0.726963906581741\n0.727813163481953\n0.728662420382166\n0.729936305732484\n0.730785562632696\n0.731634819532909\n0.732908704883227\n0.733757961783439\n0.734607218683652\n0.73588110403397\n0.736730360934183\n0.737579617834395\n0.738853503184713\n0.739702760084926\n0.740976645435244\n0.741825902335456\n0.742675159235669\n0.743949044585987\n0.7447983014862\n0.745647558386412\n0.74692144373673\n0.747770700636943\n0.748619957537155\n0.749893842887473\n0.750743099787686\n0.751592356687898\n0.752866242038217\n0.753715498938429\n0.754989384288747\n0.75583864118896\n0.756687898089172\n0.75796178343949\n0.758811040339703\n0.759660297239915\n0.760934182590234\n0.761783439490446\n0.762632696390658\n0.763906581740977\n0.764755838641189\n0.765605095541401\n0.76687898089172\n0.767728237791932\n0.768577494692144\n0.769851380042463\n0.770700636942675\n0.771974522292994\n0.772823779193206\n0.773673036093418\n0.774946921443737\n0.775796178343949\n0.776645435244161\n0.77791932059448\n0.778768577494692\n0.779617834394905\n0.780891719745223\n0.781740976645435\n0.782590233545648\n0.783864118895966\n0.784713375796178\n0.785987261146497\n0.786836518046709\n0.787685774946921\n0.78895966029724\n0.789808917197452\n0.790658174097665\n0.791932059447983\n0.792781316348195\n0.793630573248408\n0.794904458598726\n0.795753715498938\n0.796602972399151\n0.797876857749469\n0.798726114649682\n0.799575371549894\n0.800849256900212\n0.801698513800425\n0.802972399150743\n0.803821656050955\n0.804670912951168\n0.805944798301486\n0.806794055201699\n0.807643312101911\n0.808917197452229\n0.809766454352442\n0.810615711252654\n0.811889596602972\n0.812738853503185\n0.813588110403397\n0.814861995753716\n0.815711252653928\n0.816985138004246\n0.817834394904459\n0.818683651804671\n0.819957537154989\n0.820806794055202\n0.821656050955414\n0.822929936305733\n0.823779193205945\n0.824628450106157\n0.825902335456476\n0.826751592356688\n0.8276008492569\n0.828874734607219\n0.829723991507431\n0.83099787685775\n0.831847133757962\n0.832696390658174\n0.833970276008493\n0.834819532908705\n0.835668789808917\n0.836942675159236\n0.837791932059448\n0.83864118895966\n0.839915074309979\n0.840764331210191\n0.841613588110403\n0.842887473460722\n0.843736730360934\n0.844585987261147\n0.845859872611465\n0.846709129511677\n0.847983014861996\n0.848832271762208\n0.84968152866242\n0.850955414012739\n0.851804670912951\n0.852653927813164\n0.853927813163482\n0.854777070063694\n0.855626326963907\n0.856900212314225\n0.857749469214437\n0.85859872611465\n0.859872611464968\n0.86072186836518\n0.861995753715499\n0.862845010615711\n0.863694267515924\n0.864968152866242\n0.865817409766454\n0.866666666666667\n0.867940552016985\n0.868789808917197\n0.86963906581741\n0.870912951167728\n0.871762208067941\n0.872611464968153\n0.873885350318471\n0.874734607218684\n0.875583864118896\n0.876857749469214\n0.877707006369427\n0.878980891719745\n0.879830148619958\n0.88067940552017\n0.881953290870488\n0.882802547770701\n0.883651804670913\n0.884925690021231\n0.885774946921444\n0.886624203821656\n0.887898089171975\n0.888747346072187\n0.889596602972399\n0.890870488322718\n0.89171974522293\n0.892993630573248\n0.893842887473461\n0.894692144373673\n0.895966029723992\n0.896815286624204\n0.897664543524416\n0.898938428874735\n0.899787685774947\n0.900636942675159\n0.901910828025478\n0.90276008492569\n0.903609341825902\n0.904883227176221\n0.905732484076433\n0.906581740976645\n0.907855626326964\n0.908704883227176\n0.909978768577495\n0.910828025477707\n0.911677282377919\n0.912951167728238\n0.91380042462845\n0.914649681528662\n0.915923566878981\n0.916772823779193\n0.917622080679406\n0.918895966029724\n0.919745222929936\n0.920594479830149\n0.921868365180467\n0.922717622080679\n0.923991507430998\n0.92484076433121\n0.925690021231423\n0.926963906581741\n0.927813163481953\n0.928662420382166\n0.929936305732484\n0.930785562632696\n0.931634819532909\n0.932908704883227\n0.93375796178344\n0.934607218683652\n0.93588110403397\n0.936730360934183\n0.937579617834395\n0.938853503184713\n0.939702760084926\n0.940976645435244\n0.941825902335456\n0.942675159235669\n0.943949044585987\n0.9447983014862\n0.945647558386412\n0.94692144373673\n0.947770700636943\n0.948619957537155\n0.949893842887473\n0.950743099787686\n0.951592356687898\n0.952866242038217\n0.953715498938429\n0.954989384288747\n0.95583864118896\n0.956687898089172\n0.95796178343949\n0.958811040339703\n0.959660297239915\n0.960934182590234\n0.961783439490446\n0.962632696390658\n0.963906581740977\n0.964755838641189\n0.965605095541401\n0.96687898089172\n0.967728237791932\n0.968577494692144\n0.969851380042463\n0.970700636942675\n0.971974522292994\n0.972823779193206\n0.973673036093418\n0.974946921443737\n0.975796178343949\n0.976645435244161\n0.97791932059448\n0.978768577494692\n0.979617834394904\n0.980891719745223\n0.981740976645435\n0.982590233545648\n0.983864118895966\n0.984713375796178\n0.985987261146497\n0.986836518046709\n0.987685774946921\n0.98895966029724\n0.989808917197452\n0.990658174097665\n0.991932059447983\n0.992781316348195\n0.993630573248408\n0.994904458598726\n0.995753715498938\n0.996602972399151\n0.997876857749469\n0.998726114649682\n0.999575371549894"
    },
    {
      "name": "ModelComparisonData2",
      "format": {
        "type": "csv",
        "parse": {
          "TrainWeight": "number",
          "TrainLoss": "number"
        }
      },
      "values": "\"TrainWeight\",\"TrainLoss\"\n0,0\n0.000849256900212314,0.00050905562678639\n0.00191082802547771,0.00115419408770571\n0.0029723991507431,0.00171765571298393\n0.00382165605095541,0.00217216966547178\n0.00488322717622081,0.00273183236816204\n0.0059447983014862,0.00324048096752829\n0.00679405520169851,0.00358631859883919\n0.00785562632696391,0.0041815283628882\n0.0089171974522293,0.00466616234446629\n0.00997876857749469,0.00526937698111136\n0.010828025477707,0.00567436926414904\n0.0118895966029724,0.00633280395412621\n0.0129511677282378,0.00678270492918582\n0.0138004246284501,0.00729732326404744\n0.0148619957537155,0.00781913241666483\n0.0159235668789809,0.0083258815547371\n0.0169851380042463,0.00882828906699456\n0.0178343949044586,0.00922269863710865\n0.018895966029724,0.00973229696712189\n0.0199575371549894,0.0102374179955136\n0.0208067940552017,0.0106373902737029\n0.0218683651804671,0.0111643551069754\n0.0229299363057325,0.0117138421241622\n0.0239915074309979,0.0122351085735527\n0.0248407643312102,0.0126829744115117\n0.0259023354564756,0.0131385737704532\n0.026963906581741,0.01368806078764\n0.0278131634819533,0.0140193811076326\n0.0288747346072187,0.0145993951813298\n0.0299363057324841,0.0151560730162724\n0.0309978768577495,0.0157278108657601\n0.0318471337579618,0.0161429788343012\n0.0329087048832272,0.0166802550288839\n0.0339702760084926,0.0171898533588971\n0.0348195329087049,0.0176223878306975\n0.0358811040339703,0.0181325288639376\n0.0369426751592357,0.0186476899020261\n0.037791932059448,0.0190673351721887\n0.0388535031847134,0.019707317952453\n0.0399150743099788,0.0203500142488514\n0.0409766454352442,0.0208749439450232\n0.0418259023354565,0.0213141265313525\n0.0428874734607219,0.0218600859775648\n0.0439490445859873,0.022553932053094\n0.0447983014861996,0.0229639443409801\n0.045859872611465,0.0234848037629505\n0.0469214437367304,0.024058034046312\n0.0479830148619958,0.0245935464554074\n0.0488322717622081,0.0250206538949393\n0.0498938428874735,0.0255888641734524\n0.0509554140127389,0.0261014473712133\n0.0518046709129512,0.0265840462156907\n0.0528662420382166,0.0271473721651623\n0.053927813163482,0.0277640187066719\n0.0549893842887473,0.0283257165464628\n0.0558386411889597,0.0287809088779842\n0.0569002123142251,0.0293881937888307\n0.0579617834394904,0.0299984635674248\n0.0588110403397028,0.0304904240425654\n0.0598726114649682,0.0310835986695137\n0.0609341825902335,0.0315862775333846\n0.0619957537154989,0.032196411636172\n0.0628450106157113,0.032665985603205\n0.0639065817409766,0.0332934862092517\n0.064968152866242,0.0338786559636039\n0.0658174097664544,0.0343270645047897\n0.0668789808917197,0.0349420829366188\n0.0679405520169851,0.0354766456150672\n0.0687898089171975,0.0359519179659821\n0.0698513800424628,0.0365666650461977\n0.0709129511677282,0.0370626957955397\n0.0719745222929936,0.0374903459382984\n0.0728237791932059,0.0379379404246439\n0.0738853503184713,0.0383586711012603\n0.0749469214437367,0.0388616213167446\n0.075796178343949,0.0392953768708054\n0.0768577494692144,0.03974052919263\n0.0779193205944798,0.0404000492890609\n0.0789808917197452,0.0409058486964862\n0.0798301486199575,0.0413069063811292\n0.0808917197452229,0.0417205819157965\n0.0819532908704883,0.0421982964312322\n0.0828025477707006,0.0425686913835582\n0.083864118895966,0.0431451778862808\n0.0849256900212314,0.043764402268118\n0.0859872611464968,0.0442695232965097\n0.0868365180467091,0.0447996086733366\n0.0878980891719745,0.0453575075905396\n0.0889596602972399,0.045940235180371\n0.0898089171974522,0.0463187706810997\n0.0908704883227176,0.0469290404596938\n0.091932059447983,0.0474170663364397\n0.0929936305732484,0.0479208306067643\n0.0938428874734607,0.0484058716157625\n0.0949044585987261,0.0488930834376682\n0.0959660297239915,0.0494196412435205\n0.0968152866242038,0.0497943778216612\n0.0978768577494692,0.0503395232130332\n0.0989384288747346,0.0508031274445708\n0.0997876857749469,0.0512268429889349\n0.100849256900212,0.0517270796882849\n0.101910828025478,0.0522578434441454\n0.102972399150743,0.0528270034533055\n0.103821656050955,0.0532789395654658\n0.104883227176221,0.0538395519988031\n0.105944798301486,0.0543212011126335\n0.106794055201699,0.0547359620537545\n0.107855626326964,0.0552889766419158\n0.108917197452229,0.0558764528849822\n0.109978768577495,0.0563811668859538\n0.110828025477707,0.056804339727091\n0.111889596602972,0.0572792050505858\n0.112951167728238,0.057812410970967\n0.11380042462845,0.0582453524701875\n0.114861995753716,0.0588374416906821\n0.115923566878981,0.0594348222676386\n0.116985138004246,0.0599520184428277\n0.117834394904459,0.0603722064162172\n0.118895966029724,0.0609168091043624\n0.119957537154989,0.0614686026102633\n0.120806794055202,0.0618797003046031\n0.121868365180467,0.0626430123931692\n0.122929936305732,0.0632167853797576\n0.123991507430998,0.0636133657627791\n0.12484076433121,0.0640496991571674\n0.125902335456476,0.0646316126921586\n0.126963906581741,0.0651995516190583\n0.127813163481953,0.0656432115070091\n0.128874734607219,0.0662039596161531\n0.129936305732484,0.0668078526318317\n0.130997876857749,0.0674882668024964\n0.131847133757962,0.0679149672146081\n0.132908704883227,0.0684152039139582\n0.133970276008493,0.0689374200939957\n0.134819532908705,0.0693095788318089\n0.13588110403397,0.0698486188118788\n0.136942675159236,0.0703896939290494\n0.137791932059448,0.0708162586653544\n0.138853503184713,0.071305369948554\n0.139915074309979,0.0718446812802374\n0.140976645435244,0.0723352849973108\n0.141825902335456,0.0727197902335349\n0.142887473460722,0.0732979048459381\n0.143949044585987,0.0739632590020576\n0.1447983014862,0.0744522346094505\n0.145859872611465,0.0750507005928606\n0.14692144373673,0.0756661260521098\n0.147983014861996,0.0763095007275419\n0.148832271762208,0.0767969839010609\n0.149893842887473,0.0772329102680291\n0.150955414012739,0.0777559405029069\n0.151804670912951,0.0782603831522651\n0.152866242038217,0.0788672610356914\n0.153927813163482,0.0794615210690934\n0.154989384288747,0.0801739547301423\n0.15583864118896,0.0805793540406001\n0.156900212314225,0.0810877312883529\n0.15796178343949,0.0817228297395755\n0.158811040339703,0.0822008156066247\n0.159872611464968,0.0827104139366379\n0.160934182590234,0.0833493113104484\n0.161995753715499,0.0838894366969721\n0.162845010615711,0.0843109814284287\n0.163906581740977,0.08482478570845\n0.164968152866242,0.0852506720657215\n0.165817409766454,0.0857929682651525\n0.16687898089172,0.0863634850323798\n0.167940552016985,0.0867714621831652\n0.168789808917197,0.0871196063031902\n0.169851380042463,0.0875937932476514\n0.170912951167728,0.088054548287248\n0.171974522292994,0.0885674028366224\n0.172823779193206,0.0888783717856081\n0.173885350318471,0.0893954322849905\n0.174946921443737,0.0899481755215384\n0.175796178343949,0.0903988905514383\n0.176857749469214,0.0910076678961586\n0.17791932059448,0.0916609469054807\n0.178980891719745,0.0922237301517254\n0.179830148619958,0.0927013089913544\n0.180891719745223,0.0931760386390424\n0.181953290870488,0.0936346228657317\n0.182802547770701,0.0939922642922266\n0.183864118895966,0.0945051188416009\n0.184925690021231,0.0950630177588039\n0.185987261146497,0.0956415393986273\n0.186836518046709,0.0960219743606499\n0.187898089171975,0.0966639922780149\n0.18895966029724,0.0972209414645709\n0.189808917197452,0.0977217208671478\n0.190870488322718,0.0983557339119166\n0.191932059447983,0.0989921891212063\n0.192993630573248,0.0995092496205888\n0.193842887473461,0.0999063727068371\n0.194904458598726,0.100361700714165\n0.195966029723992,0.100946327765291\n0.196815286624204,0.10144412230012\n0.197876857749469,0.102151807307934\n0.198938428874735,0.10266126996214\n0.199787685774947,0.103128266088846\n0.200849256900212,0.103727410451289\n0.201910828025478,0.1043663078251\n0.202972399150743,0.104946728926217\n0.203821656050955,0.105353620670549\n0.204883227176221,0.105829978427918\n0.205944798301486,0.106423831433899\n0.206794055201699,0.106886214583177\n0.207855626326964,0.107478575155285\n0.208917197452229,0.107988580512718\n0.209978768577495,0.10848583234432\n0.210828025477707,0.10888661867735\n0.211889596602972,0.109437598128411\n0.212951167728238,0.109959000253608\n0.21380042462845,0.110382851473779\n0.214861995753716,0.110989186653978\n0.215923566878981,0.111519000679192\n0.216985138004246,0.112084904468991\n0.217834394904459,0.112480942148785\n0.218895966029724,0.112990676154605\n0.219957537154989,0.113569740497655\n0.220806794055202,0.113956009519367\n0.221868365180467,0.114440507825138\n0.222929936305732,0.115029612177885\n0.223991507430998,0.115674886314611\n0.22484076433121,0.116084491575077\n0.225902335456476,0.116543618504993\n0.226963906581741,0.117087271462491\n0.227813163481953,0.117534458921417\n0.228874734607219,0.118117729214475\n0.229936305732484,0.118682411922013\n0.230997876857749,0.119223622714991\n0.231847133757962,0.119628479322222\n0.232908704883227,0.120277552381536\n0.233970276008493,0.120935580044093\n0.234819532908705,0.121255232244708\n0.23588110403397,0.121768493821503\n0.236942675159236,0.122311197048354\n0.237791932059448,0.122645773587708\n0.238853503184713,0.123243561192084\n0.239915074309979,0.123889785059457\n0.240976645435244,0.124223683219777\n0.241825902335456,0.124590957628549\n0.242887473460722,0.125099741903722\n0.243949044585987,0.12554475854974\n0.2447983014862,0.126057341747501\n0.245859872611465,0.126632064464736\n0.24692144373673,0.127285750501478\n0.247983014861996,0.127957345744707\n0.248832271762208,0.128479968952164\n0.249893842887473,0.129173408000273\n0.250955414012739,0.129737548004585\n0.251804670912951,0.130129244058565\n0.252866242038217,0.130580230440078\n0.253927813163482,0.131100818510435\n0.254989384288747,0.131615165493683\n0.25583864118896,0.1320379313074\n0.256900212314225,0.13260464915204\n0.25796178343949,0.133147216703084\n0.258811040339703,0.133567947379701\n0.259872611464968,0.134130730625945\n0.260934182590234,0.134678996560872\n0.261995753715499,0.135293336613667\n0.262845010615711,0.135749071648415\n0.263906581740977,0.136231127789666\n0.264968152866242,0.136872060300577\n0.265817409766454,0.137387221338666\n0.26687898089172,0.137865885584748\n0.267940552016985,0.138350248214713\n0.268789808917197,0.138767994023582\n0.269851380042463,0.139453970902322\n0.270912951167728,0.139992468179165\n0.271974522292994,0.140425002650965\n0.272823779193206,0.140775046232284\n0.273885350318471,0.141503218286912\n0.274946921443737,0.141959631700693\n0.275796178343949,0.142366659120832\n0.276857749469214,0.142973401328451\n0.27791932059448,0.143521938614991\n0.278980891719745,0.143929101710936\n0.279830148619958,0.144341963190763\n0.280891719745223,0.144850747465936\n0.281953290870488,0.145438359384809\n0.282802547770701,0.14578338296128\n0.283864118895966,0.146289860747739\n0.284925690021231,0.146806242868088\n0.285987261146497,0.14715533671876\n0.286836518046709,0.147574710637309\n0.287898089171975,0.148162051204569\n0.28895966029724,0.148668936018448\n0.289808917197452,0.149102148869282\n0.290870488322718,0.149652314265502\n0.291932059447983,0.150128943374484\n0.292993630573248,0.150678294715864\n0.293842887473461,0.151212586042699\n0.294904458598726,0.151764650900213\n0.295966029723991,0.152340323348096\n0.296815286624204,0.152844358970034\n0.297876857749469,0.153455849830888\n0.298938428874735,0.154029215790057\n0.299787685774947,0.154569069824967\n0.300849256900212,0.155104175206642\n0.301910828025478,0.155671707106122\n0.302972399150743,0.156241816845929\n0.303821656050955,0.156746666522707\n0.304883227176221,0.157523139164524\n0.305944798301486,0.158110751083397\n0.306794055201699,0.158439493563062\n0.307855626326964,0.159038095222279\n0.308917197452229,0.159531955158714\n0.309978768577495,0.160017674546746\n0.310828025477707,0.160463505247604\n0.311889596602972,0.161015977132538\n0.312951167728238,0.161571569561027\n0.31380042462845,0.1619610948021\n0.314861995753715,0.162547349962906\n0.315923566878981,0.163033476378358\n0.316985138004246,0.163687298090907\n0.317834394904459,0.164209649946751\n0.318895966029724,0.164752895876829\n0.319957537154989,0.16527538340848\n0.320806794055202,0.165743329265832\n0.321868365180467,0.166338946057301\n0.322929936305732,0.166924522839074\n0.323991507430998,0.167405764925484\n0.32484076433121,0.167905730273221\n0.325902335456476,0.168475297309801\n0.326963906581741,0.169025869733441\n0.327813163481953,0.169663817376605\n0.328874734607219,0.170582885291277\n0.329936305732484,0.171597740325489\n0.330997876857749,0.172468507652972\n0.331847133757962,0.173098314747733\n0.332908704883227,0.173924309059\n0.333970276008493,0.17475925797351\n0.334819532908705,0.175676697778502\n0.33588110403397,0.176852599995282\n0.336942675159236,0.177821867958438\n0.337791932059448,0.178440956664469\n0.338853503184713,0.179310231558078\n0.339915074309979,0.180444616978003\n0.340976645435244,0.181253651813432\n0.341825902335456,0.182035280135904\n0.342887473460722,0.183912219245969\n0.343949044585987,0.184830337429994\n0.3447983014862,0.18541577853596\n0.345859872611465,0.186345157811943\n0.34692144373673,0.187444131846316\n0.347983014861996,0.189126279532333\n0.348832271762208,0.190109386427774\n0.349893842887473,0.191477676938472\n0.350955414012739,0.192876223154068\n0.351804670912951,0.194030281565967\n0.352866242038217,0.195624879322262\n0.353927813163482,0.196793319369673\n0.354989384288747,0.198490759387456\n0.35583864118896,0.199771946030245\n0.356900212314225,0.200899954687255\n0.35796178343949,0.202165674288079\n0.358811040339703,0.203143489827058\n0.359872611464968,0.204110722653114\n0.360934182590234,0.205402220657213\n0.361995753715499,0.207265320834993\n0.362845010615711,0.208173263333515\n0.363906581740977,0.209425958056894\n0.364968152866242,0.210494540710564\n0.365817409766454,0.211692693759645\n0.36687898089172,0.212950544163679\n0.367940552016985,0.214156430733742\n0.368789808917197,0.215101005700077\n0.369851380042463,0.216198215948963\n0.370912951167728,0.217429880922302\n0.371974522292994,0.218864788254096\n0.372823779193206,0.220078815372562\n0.373885350318471,0.221335580370143\n0.374946921443737,0.222595058883857\n0.375796178343949,0.223243724915751\n0.376857749469214,0.224091156004479\n0.37791932059448,0.225541937405659\n0.378980891719745,0.226940619297061\n0.379830148619958,0.228052753884686\n0.380891719745223,0.229295544274175\n0.381953290870488,0.230228586796939\n0.382802547770701,0.231248326160193\n0.383864118895966,0.232407268901133\n0.384925690021231,0.233757514529539\n0.385987261146497,0.234856217212299\n0.386836518046709,0.235745029422075\n0.387898089171975,0.236876701325866\n0.38895966029724,0.237739463780753\n0.389808917197452,0.238593271632396\n0.390870488322718,0.239535133082596\n0.391932059447983,0.240531264855482\n0.392993630573248,0.241489814429908\n0.393842887473461,0.242190172944159\n0.394904458598726,0.243154149550853\n0.395966029723992,0.244123960217236\n0.396815286624204,0.244877639323526\n0.397876857749469,0.245858846757673\n0.398938428874735,0.246815496870805\n0.399787685774947,0.247939299577807\n0.400849256900212,0.248804368521407\n0.401910828025478,0.24998841128659\n0.402972399150743,0.251053466369285\n0.403821656050955,0.25198935808399\n0.404883227176221,0.252855648109851\n0.405944798301486,0.254096132010626\n0.406794055201699,0.255146669782004\n0.407855626326964,0.256093279885439\n0.408917197452229,0.257307442679712\n0.409978768577495,0.258187435962051\n0.410828025477707,0.259096192515413\n0.411889596602972,0.260143202715816\n0.412951167728238,0.261391013110153\n0.41380042462845,0.262122984087369\n0.414861995753716,0.262965259495442\n0.415923566878981,0.263778093253458\n0.416985138004246,0.264781144492486\n0.417834394904459,0.265519492232617\n0.418895966029724,0.266376420627815\n0.419957537154989,0.267466304383138\n0.420806794055202,0.26814726125703\n0.421868365180467,0.269188708749357\n0.422929936305733,0.270102620983375\n0.423991507430998,0.271143797124089\n0.42484076433121,0.272034237443545\n0.425902335456476,0.273006490274449\n0.426963906581741,0.273912668987483\n0.427813163481953,0.274723603284206\n0.428874734607219,0.275700197740924\n0.429936305732484,0.276439630887509\n0.430997876857749,0.277350422577972\n0.431847133757962,0.278056208124492\n0.432908704883227,0.278829288871141\n0.433970276008493,0.279675363201802\n0.434819532908705,0.280467574237198\n0.43588110403397,0.281279051237147\n0.436942675159236,0.282127567732329\n0.437791932059448,0.28286591547246\n0.438853503184713,0.28380777692266\n0.439915074309979,0.284734442682509\n0.440976645435244,0.285640485719737\n0.441825902335456,0.286382089679229\n0.442887473460722,0.287245937540569\n0.443949044585987,0.287994732317817\n0.4447983014862,0.28895504567773\n0.445859872611465,0.289847928161707\n0.44692144373673,0.29069970087625\n0.447983014861996,0.291469389727732\n0.448832271762208,0.29236593545849\n0.449893842887473,0.293311731507085\n0.450955414012739,0.294110454981203\n0.451804670912951,0.294875123827836\n0.452866242038217,0.295783202002165\n0.453927813163482,0.296721671557197\n0.454989384288747,0.29752270152003\n0.45583864118896,0.298291711992478\n0.456900212314225,0.299236286958812\n0.45796178343949,0.300121435921806\n0.458811040339703,0.300723565151998\n0.459872611464968,0.301556478929408\n0.460934182590234,0.302699004897736\n0.461995753715499,0.303610203615619\n0.462845010615711,0.304290075083057\n0.463906581740977,0.305296382541446\n0.464968152866242,0.30624964075941\n0.465817409766454,0.307041580443192\n0.46687898089172,0.307867710430266\n0.467940552016985,0.308790305915913\n0.468789808917197,0.309495684435013\n0.469851380042463,0.310383953941562\n0.470912951167728,0.311752108776454\n0.471974522292994,0.312835344417249\n0.472823779193206,0.313666358733364\n0.473885350318471,0.314966132961673\n0.474946921443737,0.316324519138481\n0.475796178343949,0.317405583966369\n0.476857749469214,0.318885942693412\n0.47791932059448,0.320117064963524\n0.478980891719745,0.32184041906039\n0.479830148619958,0.322977789348063\n0.480891719745223,0.324751614845026\n0.481953290870488,0.325416833325339\n0.482802547770701,0.326025067966833\n0.483864118895966,0.326842107674857\n0.484925690021231,0.327698222015215\n0.485987261146497,0.32854619580717\n0.486836518046709,0.329151445580915\n0.487898089171975,0.330094528113376\n0.48895966029724,0.330976963560236\n0.489808917197452,0.331738918890735\n0.490870488322718,0.332527602355156\n0.491932059447983,0.333493885450565\n0.492993630573248,0.334542252409035\n0.493842887473461,0.335202179532886\n0.494904458598726,0.33652813919189\n0.495966029723992,0.337494150935685\n0.496815286624204,0.338246066256487\n0.497876857749469,0.339237720727751\n0.498938428874735,0.340709938906391\n0.499787685774947,0.341662383069515\n0.500849256900212,0.342765698729704\n0.501910828025478,0.343816914880114\n0.502972399150743,0.344925793248378\n0.503821656050955,0.345692090204692\n0.504883227176221,0.346865007553724\n0.505944798301486,0.347819215502335\n0.506794055201699,0.348714268799219\n0.507855626326964,0.349867784507891\n0.508917197452229,0.35092198552605\n0.509978768577495,0.352046602287892\n0.510828025477707,0.352941248557356\n0.511889596602972,0.353958952783509\n0.512951167728238,0.354681155102641\n0.51380042462845,0.355274465405396\n0.514861995753715,0.356137499211896\n0.515923566878981,0.35705154712172\n0.516985138004246,0.357939816628269\n0.517834394904459,0.358749529842731\n0.518895966029724,0.359634000426691\n0.519957537154989,0.360473833670244\n0.520806794055202,0.361211774382954\n0.521868365180467,0.362113747145981\n0.522929936305732,0.363380823504872\n0.523991507430998,0.364618865241126\n0.52484076433121,0.36505167106454\n0.525902335456476,0.365938855164635\n0.526963906581741,0.367031181084479\n0.527813163481953,0.367883360826443\n0.528874734607219,0.368903914244536\n0.529936305732484,0.369964627701417\n0.530997876857749,0.370913544293566\n0.531847133757962,0.37187507873574\n0.532908704883227,0.372754529314852\n0.533970276008493,0.373937079646161\n0.534819532908705,0.374484667202053\n0.53588110403397,0.375472794102343\n0.536942675159236,0.376521975115653\n0.537791932059448,0.377615522117758\n0.538853503184713,0.379240104227337\n0.539915074309979,0.380389956689228\n0.540976645435244,0.381590144875409\n0.541825902335456,0.38252535821108\n0.542887473460722,0.383801389173214\n0.543949044585987,0.384820857184854\n0.5447983014862,0.385546044371734\n0.545859872611465,0.386675545462618\n0.54692144373673,0.38726980549602\n0.547983014861996,0.388201626936523\n0.548832271762208,0.388672557661623\n0.549893842887473,0.389754979247578\n0.550955414012739,0.390610686560516\n0.551804670912951,0.391230996348806\n0.552866242038217,0.39189947104848\n0.553927813163482,0.392709184262942\n0.554989384288747,0.393480229872491\n0.55583864118896,0.394001903349302\n0.556900212314225,0.394693035908697\n0.55796178343949,0.395474935582782\n0.558811040339703,0.396385048894212\n0.559872611464968,0.39720317400869\n0.560934182590234,0.398042193197402\n0.561995753715499,0.399797877170657\n0.562845010615711,0.400297028463553\n0.563906581740977,0.401833596008967\n0.564968152866242,0.402714403346147\n0.565817409766454,0.403490740312157\n0.56687898089172,0.404366663320295\n0.567940552016985,0.405462245459501\n0.568789808917197,0.406861373412715\n0.569851380042463,0.40805233564404\n0.570912951167728,0.409430530488628\n0.571974522292994,0.410954441149626\n0.572823779193206,0.41203482759848\n0.573885350318471,0.413248583365333\n0.574946921443737,0.414135224762201\n0.575796178343949,0.415154557098034\n0.576857749469214,0.416036178490054\n0.57791932059448,0.417051983254913\n0.578980891719745,0.418522980351293\n0.579830148619958,0.419333507620595\n0.580891719745223,0.420138200830208\n0.581953290870488,0.421157940193462\n0.582802547770701,0.422032506443532\n0.583864118895966,0.423189142695759\n0.584925690021231,0.424109703044305\n0.585987261146497,0.424963375220142\n0.586836518046709,0.425686255918308\n0.587898089171975,0.426811008355957\n0.58895966029724,0.427670785943096\n0.589808917197452,0.428552000307695\n0.590870488322718,0.429424260069052\n0.591932059447983,0.43016518564951\n0.592993630573248,0.431191708803099\n0.593842887473461,0.432056642070893\n0.594904458598726,0.432755236799658\n0.595966029723991,0.433711208533756\n0.596815286624204,0.434667722971081\n0.597876857749469,0.435610398476121\n0.598938428874735,0.436581158873152\n0.599787685774947,0.437319777964896\n0.600849256900212,0.438166666350397\n0.601910828025478,0.439269574983165\n0.602972399150743,0.440513450779108\n0.603821656050955,0.441277712598321\n0.604883227176221,0.442387812048846\n0.605944798301486,0.443522333144578\n0.606794055201698,0.444326212299351\n0.607855626326964,0.445293445125407\n0.608917197452229,0.446056350186553\n0.609978768577495,0.446935665089858\n0.610828025477707,0.447866943827135\n0.611889596602972,0.448811383117663\n0.612951167728238,0.449757450517871\n0.61380042462845,0.450660508687351\n0.614861995753715,0.451685539407066\n0.615923566878981,0.452612747870142\n0.616985138004246,0.453809544161155\n0.617834394904459,0.454597413570736\n0.618895966029724,0.455311611017272\n0.619957537154989,0.456141132899514\n0.620806794055202,0.456797803804004\n0.621868365180467,0.457499247724709\n0.622929936305732,0.458413566986147\n0.623991507430998,0.459361126820229\n0.62484076433121,0.4602761244607\n0.625902335456476,0.461115550676832\n0.626963906581741,0.461863395723433\n0.627813163481953,0.462655335407216\n0.628874734607219,0.463504801633044\n0.629936305732484,0.464281545626475\n0.630997876857749,0.465269401175151\n0.631847133757962,0.466127686328416\n0.632908704883227,0.467018669351099\n0.633970276008493,0.467725811655686\n0.634819532908705,0.468329976022978\n0.63588110403397,0.469199929295621\n0.636942675159236,0.470035827940778\n0.637791932059448,0.470689242625907\n0.638853503184713,0.471441972001549\n0.639915074309979,0.472210304094964\n0.640976645435244,0.472868874460748\n0.641825902335456,0.473466526389318\n0.642887473460722,0.474298626111887\n0.643949044585987,0.475663524727418\n0.6447983014862,0.476767654442447\n0.645859872611465,0.478144763880581\n0.64692144373673,0.479493517075113\n0.647983014861996,0.480884329769726\n0.648832271762208,0.482080854709126\n0.649893842887473,0.483220395809707\n0.650955414012739,0.484282194673041\n0.651804670912951,0.485274256171725\n0.652866242038217,0.486500901140215\n0.653927813163482,0.487481972898556\n0.654989384288747,0.488483260352096\n0.65583864118896,0.489214145922858\n0.656900212314225,0.490544040180257\n0.65796178343949,0.491396762625446\n0.658811040339703,0.492359789501494\n0.659872611464968,0.493755215173535\n0.660934182590234,0.495105460801941\n0.661995753715499,0.496367788507596\n0.662845010615711,0.49721128499793\n0.663906581740977,0.49826209412092\n0.664968152866242,0.499285496730955\n0.665817409766454,0.499951122238688\n0.66687898089172,0.500971811332588\n0.667940552016985,0.502098056204111\n0.668789808917197,0.503125529088347\n0.669851380042463,0.504338606476166\n0.670912951167728,0.505333517166791\n0.671974522292994,0.506368316544589\n0.672823779193206,0.507187120038101\n0.673885350318471,0.508166699362567\n0.674946921443737,0.5090869883595\n0.675796178343949,0.509798336614095\n0.676857749469214,0.510647124460891\n0.67791932059448,0.511855588871281\n0.678980891719745,0.513073550588142\n0.679830148619957,0.513850158905766\n0.680891719745223,0.514743719768777\n0.681953290870488,0.515647184965677\n0.682802547770701,0.5163611110606\n0.683864118895966,0.517312334141463\n0.684925690021231,0.518301003744979\n0.685987261146497,0.519148299157901\n0.686836518046709,0.51995027885138\n0.687898089171974,0.521013298796975\n0.68895966029724,0.521922055350337\n0.689808917197452,0.522801234577836\n0.690870488322718,0.523622887263289\n0.691932059447983,0.524555929786052\n0.692993630573248,0.52543022468451\n0.693842887473461,0.526288916865195\n0.694904458598726,0.527112876039362\n0.695966029723991,0.528209136557601\n0.696815286624204,0.529103375799645\n0.697876857749469,0.530070608625701\n0.698938428874735,0.530976244635508\n0.7,0.532073726236008\n0.700849256900212,0.532935674636055\n0.701910828025478,0.53385297876524\n0.702972399150743,0.534890898686593\n0.703821656050955,0.535663301054209\n0.704883227176221,0.536447914244429\n0.705944798301486,0.537184633874879\n0.706794055201698,0.537850123706805\n0.707855626326964,0.538985594533185\n0.708917197452229,0.540063403141711\n0.709978768577495,0.541214748037476\n0.710828025477707,0.541952688750187\n0.711889596602972,0.542967408108592\n0.712951167728238,0.543798422424707\n0.71380042462845,0.5447178973668\n0.714861995753715,0.545774269197866\n0.715923566878981,0.546586153225235\n0.716985138004246,0.54726778847816\n0.717834394904459,0.547980086463402\n0.718895966029724,0.54885560244412\n0.719957537154989,0.549840066097628\n0.720806794055202,0.550441516948786\n0.721868365180467,0.551444161160393\n0.722929936305732,0.55261151580135\n0.723991507430998,0.553656083837232\n0.72484076433121,0.554381949403145\n0.725902335456476,0.555699768513747\n0.726963906581741,0.556898057238634\n0.727813163481953,0.557901786856695\n0.728874734607219,0.558958294363568\n0.729936305732484,0.560151291731993\n0.730997876857749,0.561401951318272\n0.731847133757962,0.562289678121594\n0.732908704883227,0.563426777057654\n0.733970276008493,0.564628322001902\n0.734819532908705,0.565420804388911\n0.73588110403397,0.566531717894276\n0.736942675159236,0.567692695772317\n0.737791932059448,0.568730887045283\n0.738853503184713,0.570221285782023\n0.739915074309979,0.57128050680503\n0.740976645435244,0.572666435170601\n0.741825902335456,0.573480490010878\n0.742887473460722,0.574845388626409\n0.743949044585987,0.575889006931643\n0.7447983014862,0.576733181801011\n0.745859872611465,0.577940425129141\n0.74692144373673,0.578880387118047\n0.747983014861996,0.579656859759864\n0.748832271762208,0.580818380341133\n0.749893842887473,0.582424374865192\n0.750955414012739,0.583813423774317\n0.751804670912951,0.584591931553235\n0.752866242038217,0.586006487514023\n0.753927813163482,0.587212916787313\n0.754989384288747,0.588471581246188\n0.75583864118896,0.589461471931964\n0.756900212314225,0.590585817342193\n0.75796178343949,0.592309985493899\n0.758811040339703,0.593548027230153\n0.759872611464968,0.594795294921264\n0.760934182590234,0.595946911168642\n0.761995753715499,0.597118200407994\n0.762845010615711,0.598041474272674\n0.763906581740977,0.599385750165585\n0.764968152866242,0.600658524908357\n0.765817409766454,0.601183454604529\n0.76687898089172,0.602512941834508\n0.767940552016985,0.603567685555893\n0.768789808917198,0.604449171272106\n0.769851380042463,0.605351415386746\n0.770912951167728,0.60648132350505\n0.771974522292994,0.607437837942375\n0.772823779193206,0.608264646308483\n0.773885350318471,0.609413684715534\n0.774946921443737,0.610518899837016\n0.775796178343949,0.611348286043451\n0.776857749469214,0.612330036180825\n0.77791932059448,0.613586665502599\n0.778980891719745,0.614505869093078\n0.779830148619958,0.615375686689914\n0.780891719745223,0.616487278574312\n0.781953290870488,0.617604975870012\n0.782802547770701,0.618606941702586\n0.783864118895966,0.619660464341711\n0.784925690021231,0.620814658429416\n0.785987261146497,0.621799529110344\n0.786836518046709,0.622682371584624\n0.787898089171975,0.623794234820636\n0.78895966029724,0.624960775406752\n0.789808917197452,0.625702379366245\n0.790870488322718,0.626691727348794\n0.791932059447983,0.627687723445873\n0.792993630573248,0.628722115796251\n0.793842887473461,0.630286729199263\n0.794904458598726,0.631400356220762\n0.795966029723992,0.632987627483494\n0.796815286624204,0.633810094223787\n0.797876857749469,0.635019779716439\n0.798938428874735,0.636057699637792\n0.799787685774947,0.636926703179787\n0.800849256900212,0.638028526406102\n0.801910828025478,0.639647410131799\n0.802972399150743,0.640689128975739\n0.803821656050955,0.641676984524415\n0.804883227176221,0.642772159636201\n0.805944798301486,0.644150083129176\n0.806794055201699,0.645012302880836\n0.807855626326964,0.646826152740779\n0.808917197452229,0.648212623809577\n0.809978768577495,0.649676430088201\n0.810828025477707,0.650836322559789\n0.811889596602972,0.652175714123658\n0.812951167728238,0.65350628676009\n0.81380042462845,0.654538508297561\n0.814861995753716,0.655724043496617\n0.815923566878981,0.657000752837785\n0.816985138004246,0.658680012297469\n0.817834394904459,0.660218711621399\n0.818895966029724,0.661042535119759\n0.819957537154989,0.66257580741142\n0.820806794055202,0.663413876869485\n0.821868365180467,0.664390335650397\n0.822929936305733,0.665416858803986\n0.823991507430998,0.666493989033479\n0.82484076433121,0.667222975142946\n0.825902335456476,0.668136344673737\n0.826963906581741,0.669478314077933\n0.827813163481953,0.670048152466127\n0.828874734607219,0.670803188330483\n0.829936305732484,0.671615750736886\n0.83099787685775,0.672673479326019\n0.831847133757962,0.673502729856648\n0.832908704883227,0.674517313539246\n0.833970276008493,0.675757797440021\n0.834819532908705,0.676553536046392\n0.83588110403397,0.677656173327547\n0.836942675159236,0.6787729208926\n0.837791932059448,0.679476535626212\n0.838853503184713,0.680663427583336\n0.839915074309979,0.681656167461053\n0.840976645435244,0.682512010449797\n0.841825902335457,0.683406385367648\n0.842887473460722,0.68435435222915\n0.843949044585987,0.685403261890847\n0.8447983014862,0.686292616803849\n0.845859872611465,0.687227965815327\n0.84692144373673,0.688162636447772\n0.847983014861996,0.689043715136564\n0.848832271762208,0.689912854354367\n0.849893842887473,0.690874931499767\n0.850955414012739,0.691728467999797\n0.851804670912951,0.692434660573737\n0.852866242038217,0.693232027289788\n0.853927813163482,0.694578066968186\n0.854989384288747,0.695685317226769\n0.85583864118896,0.696292059434389\n0.856900212314225,0.697798196564707\n0.85796178343949,0.69888604518293\n0.858811040339703,0.699652342139244\n0.859872611464968,0.700790933509178\n0.860934182590234,0.701858159404781\n0.861995753715499,0.702963645877877\n0.862845010615711,0.703830342931158\n0.863906581740977,0.705212336698334\n0.864968152866242,0.706292180443961\n0.865817409766454,0.707416932881611\n0.86687898089172,0.708860116437614\n0.867940552016985,0.709570650637369\n0.868789808917197,0.710370459517941\n0.869851380042463,0.711428188107074\n0.870912951167728,0.712354718191116\n0.871974522292994,0.713590996141883\n0.872823779193206,0.714447924537081\n0.873885350318471,0.715631017571616\n0.874946921443737,0.716855220375586\n0.875796178343949,0.717684063878794\n0.876857749469214,0.718612222072516\n0.87791932059448,0.719854062731359\n0.878980891719745,0.720943403783456\n0.879830148619958,0.721893677133672\n0.880891719745223,0.723018293895514\n0.881953290870488,0.724268003751146\n0.882802547770701,0.724879358936194\n0.883864118895966,0.72581023064605\n0.884925690021231,0.727082055658176\n0.885987261146497,0.728254973007208\n0.886836518046709,0.729172548488006\n0.887898089171975,0.730270844143346\n0.88895966029724,0.731591648121696\n0.889808917197452,0.732227153600338\n0.890870488322718,0.733327484392779\n0.891932059447983,0.734154021407273\n0.892993630573248,0.735201031607676\n0.893842887473461,0.737082312343555\n0.894904458598726,0.739241185779969\n0.895966029723992,0.742697294638756\n0.896815286624204,0.745004229414685\n0.897876857749469,0.747868384728784\n0.898938428874735,0.750530208380682\n0.899787685774947,0.752967934634282\n0.900849256900212,0.755558257136043\n0.901910828025478,0.759268293497972\n0.902972399150743,0.76260436830221\n0.903821656050955,0.764692418967519\n0.904883227176221,0.768196285710269\n0.905944798301486,0.771424691531\n0.906794055201699,0.773135699129455\n0.907855626326964,0.776024372730177\n0.908917197452229,0.779231691193844\n0.909978768577495,0.783113318416873\n0.910828025477707,0.785387787640606\n0.911889596602972,0.788861302037044\n0.912951167728238,0.792296477787205\n0.91380042462845,0.794625488685237\n0.914861995753716,0.797698274355668\n0.915923566878981,0.800041666889211\n0.916985138004246,0.802301483125819\n0.917834394904459,0.804191040085908\n0.918895966029724,0.80716518944486\n0.919957537154989,0.809218914131071\n0.920806794055202,0.811803073614506\n0.921868365180467,0.813936168647645\n0.922929936305733,0.816026932829089\n0.923991507430998,0.81898127352026\n0.92484076433121,0.820809097988294\n0.925902335456476,0.82299347847637\n0.926963906581741,0.825770355212361\n0.927813163481953,0.828181449973454\n0.928874734607219,0.831809634789544\n0.929936305732484,0.834581937582498\n0.930997876857749,0.83716601899715\n0.931847133757962,0.839115408987999\n0.932908704883227,0.841153937983859\n0.933970276008493,0.843203728071676\n0.934819532908705,0.844505808788699\n0.93588110403397,0.846237439109774\n0.936942675159236,0.848197547489354\n0.937791932059448,0.850481010350721\n0.938853503184713,0.853061467552984\n0.939915074309979,0.855370805459042\n0.940976645435244,0.858312703010387\n0.941825902335456,0.860515264056563\n0.942887473460722,0.863378469640014\n0.943949044585987,0.865676003750889\n0.9447983014862,0.867835555566336\n0.945859872611465,0.87013186859495\n0.94692144373673,0.872675654295008\n0.947983014861996,0.874645802684285\n0.948832271762208,0.876292499950358\n0.949893842887473,0.878433735531899\n0.950955414012739,0.880716073952422\n0.951804670912951,0.882734251577274\n0.952866242038217,0.88458351282277\n0.953927813163482,0.886493963857092\n0.954989384288747,0.888504272285156\n0.95583864118896,0.890180411201286\n0.956900212314225,0.893074686544475\n0.95796178343949,0.895774499422252\n0.958811040339703,0.897481029719086\n0.959872611464968,0.899638682073239\n0.960934182590234,0.902165236945845\n0.961995753715499,0.903891711586265\n0.962845010615711,0.905701898199427\n0.963906581740977,0.907486306409314\n0.964968152866242,0.910219573604326\n0.965817409766454,0.911803724323505\n0.96687898089172,0.913931935027602\n0.967940552016985,0.916074663043017\n0.968789808917197,0.917905879406219\n0.969851380042463,0.920601214982375\n0.970912951167728,0.924057595192776\n0.971974522292994,0.927566520974765\n0.972823779193206,0.930377723689854\n0.973885350318471,0.933330571947151\n0.974946921443737,0.936109929882054\n0.975796178343949,0.939157790238442\n0.976857749469214,0.942701991730177\n0.97791932059448,0.945733570989759\n0.978980891719745,0.948923037853963\n0.979830148619958,0.950897663544861\n0.980891719745223,0.954397770399414\n0.981953290870488,0.957322572799113\n0.982802547770701,0.959434231054791\n0.983864118895966,0.961545075255629\n0.984925690021231,0.963594186964412\n0.985987261146497,0.966933207602006\n0.986836518046709,0.968950978199438\n0.987898089171975,0.971124097595557\n0.98895966029724,0.973858914831467\n0.989808917197452,0.975663810088167\n0.990870488322718,0.977411178802821\n0.991932059447983,0.979192873496573\n0.992993630573248,0.982341948004769\n0.993842887473461,0.983980097695019\n0.994904458598726,0.986731331703541\n0.995966029723992,0.989721122814656\n0.996815286624204,0.991808088073512\n0.997876857749469,0.993868325198446\n0.998938428874735,0.997017806734061\n0.999787685774947,0.999289601476052"
    },
    {
      "name": "scale/x",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n-0.0499893842887474\n1.04977707006369"
    },
    {
      "name": "scale/y",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n-0.0499787685774947\n1.04955414012739"
    }
  ],
  "scales": [
    {
      "name": "x",
      "domain": {
        "data": "scale/x",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "width"
    },
    {
      "name": "y",
      "domain": {
        "data": "scale/y",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "height"
    }
  ],
  "marks": [
    {
      "type": "line",
      "properties": {
        "update": {
          "x": {
            "scale": "x",
            "field": "data.ValidationWeight"
          },
          "y": {
            "scale": "y",
            "field": "data.ValidationLoss"
          },
          "stroke": {
            "value": "blue"
          }
        },
        "ggvis": {
          "data": {
            "value": ".0"
          }
        }
      },
      "from": {
        "data": ".0"
      }
    },
    {
      "type": "line",
      "properties": {
        "update": {
          "x": {
            "scale": "x",
            "field": "data.ValidationWeight"
          },
          "y": {
            "scale": "y",
            "field": "data.ValidationWeight"
          },
          "stroke": {
            "value": "black"
          }
        },
        "ggvis": {
          "data": {
            "value": "ModelComparisonData1"
          }
        }
      },
      "from": {
        "data": "ModelComparisonData1"
      }
    },
    {
      "type": "line",
      "properties": {
        "update": {
          "x": {
            "scale": "x",
            "field": "data.TrainWeight"
          },
          "y": {
            "scale": "y",
            "field": "data.TrainLoss"
          },
          "stroke": {
            "value": "red"
          }
        },
        "ggvis": {
          "data": {
            "value": "ModelComparisonData2"
          }
        }
      },
      "from": {
        "data": "ModelComparisonData2"
      }
    }
  ],
  "legends": [],
  "axes": [
    {
      "type": "x",
      "scale": "x",
      "orient": "bottom",
      "layer": "back",
      "grid": true,
      "title": "TrainWeight"
    },
    {
      "type": "y",
      "scale": "y",
      "orient": "left",
      "layer": "back",
      "grid": true,
      "title": "TrainLoss"
    }
  ],
  "padding": null,
  "ggvis_opts": {
    "keep_aspect": false,
    "resizable": true,
    "padding": {},
    "duration": 250,
    "renderer": "svg",
    "hover_duration": 0,
    "width": 576,
    "height": 384
  },
  "handlers": null
};
ggvis.getPlot("plot_id760492419").parseSpec(plot_id760492419_spec);
</script><!--/html_preserve-->

The above chart shows lorenz curve for Trainset (redline) and Testset (blueline). Further away the curves are from the diagonal line means better prediction power. From the chart, our testset performance was even better than trainset, therefore, no overfitting problem presents in the linear regression model.

###5. Gradient boosting

####Trainset Variable Transformation for gradient boosting

```r
#Train set
#Create missing flags
for (i in 1:ncol(gbm.trainset)){
  if(colnames(gbm.trainset)[i]!='HC01_VC128' & colnames(gbm.trainset)[i]!='Target'){
    gbm.trainset[,paste0("flag_",colnames(gbm.trainset)[i])]<-as.factor((ifelse(is.na(gbm.trainset[,i]),1,0)))
  }
}

#boxplot shows outliers for dependent variable
ggplot(data=gbm.trainset,aes(x=1,y=gbm.trainset$HC01_VC128))+geom_boxplot()+xlab('Boxplot')+ylab('')
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-39-1.png)<!-- -->

```r
#tranform and cap dependent variable
gbm.trainset$Target<-pmin(gbm.trainset$HC01_VC128,quantile(gbm.trainset$HC01_VC128,0.99))
gbm.trainset$Target<-gbm.trainset$HC01_VC128
gbm.trainset<-as.data.frame(gbm.trainset%>%select(-HC01_VC128))

#Remove independent variable outliers
for (i in 1:ncol(gbm.trainset)){
if ((substr(colnames(gbm.trainset)[i],1,5)=='Weat_'|substr(colnames(gbm.trainset)[i],1,5)=='HC01_')&(class(eval(parse(text=paste0('gbm.trainset$',colnames(gbm.trainset)[i]))))=='numeric'|class(eval(parse(text=paste0('gbm.trainset$',colnames(gbm.trainset)[i]))))=='integer')){
  gbm.trainset[,i]<-pmin(pmax(eval(parse(text=paste0('gbm.trainset$',colnames(gbm.trainset)[i]))),quantile(eval(parse(text=paste0('gbm.trainset$',colnames(gbm.trainset)[i]))),0.05,na.rm=TRUE)),quantile(eval(parse(text=paste0('gbm.trainset$',colnames(gbm.trainset)[i]))),0.95,na.rm=TRUE))
}
}

#save a capping value table for gbm.testset transformation later
gbm.trainset.num<-gbm.trainset[,sapply(gbm.trainset,class)=='numeric'|sapply(gbm.trainset,class)=='integer']
capping.gbm<-as.matrix(rbind(colnames(gbm.trainset.num),sapply(gbm.trainset.num,function(x) quantile(x,0.05,na.rm=TRUE)),sapply(gbm.trainset.num,function(x) quantile(x,0.95,na.rm=TRUE))))
colnames(capping.gbm)<-capping.gbm[1,]
capping.gbm<-as.data.frame(capping.gbm[2:3,])

#Convert character variables to factors
for (i in 1:ncol(gbm.trainset)){
  if(class(gbm.trainset[,i])=="character"){   
    gbm.trainset[,i]<-as.factor(gbm.trainset[,i])
  } else {
    gbm.trainset[,i]=gbm.trainset[,i]
  }
}

#select useful columns 
gbm.trainset<-as.data.frame(gbm.trainset%>%select(Target,starts_with('Weat_'),starts_with('flag_'),starts_with('HC01_'),STATE.x,ELEVATION))
```

####Testset Variable Transformation for gradient boosting

```r
#Create missing flags
for (i in 1:ncol(gbm.testset)){
  if(colnames(gbm.testset)[i]!='HC01_VC128' & colnames(gbm.testset)[i]!='Target'){
    gbm.testset[,paste0("flag_",colnames(gbm.testset)[i])]<-as.factor((ifelse(is.na(gbm.testset[,i]),1,0)))
  }
}

#Create Target variable
gbm.testset$Target<-gbm.testset$HC01_VC128
gbm.testset<-as.data.frame(gbm.testset%>%select(-HC01_VC128))

#capping variables using gbm.trainset capping values
for (i in 1:ncol(gbm.testset)){
  if ((substr(colnames(gbm.testset)[i],1,5)=='Weat_'|substr(colnames(gbm.testset)[i],1,5)=='HC01_')&(class(eval(parse(text=paste0('gbm.testset$',colnames(gbm.testset)[i]))))=='numeric'|class(eval(parse(text=paste0('gbm.testset$',colnames(gbm.testset)[i]))))=='integer')){
    gbm.testset[,paste0(colnames(gbm.testset)[i])]<-pmin(pmax(gbm.testset[,i],capping.gbm[1,colnames(gbm.testset)[i]]),capping.gbm[2,colnames(gbm.testset)[i]])
  }
}

#Convert character variables to factors
for (i in 1:ncol(gbm.testset)){
  if((class(gbm.testset[,i])=='character')){   
    gbm.testset[,i]<-as.factor(gbm.testset[,i])
  }
}

#select useful columns 
gbm.testset<-as.data.frame(gbm.testset%>%select(Target,starts_with('Weat_'),starts_with('flag_'),starts_with('HC01_'),STATE.x,ELEVATION))
```

####Run gradient boosting model \newline

As parameter tuning is time-consuming, R markdown file documented the codes as below, however, cross-validation parameter tuning was run separately from this file with the following codes.

```r
# ctrl <- trainControl(method = "repeatedcv",
#                      number = 5,
#                      # repeats = 5,
#                      summaryFunction=defaultSummary)
# 
# 
# set.seed(5627)
# 
# gbmGrid <-  expand.grid(interaction.depth = c(2,3),
#                         n.trees = (1:3)*500, 
#                         shrinkage = c(0.1),
#                         n.minobsinnode = 100) 
# 
# gbm_fit <- train(Target~.,
#                  data = trainset,
#                  method = "gbm",
#                  bag.fraction = 0.5,
#                  verbose = FALSE,
#                  metric = "RMSE",
#                  trControl = ctrl,
#                  tuneGrid =gbmGrid
# )
# 
# plot(gbm_fit)
# summary(gbm_fit)
# gbm_fit$bestTune

#run gbm with tuned parameters##############
set.seed(12345)
gbm_tuned <- gbm(Target~.,
                 distribution = "gaussian",
                 data = gbm.trainset,
                 n.trees = 1500,# convergence issue, just need enough to converge
                 shrinkage = 0.1,  # convergence issue, lower is better but takes longer.
                 interaction.depth = 3, # effects tree complexity
                 n.minobsinnode = 200, # tree argument 1082/bag.fraction/train.fraction
                 bag.fraction = 0.5, #optimization argument
                 train.fraction = 1.0,
                 cv.folds=5, #gives condition on how good a model is doing
                 keep.data = TRUE,
                 verbose = TRUE, #"CV",
                 class.stratify.cv=NULL
)
best.iter <- gbm.perf(gbm_tuned, method="cv")
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-41-1.png)<!-- -->

####Gains Chart

```r
#Predict on trainset and testset
gbm.trainset$gbm_pred<- predict.gbm(gbm_tuned, n.trees = best.iter,newdata = gbm.trainset)
gbm.testset$gbm_pred <- predict.gbm(gbm_tuned, n.trees = best.iter,newdata = gbm.testset)

#Gains Chart on testset
GAINS.CHART(gbm.testset$Target,gbm.testset$gbm_pred,n.rank=20)
```

![](Home_Pricing_Analysis_files/figure-html/unnamed-chunk-42-1.png)<!-- -->

```
## $gainschart
##    Rank Actual_Sum Actual_Mean Pred_Sum Pred_Mean Count
## 1     1    7132200    60442.37  6211418  52639.14   118
## 2     2    8247200    69891.53  8043978  68169.30   118
## 3     3    8929800    75676.27  8930668  75683.62   118
## 4     4    9549900    81623.08  9617977  82204.93   117
## 5     5   10175500    86233.05 10377220  87942.55   118
## 6     6   11151800    94506.78 10971049  92974.99   118
## 7     7   11485900    98170.09 11543818  98665.11   117
## 8     8   12691500   107555.08 12440965 105431.91   118
## 9     9   13120100   111187.29 13256961 112347.13   118
## 10   10   14265800   120896.61 14213748 120455.49   118
## 11   11   14716500   125782.05 15053535 128662.69   117
## 12   12   16583000   140533.90 16204647 137327.52   118
## 13   13   17664000   149694.92 17454559 147919.99   118
## 14   14   18936600   161851.28 18823928 160888.27   117
## 15   15   20793000   176211.86 20667931 175151.95   118
## 16   16   22572300   191290.68 22792007 193152.61   118
## 17   17   25909800   221451.28 25666247 219369.63   117
## 18   18   29003300   245790.68 29702521 251716.28   118
## 19   19   38269500   324317.80 37794034 320288.42   118
## 20   20   62115800   526405.08 57588786 488040.56   118
## 
## $ks
## [1] 23.29
## 
## $gini
## [1] 63.75
```

####Lorenz Curve, Train vs Test

```r
##Lorenz Curve
trainCheck<- OrderedCDF(Score = gbm.trainset$gbm_pred, Loss = (gbm.trainset$Target), Weight =rep(1,nrow(gbm.trainset)), NBins = 1000)
ValidationCheck<- OrderedCDF(Score = gbm.testset$gbm_pred, Loss = (gbm.testset$Target), Weight =rep(1,nrow(gbm.testset)), NBins = 1000)

ValidationWeight <- ValidationCheck$WeightPts
ValidationLoss <- ValidationCheck$LossPts
TrainWeight <- trainCheck$WeightPts
TrainLoss <- trainCheck$LossPts

ModelComparisonData <- data.frame(ValidationWeight, ValidationLoss, TrainWeight, TrainLoss)
```


```r
ModelComparisonData %>% ggvis(~ValidationWeight, ~ValidationLoss) %>% layer_paths(stroke := "blue") %>% 
  layer_paths(data = ModelComparisonData, x = ~ValidationWeight, y = ~ValidationWeight, stroke := "black") %>%
  layer_paths(data = ModelComparisonData, x = ~TrainWeight, y = ~TrainLoss, stroke := "red")
```

<!--html_preserve--><div id="plot_id524633314-container" class="ggvis-output-container">
<div id="plot_id524633314" class="ggvis-output"></div>
<div class="plot-gear-icon">
<nav class="ggvis-control">
<a class="ggvis-dropdown-toggle" title="Controls" onclick="return false;"></a>
<ul class="ggvis-dropdown">
<li>
Renderer: 
<a id="plot_id524633314_renderer_svg" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id524633314" data-renderer="svg">SVG</a>
 | 
<a id="plot_id524633314_renderer_canvas" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id524633314" data-renderer="canvas">Canvas</a>
</li>
<li>
<a id="plot_id524633314_download" class="ggvis-download" data-plot-id="plot_id524633314">Download</a>
</li>
</ul>
</nav>
</div>
</div>
<script type="text/javascript">
var plot_id524633314_spec = {
  "data": [
    {
      "name": ".0",
      "format": {
        "type": "csv",
        "parse": {
          "ValidationWeight": "number",
          "ValidationLoss": "number"
        }
      },
      "values": "\"ValidationWeight\",\"ValidationLoss\"\n0,0\n0.000849256900212314,0.000239744879303856\n0.00169851380042463,0.00055985117066487\n0.0029723991507431,0.00088102894751998\n0.00382165605095541,0.00115131116340556\n0.00467091295116773,0.00144221947505247\n0.0059447983014862,0.00189572571042837\n0.00679405520169851,0.00216922238279623\n0.00764331210191083,0.00239262710831513\n0.0089171974522293,0.0028817602363697\n0.00976645435244161,0.00313275571336156\n0.0106157112526539,0.00346116601730181\n0.0118895966029724,0.00394333448964476\n0.0127388535031847,0.00419941952273357\n0.013588110403397,0.00436014234684789\n0.0148619957537155,0.00482704215089998\n0.0157112526539278,0.00512518298963204\n0.0169851380042463,0.00558645749484013\n0.0178343949044586,0.00585540035385808\n0.0186836518046709,0.0061934540272452\n0.0199575371549894,0.0067211606330872\n0.0208067940552017,0.00701742637220459\n0.021656050955414,0.00735548004559171\n0.0229299363057325,0.00787568625297505\n0.0237791932059448,0.00821936522520616\n0.0246284501061571,0.00848723659873002\n0.0259023354564756,0.0089865488389785\n0.0267515923566879,0.00930290493111018\n0.0276008492569002,0.00976498305043884\n0.0288747346072187,0.0102029527461504\n0.029723991507431,0.0105881517812777\n0.0309978768577495,0.011125233885193\n0.0318471337579618,0.0114638233013272\n0.0326963906581741,0.0118474151082133\n0.0339702760084926,0.012370835772079\n0.0348195329087049,0.0128026444261994\n0.0356687898089172,0.0132057908433528\n0.0369426751592357,0.0137517127025945\n0.037791932059448,0.0140177089765037\n0.0386411889596603,0.0144010329120163\n0.0399150743099788,0.0149608840826812\n0.0407643312101911,0.0152633108633896\n0.0416135881104034,0.0156257408317674\n0.0428874734607219,0.0160385306183677\n0.0437367303609342,0.0163728340925255\n0.0445859872611465,0.0166816897861985\n0.045859872611465,0.0172305582305489\n0.0467091295116773,0.0176125428091939\n0.0479830148619958,0.0181590004111826\n0.0488322717622081,0.0185377705333453\n0.0496815286624204,0.0188795744059617\n0.0509554140127389,0.019499964507043\n0.0518046709129512,0.0198736450731088\n0.0526539278131635,0.0201897332938669\n0.053927813163482,0.020673508994451\n0.0547770700636943,0.0210123662819587\n0.0556263269639066,0.0213217577183788\n0.0569002123142251,0.021896609685961\n0.0577494692144374,0.0222852910489441\n0.0585987261146497,0.0225973611990994\n0.0598726114649682,0.023087030069901\n0.0607218683651805,0.0234376736978438\n0.0619957537154989,0.0240727967244688\n0.0628450106157113,0.0245091591919392\n0.0636942675159236,0.0248946260984401\n0.064968152866242,0.0254753712362398\n0.0658174097664544,0.0258332473912677\n0.0666666666666667,0.0262615737175323\n0.0679405520169851,0.0268187461744619\n0.0687898089171975,0.0271656396031753\n0.0696390658174098,0.0275406595261088\n0.0709129511677282,0.0280308641396574\n0.0717622080679405,0.0283906153943\n0.0726114649681529,0.0287442056073515\n0.0738853503184713,0.0293624527374445\n0.0747346072186836,0.0297843501507446\n0.075583864118896,0.0301253504092405\n0.0768577494692144,0.0307521694232863\n0.0777070063694267,0.0311258499893521\n0.0789808917197452,0.0316886477451257\n0.0798301486199575,0.0320666142531679\n0.0806794055201698,0.0325086020194823\n0.0819532908704883,0.0331667619842304\n0.0828025477707006,0.0336031244517008\n0.0836518046709129,0.033992877300178\n0.0849256900212314,0.0345717473383631\n0.0857749469214437,0.0350841852759142\n0.086624203821656,0.0354334895469893\n0.0878980891719745,0.0358990499941738\n0.0887473460721868,0.0362858562575423\n0.0895966029723991,0.0366740018777783\n0.0908704883227176,0.0371992976412586\n0.0917197452229299,0.0375922649462181\n0.0929936305732484,0.0380709510907053\n0.0938428874734607,0.0385057063299345\n0.094692144373673,0.0388630467422153\n0.0959660297239915,0.0394395059380387\n0.0968152866242038,0.0397957748648254\n0.0976645435244161,0.0400984695169074\n0.0989384288747346,0.0406762680695983\n0.0997876857749469,0.0410566454200022\n0.100636942675159,0.0414206826166212\n0.101910828025478,0.0420161606799647\n0.10276008492569,0.0424434155207353\n0.103609341825902,0.0427672720113256\n0.104883227176221,0.0433681075021396\n0.105732484076433,0.0437583960933639\n0.106581740976645,0.0441931513325931\n0.107855626326964,0.0448084518775774\n0.108704883227176,0.0452016870539104\n0.109978768577495,0.0458713654877201\n0.110828025477707,0.0463152283536491\n0.111677282377919,0.0467548052776018\n0.112951167728238,0.0473561765111629\n0.11380042462845,0.0478209333442268\n0.114649681528662,0.0481964890099072\n0.115923566878981,0.0487823237038039\n0.116772823779193,0.0491953813617777\n0.117622080679406,0.0496360297712245\n0.118895966029724,0.0502465086314853\n0.119745222929936,0.0505417028851086\n0.120594479830149,0.0509277055343565\n0.121868365180467,0.0515004145309505\n0.122717622080679,0.0519062396618392\n0.123991507430998,0.0525413626884643\n0.12484076433121,0.0529423661346295\n0.125690021231423,0.0533862290005585\n0.126963906581741,0.0539857251345049\n0.127813163481953,0.0543269932643743\n0.128662420382166,0.0547373722086129\n0.129936305732484,0.0553711558783703\n0.130785562632696,0.0557292999047717\n0.131634819532909,0.0561817346546535\n0.132908704883227,0.0567951601000232\n0.133757961783439,0.0572216113266732\n0.134607218683652,0.0575612722283014\n0.13588110403397,0.0580873716059023\n0.136730360934183,0.0584698919272943\n0.137579617834395,0.0588183925842489\n0.138853503184713,0.0593731541988168\n0.139702760084926,0.0597015645027571\n0.140976645435244,0.0603224903465854\n0.141825902335456,0.0607157255229184\n0.142675159235669,0.0611161932263366\n0.143949044585987,0.0617357797132973\n0.1447983014862,0.0621330329602332\n0.145647558386412,0.0625624307719919\n0.14692144373673,0.0632578248576599\n0.147770700636943,0.0636510600339929\n0.148619957537155,0.0640909048293191\n0.149893842887473,0.0648958583067583\n0.150743099787686,0.0653378460730726\n0.151592356687898,0.0657096515395238\n0.152866242038217,0.0663501319936193\n0.153715498938429,0.0667366703856142\n0.154989384288747,0.0673886693087713\n0.15583864118896,0.0678523546563411\n0.156687898089172,0.0682498757746505\n0.15796178343949,0.0689184827229661\n0.158811040339703,0.069333147609181\n0.159660297239915,0.0697250434286464\n0.160934182590234,0.0703459692724747\n0.161783439490446,0.0708018863502124\n0.162632696390658,0.0712867335362905\n0.163906581740977,0.0718664071885962\n0.164755838641189,0.0722770540042083\n0.165605095541401,0.0727742232734686\n0.16687898089172,0.0733330029586393\n0.167728237791932,0.0737966883062091\n0.168577494692144,0.07426653469537\n0.169851380042463,0.0749463922413735\n0.170700636942675,0.0754124884313051\n0.171974522292994,0.076101721475382\n0.172823779193206,0.0765806754912426\n0.173673036093418,0.0769680174973581\n0.174946921443737,0.0775594774900988\n0.175796178343949,0.0779760174759284\n0.176645435244161,0.0783823783495641\n0.17791932059448,0.0790038399361395\n0.178768577494692,0.0794190405651014\n0.179617834394904,0.0798787078420684\n0.180891719745223,0.0805695481143864\n0.181740976645435,0.081041805345909\n0.182590233545648,0.0814811143984881\n0.183864118895966,0.0821258807945601\n0.184713375796178,0.0824944718045289\n0.185987261146497,0.0831895980188233\n0.186836518046709,0.0836543548518872\n0.187685774946921,0.0840966104895751\n0.18895966029724,0.0847837005626638\n0.189808917197452,0.0851788108386115\n0.190658174097665,0.0856566933689781\n0.191932059447983,0.0863694990939251\n0.192781316348195,0.0869049739695993\n0.193630573248408,0.0873081203867527\n0.194904458598726,0.0880061931861559\n0.195753715498938,0.0883911243499097\n0.196602972399151,0.0888540060833589\n0.197876857749469,0.0894998439649249\n0.198726114649682,0.089927366677069\n0.199575371549894,0.0904352508012702\n0.200849256900212,0.0912075239711395\n0.201698513800425,0.0916829956591444\n0.202972399150743,0.0922532938133767\n0.203821656050955,0.0926612619152535\n0.204670912951168,0.0931187862212323\n0.205944798301486,0.0937032815582614\n0.206794055201699,0.0941474122955639\n0.207643312101911,0.0945245751894855\n0.208917197452229,0.0952092544202125\n0.209766454352442,0.0956959767059054\n0.210615711252654,0.0961808238919835\n0.211889596602972,0.0968738071352898\n0.212738853503185,0.0972592740417906\n0.213588110403397,0.0976913505672846\n0.214861995753716,0.0983559394449973\n0.215711252653928,0.0988544480711252\n0.216985138004246,0.0996087738589684\n0.217834394904459,0.100033885728751\n0.218683651804671,0.100601773040621\n0.219957537154989,0.101340830160174\n0.220806794055202,0.101877644392716\n0.221656050955414,0.102364366678408\n0.222929936305732,0.10306993987627\n0.223779193205945,0.103521571012031\n0.224628450106157,0.103975077247407\n0.225902335456476,0.104665381776978\n0.226751592356688,0.105189070312217\n0.2276008492569,0.105618200252603\n0.228874734607219,0.10628011041658\n0.229723991507431,0.106718615855039\n0.230997876857749,0.107307665005418\n0.231847133757962,0.10775634955607\n0.232696390658174,0.10820101603612\n0.233970276008493,0.108961770736928\n0.234819532908705,0.109440724752788\n0.235668789808917,0.109909231785081\n0.236942675159236,0.110586678488723\n0.237791932059448,0.111115724451433\n0.23864118895966,0.111615036691681\n0.239915074309979,0.112327038802508\n0.240764331210191,0.112781080780631\n0.241613588110403,0.113246373356442\n0.242887473460722,0.114011681870599\n0.243736730360934,0.114513940695957\n0.244585987261146,0.114973072230177\n0.245859872611465,0.115628553481189\n0.246709129511677,0.116088756500903\n0.247983014861996,0.11678173974421\n0.248832271762208,0.117314535906149\n0.24968152866242,0.117787596751792\n0.250955414012739,0.118396200512438\n0.251804670912951,0.118930336031245\n0.252653927813163,0.119410093661226\n0.253927813163482,0.120068789368721\n0.254777070063694,0.120537296401014\n0.255626326963907,0.120990534765017\n0.256900212314225,0.121732806341051\n0.257749469214437,0.122258102104531\n0.25859872611465,0.122727144879572\n0.259872611464968,0.123587815602704\n0.26072186836518,0.124100789283002\n0.261995753715499,0.124968692533219\n0.262845010615711,0.125426484710572\n0.263694267515924,0.125895795356985\n0.264968152866242,0.126457521627265\n0.265817409766454,0.127062375188682\n0.266666666666667,0.127724017481286\n0.267940552016985,0.128508612734337\n0.268789808917197,0.129010067945574\n0.26963906581741,0.129483932405338\n0.270912951167728,0.130289689496897\n0.271762208067941,0.130755249944082\n0.272611464968153,0.131208756179458\n0.273885350318471,0.131949956269998\n0.274734607218684,0.132445518311017\n0.275583864118896,0.133033228104529\n0.276857749469214,0.133846217723174\n0.277707006369427,0.134336958079469\n0.278980891719745,0.135137089872185\n0.279830148619958,0.135685958316536\n0.28067940552017,0.136183127585796\n0.281953290870488,0.136952186299183\n0.282802547770701,0.137473999734807\n0.283651804670913,0.137988580643347\n0.284925690021231,0.138716655036585\n0.285774946921444,0.139203645193651\n0.286624203821656,0.139768318049039\n0.287898089171975,0.140489695657939\n0.288747346072187,0.141017670135155\n0.289596602972399,0.141506803263209\n0.290870488322718,0.142321667981469\n0.29171974522293,0.142806515167547\n0.292993630573248,0.143716474223407\n0.293842887473461,0.144152836690878\n0.294692144373673,0.144599914013289\n0.295966029723991,0.145297986812692\n0.296815286624204,0.145806138808267\n0.297664543524416,0.146373490377391\n0.298938428874735,0.147107993683593\n0.299787685774947,0.147630342861965\n0.300636942675159,0.14812483341749\n0.301910828025478,0.14896862824409\n0.30276008492569,0.149481066181641\n0.303609341825902,0.150042256709173\n0.304883227176221,0.150851228257215\n0.305732484076433,0.151474832814779\n0.306581740976645,0.15186565714875\n0.307855626326964,0.152640609032355\n0.308704883227176,0.153103490765804\n0.309978768577495,0.153871745865071\n0.310828025477707,0.154253998315089\n0.311677282377919,0.154628750366649\n0.312951167728238,0.155450311869247\n0.31380042462845,0.15592873014236\n0.314649681528662,0.156384915091471\n0.315923566878981,0.157088077446972\n0.316772823779193,0.157607480040234\n0.317622080679406,0.15824796049433\n0.318895966029724,0.159037109560731\n0.319745222929936,0.159532939473124\n0.320594479830149,0.159988052936741\n0.321868365180467,0.160790059829071\n0.322717622080679,0.161237940765603\n0.323991507430998,0.161996016752676\n0.32484076433121,0.162492382407815\n0.325690021231422,0.163010445644211\n0.326963906581741,0.163850490271581\n0.327813163481953,0.164370696478965\n0.328662420382166,0.164887688229866\n0.329936305732484,0.165660765013856\n0.330785562632696,0.166185257163215\n0.331634819532909,0.166628316415024\n0.332908704883227,0.167404607655496\n0.333757961783439,0.168008121860045\n0.334607218683652,0.168457342153445\n0.33588110403397,0.16937453373639\n0.336730360934183,0.169912955197173\n0.337579617834395,0.17043798308928\n0.338853503184713,0.171378479481722\n0.339702760084926,0.171996458740442\n0.340976645435244,0.172873201745986\n0.341825902335456,0.17336233487404\n0.342675159235669,0.173943347883213\n0.343949044585987,0.174805625834587\n0.3447983014862,0.175441284603959\n0.345647558386412,0.175992563890671\n0.34692144373673,0.176869842638962\n0.347770700636943,0.177462374117196\n0.348619957537155,0.177922041394163\n0.349893842887473,0.178596005769949\n0.350743099787686,0.179190680219172\n0.351592356687898,0.179702314542603\n0.352866242038217,0.180611202112969\n0.353715498938429,0.181239092612509\n0.354989384288747,0.182094405908171\n0.35583864118896,0.182662293220042\n0.356687898089172,0.183251342370421\n0.35796178343949,0.184217018671974\n0.358811040339703,0.184774191128904\n0.359660297239915,0.185276449954261\n0.360934182590234,0.186048990995504\n0.361783439490446,0.186598395182601\n0.362632696390658,0.187115654804876\n0.363906581740977,0.187952217104391\n0.364755838641189,0.188471887569027\n0.365605095541401,0.189046739536609\n0.36687898089172,0.189859729155254\n0.367728237791932,0.190418508840425\n0.368577494692144,0.190953447973352\n0.369851380042463,0.191723846043607\n0.370700636942675,0.192283429342898\n0.371974522292994,0.193238390789511\n0.372823779193206,0.193828779296757\n0.373673036093418,0.194483189062276\n0.374946921443737,0.195273409614171\n0.375796178343949,0.195827099743245\n0.376645435244161,0.196389897499019\n0.37791932059448,0.197221370242437\n0.378768577494692,0.197776131857005\n0.379617834394904,0.198347233625358\n0.380891719745223,0.199209511576731\n0.381740976645435,0.199746057937899\n0.382590233545648,0.200377698636669\n0.383864118895966,0.201148900321044\n0.384713375796178,0.201679285640621\n0.385987261146497,0.202637461543716\n0.386836518046709,0.203199187813995\n0.387685774946921,0.203997444507097\n0.38895966029724,0.204898028064884\n0.389808917197452,0.205405108574964\n0.390658174097665,0.205985585841391\n0.391932059447983,0.206899830839228\n0.392781316348195,0.207436377200396\n0.393630573248408,0.207996496242434\n0.394904458598726,0.208829040471346\n0.395753715498938,0.209378712529817\n0.396602972399151,0.210193309376703\n0.397876857749469,0.211010584937325\n0.398726114649682,0.211621867411706\n0.399575371549894,0.21227359846349\n0.400849256900212,0.213154091668263\n0.401698513800425,0.213687423572949\n0.402972399150743,0.214730514701451\n0.403821656050955,0.215283669087777\n0.404670912951168,0.215894951562159\n0.405944798301486,0.216838126668336\n0.406794055201699,0.217499501089567\n0.407643312101911,0.218124445003998\n0.408917197452229,0.21890529005782\n0.409766454352442,0.21945415850217\n0.410615711252654,0.219966864311095\n0.411889596602972,0.220767531846558\n0.412738853503185,0.221361938424407\n0.413588110403397,0.221892055872611\n0.414861995753716,0.222873268713829\n0.415711252653928,0.223460710635967\n0.416985138004246,0.224293790607626\n0.417834394904459,0.224908555409863\n0.418683651804671,0.225573947901697\n0.419957537154989,0.226501854339583\n0.420806794055202,0.227136173752088\n0.421656050955414,0.227735134143287\n0.422929936305733,0.228566339015332\n0.423779193205945,0.229158602622193\n0.424628450106157,0.229743633701969\n0.425902335456476,0.230536800838973\n0.426751592356688,0.231104152408097\n0.4276008492569,0.231653824466568\n0.428874734607219,0.232451009674175\n0.429723991507431,0.233070596161135\n0.430997876857749,0.234124134273205\n0.431847133757962,0.234809081375305\n0.432696390658174,0.235406702409637\n0.433970276008493,0.236260408477057\n0.434819532908705,0.236866869266716\n0.435668789808917,0.237387611216846\n0.436942675159236,0.238290069874248\n0.437791932059448,0.238888226651327\n0.43864118895966,0.239462007133415\n0.439915074309979,0.240285443735627\n0.440764331210191,0.24095458642669\n0.441613588110403,0.24162051466127\n0.442887473460722,0.242537438372842\n0.443736730360934,0.243132112822065\n0.444585987261147,0.243755717379629\n0.445859872611465,0.244653890095054\n0.446709129511677,0.245263565341194\n0.447983014861996,0.246219062530554\n0.448832271762208,0.246785610485557\n0.44968152866242,0.247409750785868\n0.450955414012739,0.248360426290504\n0.451804670912951,0.249010550114046\n0.452653927813163,0.249599599264425\n0.453927813163482,0.250305440333661\n0.454777070063694,0.250924491077874\n0.455626326963907,0.251586401241852\n0.456900212314225,0.252491538612989\n0.457749469214437,0.253106839157973\n0.45859872611465,0.253687048553026\n0.459872611464968,0.25463986702865\n0.46072186836518,0.255292133823181\n0.461995753715499,0.256225665559911\n0.462845010615711,0.256914630732615\n0.463694267515924,0.25749885819827\n0.464968152866242,0.258489178666188\n0.465817409766454,0.259083853115411\n0.466666666666667,0.259606202293783\n0.467940552016985,0.260612595044112\n0.468789808917197,0.26122441326124\n0.46963906581741,0.26178238933229\n0.470912951167728,0.262866732652315\n0.471762208067941,0.263508820334652\n0.472611464968153,0.264227251358443\n0.473885350318471,0.265301683437647\n0.474734607218684,0.265929573937187\n0.475583864118896,0.266603538312973\n0.476857749469214,0.26758314392595\n0.477707006369427,0.268197908728187\n0.478980891719745,0.269296181359635\n0.479830148619958,0.269979253362121\n0.48067940552017,0.270439992124582\n0.481953290870488,0.271484690481325\n0.482802547770701,0.27220419299061\n0.483651804670913,0.272729756625464\n0.484925690021231,0.273823475443562\n0.485774946921444,0.274364843489453\n0.486624203821656,0.275191494548148\n0.487898089171975,0.276254140286917\n0.488747346072187,0.276847743250646\n0.489596602972399,0.277585996756078\n0.490870488322718,0.278598550547998\n0.49171974522293,0.279147954735095\n0.492993630573248,0.280148990057954\n0.493842887473461,0.280830990574946\n0.494692144373673,0.281542724814399\n0.495966029723992,0.282456434069489\n0.496815286624204,0.283112718934622\n0.497664543524416,0.283871866407189\n0.498938428874735,0.284869687273565\n0.499787685774947,0.28561276246372\n0.500636942675159,0.286279226441048\n0.501910828025478,0.287314549299717\n0.50276008492569,0.287780645489649\n0.503609341825902,0.288469074919605\n0.504883227176221,0.289458323902029\n0.505732484076433,0.290144342489623\n0.506581740976645,0.290815628151674\n0.507855626326964,0.291769785984166\n0.508704883227176,0.292423660006938\n0.509978768577495,0.29336871021273\n0.510828025477707,0.294041603103022\n0.511677282377919,0.294759498384066\n0.512951167728238,0.29583848427662\n0.51380042462845,0.29662066868731\n0.514649681528662,0.297310705345507\n0.515923566878981,0.298376297669385\n0.516772823779193,0.299044368874954\n0.517622080679406,0.29965752644895\n0.518895966029724,0.300544180695314\n0.519745222929936,0.301287255885469\n0.520594479830149,0.301945147978844\n0.521868365180467,0.302781978149732\n0.522717622080679,0.30341120800614\n0.523991507430998,0.30432491726123\n0.52484076433121,0.305014686048053\n0.525690021231422,0.305669363684946\n0.526963906581741,0.306676827920769\n0.527813163481953,0.307438921978444\n0.528662420382166,0.30812119036681\n0.529936305732484,0.30908204498364\n0.530785562632696,0.309859139838232\n0.531634819532909,0.310598196957785\n0.532908704883227,0.311711202514776\n0.533757961783439,0.31231123439147\n0.534607218683652,0.31300448550615\n0.53588110403397,0.313752382381028\n0.536730360934183,0.31443893671137\n0.537579617834395,0.315139420353135\n0.538853503184713,0.316100274969965\n0.539702760084926,0.316814955794527\n0.540976645435244,0.317898763371804\n0.541825902335456,0.318607818897522\n0.542675159235669,0.319185081707466\n0.543949044585987,0.320258442301176\n0.5447983014862,0.320947943216626\n0.545647558386412,0.321591905998578\n0.54692144373673,0.322531330905526\n0.547770700636943,0.323294764320069\n0.548619957537155,0.324129719391343\n0.549893842887473,0.32537317830724\n0.550743099787686,0.326009104947986\n0.551592356687898,0.326826112637234\n0.552866242038217,0.327562223171677\n0.553715498938429,0.328406285869651\n0.554989384288747,0.329392052524219\n0.55583864118896,0.330182005204741\n0.556687898089172,0.331035175529414\n0.55796178343949,0.332108803994498\n0.558811040339703,0.332809287636263\n0.559660297239915,0.333586114619482\n0.560934182590234,0.334794482385448\n0.561783439490446,0.335564612584329\n0.562632696390658,0.336254381371153\n0.563906581740977,0.337503197714521\n0.564755838641189,0.338172876148331\n0.565605095541401,0.339018546074546\n0.56687898089172,0.340165839167349\n0.567728237791932,0.340864447709499\n0.568577494692144,0.341627881124042\n0.569851380042463,0.343085637138759\n0.570700636942675,0.343851749267037\n0.571974522292994,0.344990470475887\n0.572823779193206,0.345814174949473\n0.573673036093418,0.346515194333985\n0.574946921443737,0.347624985434494\n0.575796178343949,0.348374489537614\n0.576645435244161,0.349106314130081\n0.57791932059448,0.35024423172481\n0.578768577494692,0.351054810501094\n0.579617834394904,0.351787974450428\n0.580891719745223,0.352759276050826\n0.581740976645435,0.353541460461516\n0.582590233545648,0.354308108332541\n0.583864118895966,0.355409327549097\n0.584713375796178,0.356200351715113\n0.585987261146497,0.357249068142459\n0.586836518046709,0.358048396321055\n0.587685774946921,0.358788792797474\n0.58895966029724,0.359955104757797\n0.589808917197452,0.36075952249249\n0.590658174097665,0.361592870335522\n0.591932059447983,0.362773647350015\n0.592781316348195,0.363470380792551\n0.593630573248408,0.364103092976814\n0.594904458598726,0.365333961938157\n0.595753715498938,0.366041678107007\n0.596602972399151,0.366850649655049\n0.597876857749469,0.368022319042842\n0.598726114649682,0.368847095001922\n0.599575371549894,0.369437483509169\n0.600849256900212,0.370579151303127\n0.601698513800425,0.371355710414973\n0.602972399150743,0.37278614354959\n0.603821656050955,0.373633152832673\n0.604670912951168,0.374442392252088\n0.605944798301486,0.376015065086047\n0.606794055201698,0.376903058689279\n0.607643312101911,0.37760327445967\n0.608917197452229,0.378886110467476\n0.609766454352442,0.379589540694349\n0.610615711252654,0.380298328348694\n0.611889596602972,0.381543394492832\n0.612738853503185,0.382160570137431\n0.613588110403397,0.383018562146828\n0.614861995753715,0.384287736714584\n0.615711252653928,0.38508706489318\n0.616985138004246,0.38635731094643\n0.617834394904459,0.387218249540935\n0.618683651804671,0.388027756831725\n0.619957537154989,0.389265322577405\n0.620806794055202,0.390078580067423\n0.621656050955414,0.390819244415217\n0.622929936305732,0.391829119493402\n0.623779193205945,0.392482993516173\n0.624628450106157,0.393236247818523\n0.625902335456476,0.394492296688976\n0.626751592356688,0.395316536905309\n0.6276008492569,0.39613381246593\n0.628874734607219,0.397294499127409\n0.629723991507431,0.398146062223841\n0.630997876857749,0.399345590234481\n0.631847133757962,0.400097237308589\n0.632696390658174,0.400883707661255\n0.633970276008493,0.40208055695816\n0.634819532908705,0.402927030498495\n0.635668789808917,0.403623496069657\n0.636942675159236,0.404353713433883\n0.637791932059448,0.405130540417103\n0.63864118895966,0.405929868595698\n0.639915074309979,0.407140915075399\n0.640764331210191,0.408268117815188\n0.641613588110403,0.409088339960918\n0.642887473460722,0.410271527817772\n0.643736730360934,0.411039782917039\n0.644585987261146,0.411913579337474\n0.645859872611465,0.413083105754279\n0.646709129511677,0.413904399385503\n0.647983014861996,0.415145983201786\n0.648832271762208,0.415909148744956\n0.64968152866242,0.416667492603402\n0.650955414012739,0.417885235867441\n0.651804670912951,0.418690457216254\n0.652653927813163,0.419362278621052\n0.653927813163482,0.420368939242754\n0.654777070063694,0.421049064660131\n0.655626326963907,0.421903574341673\n0.656900212314225,0.423216411943313\n0.657749469214437,0.424029401561958\n0.65859872611465,0.42483837311\n0.659872611464968,0.426115048076215\n0.66072186836518,0.426996077023735\n0.661995753715499,0.428225338756836\n0.662845010615711,0.429037792632734\n0.663694267515924,0.429905428011577\n0.664968152866242,0.431019505054063\n0.665817409766454,0.431797135651403\n0.666666666666667,0.43264146622075\n0.667940552016985,0.433972786947164\n0.668789808917197,0.43471746936556\n0.66963906581741,0.435421971077928\n0.670912951167728,0.436738023136051\n0.671762208067941,0.437471990699506\n0.672611464968153,0.438396414809537\n0.673885350318471,0.439736307419903\n0.674734607218684,0.440581441603371\n0.675583864118896,0.441396574193004\n0.676857749469214,0.442769950725061\n0.677707006369427,0.443620442335999\n0.678980891719745,0.444943994792581\n0.679830148619957,0.446093966599118\n0.68067940552017,0.447018122837776\n0.681953290870488,0.44834113955161\n0.682802547770701,0.449224579341492\n0.683651804670913,0.450097036405059\n0.684925690021231,0.451372372014406\n0.685774946921444,0.452304832265643\n0.686624203821656,0.453513467902982\n0.687898089171974,0.454850949670987\n0.688747346072187,0.455600989516854\n0.689596602972399,0.456548450565008\n0.690870488322718,0.458056834269321\n0.69171974522293,0.459093496484858\n0.692993630573248,0.460337223272129\n0.693842887473461,0.461300488731321\n0.694692144373673,0.462172410052141\n0.695966029723991,0.463713474064024\n0.696815286624204,0.464585931127591\n0.697664543524416,0.465461334776267\n0.698938428874735,0.466950163870313\n0.699787685774947,0.467836818116677\n0.700636942675159,0.468937233719113\n0.701910828025478,0.470410526273494\n0.70276008492569,0.471272000610747\n0.703609341825902,0.472220801015768\n0.704883227176221,0.473618821714189\n0.705732484076433,0.47455771087839\n0.706581740976645,0.475689735302902\n0.707855626326964,0.477024538357172\n0.708704883227176,0.477857350457457\n0.709978768577495,0.479559940907575\n0.710828025477707,0.480386859837643\n0.711677282377919,0.481331642172062\n0.712951167728238,0.482993784044777\n0.71380042462845,0.483867044722465\n0.714649681528662,0.484863794103347\n0.715923566878981,0.486161630908071\n0.716772823779193,0.487059803623496\n0.717622080679405,0.488054677904764\n0.718895966029724,0.489633779651687\n0.719745222929936,0.49061231377917\n0.720594479830149,0.491396641160847\n0.721868365180467,0.492803501614595\n0.722717622080679,0.493622384403457\n0.723991507430998,0.494936025619218\n0.72484076433121,0.495724906814246\n0.725690021231422,0.496360565583618\n0.726963906581741,0.497669652986029\n0.727813163481953,0.498669081080647\n0.728662420382166,0.499537252202238\n0.729936305732484,0.50093312992967\n0.730785562632696,0.501801033179888\n0.731634819532909,0.502735100659365\n0.732908704883227,0.50409079768077\n0.733757961783439,0.505066385223144\n0.734607218683652,0.506044919350626\n0.73588110403397,0.5078734093463\n0.736730360934183,0.508782296916667\n0.737579617834395,0.509648592938643\n0.738853503184713,0.51104179195234\n0.739702760084926,0.511994342556591\n0.740976645435244,0.513297268917411\n0.741825902335456,0.514152850084446\n0.742675159235669,0.515022628434278\n0.743949044585987,0.516565567545776\n0.7447983014862,0.517574103267093\n0.745647558386412,0.518529868327826\n0.74692144373673,0.519980659686832\n0.747770700636943,0.520953300644097\n0.748619957537155,0.522065234715594\n0.749893842887473,0.523535312813493\n0.750743099787686,0.524606530436215\n0.751592356687898,0.525633549282306\n0.752866242038217,0.527058624989453\n0.753715498938429,0.527990817369316\n0.754989384288747,0.529349460975829\n0.75583864118896,0.530402463345151\n0.756687898089172,0.53137162197456\n0.75796178343949,0.532820538233951\n0.758811040339703,0.533626563196884\n0.759660297239915,0.534579381672509\n0.760934182590234,0.53601544010597\n0.761783439490446,0.537019957756684\n0.762632696390658,0.538028493478002\n0.763906581740977,0.539711261446479\n0.764755838641189,0.540662740565235\n0.765605095541401,0.542035849225919\n0.76687898089172,0.543486908456297\n0.767728237791932,0.544498926505471\n0.768577494692144,0.545484157417291\n0.769851380042463,0.546972986511337\n0.770700636942675,0.547878123882474\n0.771974522292994,0.549470887069447\n0.772823779193206,0.550447813968688\n0.773673036093418,0.551364737680261\n0.774946921443737,0.552843387662112\n0.775796178343949,0.553942731779054\n0.776645435244161,0.555096989527569\n0.77791932059448,0.556877530547382\n0.778768577494692,0.557939908414777\n0.779617834394905,0.559082915565604\n0.780891719745223,0.560600942639363\n0.781740976645435,0.561673767490326\n0.782590233545648,0.562706947378008\n0.783864118895966,0.564179704189642\n0.784713375796178,0.565092341959238\n0.785987261146497,0.566671175834788\n0.786836518046709,0.567725249689604\n0.787685774946921,0.56872307055598\n0.78895966029724,0.570102608129628\n0.789808917197452,0.571280438559013\n0.790658174097665,0.572302099977633\n0.791932059447983,0.573969599277819\n0.792781316348195,0.575073229336737\n0.793630573248408,0.576153018843412\n0.794904458598726,0.57768310012898\n0.795753715498938,0.578661098513716\n0.796602972399151,0.579745709705114\n0.797876857749469,0.581366599386307\n0.798726114649682,0.582477997715057\n0.799575371549894,0.583491890863845\n0.800849256900212,0.585219125480327\n0.801698513800425,0.586297039887387\n0.802972399150743,0.587891946045348\n0.803821656050955,0.58899584397564\n0.804670912951168,0.590114474831475\n0.805944798301486,0.591678843652855\n0.806794055201699,0.592893372460412\n0.807643312101911,0.594055398478758\n0.808917197452229,0.595830849942475\n0.809766454352442,0.596887334639653\n0.810615711252654,0.598048557043879\n0.811889596602972,0.599583192142797\n0.812738853503185,0.600769058713387\n0.813588110403397,0.601699643865009\n0.814861995753716,0.603317051218346\n0.815711252653928,0.604542830623591\n0.816985138004246,0.60610719944497\n0.817834394904459,0.607166630727257\n0.818683651804671,0.608525810076517\n0.819957537154989,0.610292421784907\n0.820806794055202,0.611466502015062\n0.821656050955414,0.612633081846759\n0.822929936305733,0.614470411597759\n0.823779193205945,0.615711191799922\n0.824628450106157,0.616765533526111\n0.825902335456476,0.618422317971356\n0.826751592356688,0.619525948030275\n0.8276008492569,0.62076163867634\n0.828874734607219,0.622581556788061\n0.829723991507431,0.623868678737844\n0.83099787685775,0.625749671522728\n0.831847133757962,0.627029025202678\n0.832696390658174,0.628454636652572\n0.833970276008493,0.63033589730883\n0.834819532908705,0.63161525098878\n0.835668789808917,0.632943893001459\n0.836942675159236,0.635021235503136\n0.837791932059448,0.636053879648071\n0.83864118895966,0.637112507316237\n0.839915074309979,0.638898405763521\n0.840764331210191,0.64034116098132\n0.841613588110403,0.64159238816705\n0.842887473460722,0.64346239822562\n0.843736730360934,0.644716036253712\n0.844585987261147,0.645924939762425\n0.845859872611465,0.647999603550367\n0.846709129511677,0.649225918698359\n0.847983014861996,0.651147092189273\n0.848832271762208,0.652297331867184\n0.84968152866242,0.653404979996705\n0.850955414012739,0.655287580009831\n0.851804670912951,0.656298258702136\n0.852653927813164,0.657705119155884\n0.853927813163482,0.659435032486101\n0.854777070063694,0.660650364907779\n0.855626326963907,0.66193212943009\n0.856900212314225,0.663519535189593\n0.857749469214437,0.664777191288287\n0.85859872611465,0.666103422458604\n0.859872611464968,0.668140584254253\n0.86072186836518,0.669394490153718\n0.861995753715499,0.67137030940483\n0.862845010615711,0.672561265531517\n0.863694267515924,0.67381651078785\n0.864968152866242,0.675845904313667\n0.865817409766454,0.676559513652734\n0.866666666666667,0.677628588304468\n0.867940552016985,0.679653160145561\n0.868789808917197,0.680744200249924\n0.86963906581741,0.681966765198687\n0.870912951167728,0.683869187693453\n0.871762208067941,0.685203187133602\n0.872611464968153,0.686484148041793\n0.873885350318471,0.688462646006641\n0.874734607218684,0.689725123790058\n0.875583864118896,0.691004209598635\n0.876857749469214,0.692855736532432\n0.877707006369427,0.693998475811885\n0.878980891719745,0.695910005933351\n0.879830148619958,0.697383834230479\n0.88067940552017,0.698578808427769\n0.881953290870488,0.700704099905307\n0.882802547770701,0.702196143455835\n0.883651804670913,0.703508981057476\n0.884925690021231,0.705348989522211\n0.885774946921444,0.706789066026275\n0.886624203821656,0.708075384361937\n0.887898089171975,0.710317199886958\n0.888747346072187,0.711511638341501\n0.889596602972399,0.713099044101004\n0.890870488322718,0.715438632677361\n0.89171974522293,0.716959338464856\n0.892993630573248,0.719028912696701\n0.893842887473461,0.720542118085738\n0.894692144373673,0.721826293450411\n0.895966029723992,0.724348034560765\n0.896815286624204,0.726011783661721\n0.897664543524416,0.7274537352654\n0.898938428874735,0.729548489406357\n0.899787685774947,0.731096518073951\n0.900636942675159,0.732971081945871\n0.901910828025478,0.734982260218288\n0.90276008492569,0.736300723118773\n0.903609341825902,0.73861673901426\n0.904883227176221,0.741031063703831\n0.905732484076433,0.742436049057963\n0.906581740976645,0.743864607092966\n0.907855626326964,0.746118208958422\n0.908704883227176,0.747403991551337\n0.909978768577495,0.7507451511933\n0.910828025477707,0.752246034499154\n0.911677282377919,0.753878442649409\n0.912951167728238,0.756245621977239\n0.91380042462845,0.757889548596555\n0.914649681528662,0.759662053475162\n0.915923566878981,0.762281567636852\n0.916772823779193,0.763521008482147\n0.917622080679406,0.765298335045478\n0.918895966029724,0.76721870492227\n0.919745222929936,0.768648066571394\n0.920594479830149,0.770351460635632\n0.921868365180467,0.773215005618602\n0.922717622080679,0.774920274782455\n0.923991507430998,0.778991651788644\n0.92484076433121,0.780636114150707\n0.925690021231423,0.782374599364877\n0.926963906581741,0.784920448898848\n0.927813163481953,0.786424278789811\n0.928662420382166,0.788201873224515\n0.929936305732484,0.79058914290536\n0.930785562632696,0.792250481163955\n0.931634819532909,0.794010128216633\n0.932908704883227,0.796263730082089\n0.93375796178344,0.798155973464662\n0.934607218683652,0.79973320011197\n0.93588110403397,0.802407092162485\n0.936730360934183,0.803944941717886\n0.937579617834395,0.805635210084821\n0.938853503184713,0.808462324560992\n0.939702760084926,0.810137324259637\n0.940976645435244,0.812793804670873\n0.941825902335456,0.814590417973098\n0.942675159235669,0.816377923648622\n0.943949044585987,0.818861359152562\n0.9447983014862,0.822779781604469\n0.945647558386412,0.824487997353431\n0.94692144373673,0.82717662232949\n0.947770700636943,0.829080652052497\n0.948619957537155,0.830811368996835\n0.949893842887473,0.833609553364665\n0.950743099787686,0.835405363052769\n0.951592356687898,0.837306178319295\n0.952866242038217,0.840273121652445\n0.953715498938429,0.842048305244788\n0.954989384288747,0.845073644537366\n0.95583864118896,0.846700963131524\n0.956687898089172,0.848217115105669\n0.95796178343949,0.851024139228825\n0.958811040339703,0.852939687420894\n0.959660297239915,0.855053728300745\n0.960934182590234,0.85783637612891\n0.961783439490446,0.859497178644758\n0.962632696390658,0.861778639133061\n0.963906581740977,0.865286414769356\n0.964755838641189,0.867439564869741\n0.965605095541401,0.870522228636253\n0.96687898089172,0.873655520092362\n0.967728237791932,0.875790187067974\n0.968577494692144,0.878304695651242\n0.969851380042463,0.881386555803634\n0.970700636942675,0.883947406134522\n0.971974522292994,0.88735124767789\n0.972823779193206,0.889430465279182\n0.973673036093418,0.892080784648827\n0.974946921443737,0.895404800522885\n0.975796178343949,0.897746264198857\n0.976645435244161,0.900326401268639\n0.97791932059448,0.904815925488899\n0.978768577494692,0.906886839077612\n0.979617834394904,0.910819726583689\n0.980891719745223,0.916915139688224\n0.981740976645435,0.919726449753357\n0.982590233545648,0.922071931499932\n0.983864118895966,0.926058929023462\n0.984713375796178,0.929193024093691\n0.985987261146497,0.933847557080041\n0.986836518046709,0.941451086017516\n0.987685774946921,0.945305219339777\n0.98895966029724,0.950105474353325\n0.989808917197452,0.953344842873349\n0.990658174097665,0.956849671924535\n0.991932059447983,0.96160867474656\n0.992781316348195,0.96449498879628\n0.993630573248408,0.968057945935521\n0.994904458598726,0.974445338837197\n0.995753715498938,0.978723512543747\n0.996602972399151,0.982169142021384\n0.997876857749469,0.989175585667274\n0.998726114649682,0.992864442352071\n0.999575371549894,0.997097613667869"
    },
    {
      "name": "ModelComparisonData1",
      "format": {
        "type": "csv",
        "parse": {
          "ValidationWeight": "number"
        }
      },
      "values": "\"ValidationWeight\"\n0\n0.000849256900212314\n0.00169851380042463\n0.0029723991507431\n0.00382165605095541\n0.00467091295116773\n0.0059447983014862\n0.00679405520169851\n0.00764331210191083\n0.0089171974522293\n0.00976645435244161\n0.0106157112526539\n0.0118895966029724\n0.0127388535031847\n0.013588110403397\n0.0148619957537155\n0.0157112526539278\n0.0169851380042463\n0.0178343949044586\n0.0186836518046709\n0.0199575371549894\n0.0208067940552017\n0.021656050955414\n0.0229299363057325\n0.0237791932059448\n0.0246284501061571\n0.0259023354564756\n0.0267515923566879\n0.0276008492569002\n0.0288747346072187\n0.029723991507431\n0.0309978768577495\n0.0318471337579618\n0.0326963906581741\n0.0339702760084926\n0.0348195329087049\n0.0356687898089172\n0.0369426751592357\n0.037791932059448\n0.0386411889596603\n0.0399150743099788\n0.0407643312101911\n0.0416135881104034\n0.0428874734607219\n0.0437367303609342\n0.0445859872611465\n0.045859872611465\n0.0467091295116773\n0.0479830148619958\n0.0488322717622081\n0.0496815286624204\n0.0509554140127389\n0.0518046709129512\n0.0526539278131635\n0.053927813163482\n0.0547770700636943\n0.0556263269639066\n0.0569002123142251\n0.0577494692144374\n0.0585987261146497\n0.0598726114649682\n0.0607218683651805\n0.0619957537154989\n0.0628450106157113\n0.0636942675159236\n0.064968152866242\n0.0658174097664544\n0.0666666666666667\n0.0679405520169851\n0.0687898089171975\n0.0696390658174098\n0.0709129511677282\n0.0717622080679405\n0.0726114649681529\n0.0738853503184713\n0.0747346072186836\n0.075583864118896\n0.0768577494692144\n0.0777070063694267\n0.0789808917197452\n0.0798301486199575\n0.0806794055201698\n0.0819532908704883\n0.0828025477707006\n0.0836518046709129\n0.0849256900212314\n0.0857749469214437\n0.086624203821656\n0.0878980891719745\n0.0887473460721868\n0.0895966029723991\n0.0908704883227176\n0.0917197452229299\n0.0929936305732484\n0.0938428874734607\n0.094692144373673\n0.0959660297239915\n0.0968152866242038\n0.0976645435244161\n0.0989384288747346\n0.0997876857749469\n0.100636942675159\n0.101910828025478\n0.10276008492569\n0.103609341825902\n0.104883227176221\n0.105732484076433\n0.106581740976645\n0.107855626326964\n0.108704883227176\n0.109978768577495\n0.110828025477707\n0.111677282377919\n0.112951167728238\n0.11380042462845\n0.114649681528662\n0.115923566878981\n0.116772823779193\n0.117622080679406\n0.118895966029724\n0.119745222929936\n0.120594479830149\n0.121868365180467\n0.122717622080679\n0.123991507430998\n0.12484076433121\n0.125690021231423\n0.126963906581741\n0.127813163481953\n0.128662420382166\n0.129936305732484\n0.130785562632696\n0.131634819532909\n0.132908704883227\n0.133757961783439\n0.134607218683652\n0.13588110403397\n0.136730360934183\n0.137579617834395\n0.138853503184713\n0.139702760084926\n0.140976645435244\n0.141825902335456\n0.142675159235669\n0.143949044585987\n0.1447983014862\n0.145647558386412\n0.14692144373673\n0.147770700636943\n0.148619957537155\n0.149893842887473\n0.150743099787686\n0.151592356687898\n0.152866242038217\n0.153715498938429\n0.154989384288747\n0.15583864118896\n0.156687898089172\n0.15796178343949\n0.158811040339703\n0.159660297239915\n0.160934182590234\n0.161783439490446\n0.162632696390658\n0.163906581740977\n0.164755838641189\n0.165605095541401\n0.16687898089172\n0.167728237791932\n0.168577494692144\n0.169851380042463\n0.170700636942675\n0.171974522292994\n0.172823779193206\n0.173673036093418\n0.174946921443737\n0.175796178343949\n0.176645435244161\n0.17791932059448\n0.178768577494692\n0.179617834394904\n0.180891719745223\n0.181740976645435\n0.182590233545648\n0.183864118895966\n0.184713375796178\n0.185987261146497\n0.186836518046709\n0.187685774946921\n0.18895966029724\n0.189808917197452\n0.190658174097665\n0.191932059447983\n0.192781316348195\n0.193630573248408\n0.194904458598726\n0.195753715498938\n0.196602972399151\n0.197876857749469\n0.198726114649682\n0.199575371549894\n0.200849256900212\n0.201698513800425\n0.202972399150743\n0.203821656050955\n0.204670912951168\n0.205944798301486\n0.206794055201699\n0.207643312101911\n0.208917197452229\n0.209766454352442\n0.210615711252654\n0.211889596602972\n0.212738853503185\n0.213588110403397\n0.214861995753716\n0.215711252653928\n0.216985138004246\n0.217834394904459\n0.218683651804671\n0.219957537154989\n0.220806794055202\n0.221656050955414\n0.222929936305732\n0.223779193205945\n0.224628450106157\n0.225902335456476\n0.226751592356688\n0.2276008492569\n0.228874734607219\n0.229723991507431\n0.230997876857749\n0.231847133757962\n0.232696390658174\n0.233970276008493\n0.234819532908705\n0.235668789808917\n0.236942675159236\n0.237791932059448\n0.23864118895966\n0.239915074309979\n0.240764331210191\n0.241613588110403\n0.242887473460722\n0.243736730360934\n0.244585987261146\n0.245859872611465\n0.246709129511677\n0.247983014861996\n0.248832271762208\n0.24968152866242\n0.250955414012739\n0.251804670912951\n0.252653927813163\n0.253927813163482\n0.254777070063694\n0.255626326963907\n0.256900212314225\n0.257749469214437\n0.25859872611465\n0.259872611464968\n0.26072186836518\n0.261995753715499\n0.262845010615711\n0.263694267515924\n0.264968152866242\n0.265817409766454\n0.266666666666667\n0.267940552016985\n0.268789808917197\n0.26963906581741\n0.270912951167728\n0.271762208067941\n0.272611464968153\n0.273885350318471\n0.274734607218684\n0.275583864118896\n0.276857749469214\n0.277707006369427\n0.278980891719745\n0.279830148619958\n0.28067940552017\n0.281953290870488\n0.282802547770701\n0.283651804670913\n0.284925690021231\n0.285774946921444\n0.286624203821656\n0.287898089171975\n0.288747346072187\n0.289596602972399\n0.290870488322718\n0.29171974522293\n0.292993630573248\n0.293842887473461\n0.294692144373673\n0.295966029723991\n0.296815286624204\n0.297664543524416\n0.298938428874735\n0.299787685774947\n0.300636942675159\n0.301910828025478\n0.30276008492569\n0.303609341825902\n0.304883227176221\n0.305732484076433\n0.306581740976645\n0.307855626326964\n0.308704883227176\n0.309978768577495\n0.310828025477707\n0.311677282377919\n0.312951167728238\n0.31380042462845\n0.314649681528662\n0.315923566878981\n0.316772823779193\n0.317622080679406\n0.318895966029724\n0.319745222929936\n0.320594479830149\n0.321868365180467\n0.322717622080679\n0.323991507430998\n0.32484076433121\n0.325690021231422\n0.326963906581741\n0.327813163481953\n0.328662420382166\n0.329936305732484\n0.330785562632696\n0.331634819532909\n0.332908704883227\n0.333757961783439\n0.334607218683652\n0.33588110403397\n0.336730360934183\n0.337579617834395\n0.338853503184713\n0.339702760084926\n0.340976645435244\n0.341825902335456\n0.342675159235669\n0.343949044585987\n0.3447983014862\n0.345647558386412\n0.34692144373673\n0.347770700636943\n0.348619957537155\n0.349893842887473\n0.350743099787686\n0.351592356687898\n0.352866242038217\n0.353715498938429\n0.354989384288747\n0.35583864118896\n0.356687898089172\n0.35796178343949\n0.358811040339703\n0.359660297239915\n0.360934182590234\n0.361783439490446\n0.362632696390658\n0.363906581740977\n0.364755838641189\n0.365605095541401\n0.36687898089172\n0.367728237791932\n0.368577494692144\n0.369851380042463\n0.370700636942675\n0.371974522292994\n0.372823779193206\n0.373673036093418\n0.374946921443737\n0.375796178343949\n0.376645435244161\n0.37791932059448\n0.378768577494692\n0.379617834394904\n0.380891719745223\n0.381740976645435\n0.382590233545648\n0.383864118895966\n0.384713375796178\n0.385987261146497\n0.386836518046709\n0.387685774946921\n0.38895966029724\n0.389808917197452\n0.390658174097665\n0.391932059447983\n0.392781316348195\n0.393630573248408\n0.394904458598726\n0.395753715498938\n0.396602972399151\n0.397876857749469\n0.398726114649682\n0.399575371549894\n0.400849256900212\n0.401698513800425\n0.402972399150743\n0.403821656050955\n0.404670912951168\n0.405944798301486\n0.406794055201699\n0.407643312101911\n0.408917197452229\n0.409766454352442\n0.410615711252654\n0.411889596602972\n0.412738853503185\n0.413588110403397\n0.414861995753716\n0.415711252653928\n0.416985138004246\n0.417834394904459\n0.418683651804671\n0.419957537154989\n0.420806794055202\n0.421656050955414\n0.422929936305733\n0.423779193205945\n0.424628450106157\n0.425902335456476\n0.426751592356688\n0.4276008492569\n0.428874734607219\n0.429723991507431\n0.430997876857749\n0.431847133757962\n0.432696390658174\n0.433970276008493\n0.434819532908705\n0.435668789808917\n0.436942675159236\n0.437791932059448\n0.43864118895966\n0.439915074309979\n0.440764331210191\n0.441613588110403\n0.442887473460722\n0.443736730360934\n0.444585987261147\n0.445859872611465\n0.446709129511677\n0.447983014861996\n0.448832271762208\n0.44968152866242\n0.450955414012739\n0.451804670912951\n0.452653927813163\n0.453927813163482\n0.454777070063694\n0.455626326963907\n0.456900212314225\n0.457749469214437\n0.45859872611465\n0.459872611464968\n0.46072186836518\n0.461995753715499\n0.462845010615711\n0.463694267515924\n0.464968152866242\n0.465817409766454\n0.466666666666667\n0.467940552016985\n0.468789808917197\n0.46963906581741\n0.470912951167728\n0.471762208067941\n0.472611464968153\n0.473885350318471\n0.474734607218684\n0.475583864118896\n0.476857749469214\n0.477707006369427\n0.478980891719745\n0.479830148619958\n0.48067940552017\n0.481953290870488\n0.482802547770701\n0.483651804670913\n0.484925690021231\n0.485774946921444\n0.486624203821656\n0.487898089171975\n0.488747346072187\n0.489596602972399\n0.490870488322718\n0.49171974522293\n0.492993630573248\n0.493842887473461\n0.494692144373673\n0.495966029723992\n0.496815286624204\n0.497664543524416\n0.498938428874735\n0.499787685774947\n0.500636942675159\n0.501910828025478\n0.50276008492569\n0.503609341825902\n0.504883227176221\n0.505732484076433\n0.506581740976645\n0.507855626326964\n0.508704883227176\n0.509978768577495\n0.510828025477707\n0.511677282377919\n0.512951167728238\n0.51380042462845\n0.514649681528662\n0.515923566878981\n0.516772823779193\n0.517622080679406\n0.518895966029724\n0.519745222929936\n0.520594479830149\n0.521868365180467\n0.522717622080679\n0.523991507430998\n0.52484076433121\n0.525690021231422\n0.526963906581741\n0.527813163481953\n0.528662420382166\n0.529936305732484\n0.530785562632696\n0.531634819532909\n0.532908704883227\n0.533757961783439\n0.534607218683652\n0.53588110403397\n0.536730360934183\n0.537579617834395\n0.538853503184713\n0.539702760084926\n0.540976645435244\n0.541825902335456\n0.542675159235669\n0.543949044585987\n0.5447983014862\n0.545647558386412\n0.54692144373673\n0.547770700636943\n0.548619957537155\n0.549893842887473\n0.550743099787686\n0.551592356687898\n0.552866242038217\n0.553715498938429\n0.554989384288747\n0.55583864118896\n0.556687898089172\n0.55796178343949\n0.558811040339703\n0.559660297239915\n0.560934182590234\n0.561783439490446\n0.562632696390658\n0.563906581740977\n0.564755838641189\n0.565605095541401\n0.56687898089172\n0.567728237791932\n0.568577494692144\n0.569851380042463\n0.570700636942675\n0.571974522292994\n0.572823779193206\n0.573673036093418\n0.574946921443737\n0.575796178343949\n0.576645435244161\n0.57791932059448\n0.578768577494692\n0.579617834394904\n0.580891719745223\n0.581740976645435\n0.582590233545648\n0.583864118895966\n0.584713375796178\n0.585987261146497\n0.586836518046709\n0.587685774946921\n0.58895966029724\n0.589808917197452\n0.590658174097665\n0.591932059447983\n0.592781316348195\n0.593630573248408\n0.594904458598726\n0.595753715498938\n0.596602972399151\n0.597876857749469\n0.598726114649682\n0.599575371549894\n0.600849256900212\n0.601698513800425\n0.602972399150743\n0.603821656050955\n0.604670912951168\n0.605944798301486\n0.606794055201698\n0.607643312101911\n0.608917197452229\n0.609766454352442\n0.610615711252654\n0.611889596602972\n0.612738853503185\n0.613588110403397\n0.614861995753715\n0.615711252653928\n0.616985138004246\n0.617834394904459\n0.618683651804671\n0.619957537154989\n0.620806794055202\n0.621656050955414\n0.622929936305732\n0.623779193205945\n0.624628450106157\n0.625902335456476\n0.626751592356688\n0.6276008492569\n0.628874734607219\n0.629723991507431\n0.630997876857749\n0.631847133757962\n0.632696390658174\n0.633970276008493\n0.634819532908705\n0.635668789808917\n0.636942675159236\n0.637791932059448\n0.63864118895966\n0.639915074309979\n0.640764331210191\n0.641613588110403\n0.642887473460722\n0.643736730360934\n0.644585987261146\n0.645859872611465\n0.646709129511677\n0.647983014861996\n0.648832271762208\n0.64968152866242\n0.650955414012739\n0.651804670912951\n0.652653927813163\n0.653927813163482\n0.654777070063694\n0.655626326963907\n0.656900212314225\n0.657749469214437\n0.65859872611465\n0.659872611464968\n0.66072186836518\n0.661995753715499\n0.662845010615711\n0.663694267515924\n0.664968152866242\n0.665817409766454\n0.666666666666667\n0.667940552016985\n0.668789808917197\n0.66963906581741\n0.670912951167728\n0.671762208067941\n0.672611464968153\n0.673885350318471\n0.674734607218684\n0.675583864118896\n0.676857749469214\n0.677707006369427\n0.678980891719745\n0.679830148619957\n0.68067940552017\n0.681953290870488\n0.682802547770701\n0.683651804670913\n0.684925690021231\n0.685774946921444\n0.686624203821656\n0.687898089171974\n0.688747346072187\n0.689596602972399\n0.690870488322718\n0.69171974522293\n0.692993630573248\n0.693842887473461\n0.694692144373673\n0.695966029723991\n0.696815286624204\n0.697664543524416\n0.698938428874735\n0.699787685774947\n0.700636942675159\n0.701910828025478\n0.70276008492569\n0.703609341825902\n0.704883227176221\n0.705732484076433\n0.706581740976645\n0.707855626326964\n0.708704883227176\n0.709978768577495\n0.710828025477707\n0.711677282377919\n0.712951167728238\n0.71380042462845\n0.714649681528662\n0.715923566878981\n0.716772823779193\n0.717622080679405\n0.718895966029724\n0.719745222929936\n0.720594479830149\n0.721868365180467\n0.722717622080679\n0.723991507430998\n0.72484076433121\n0.725690021231422\n0.726963906581741\n0.727813163481953\n0.728662420382166\n0.729936305732484\n0.730785562632696\n0.731634819532909\n0.732908704883227\n0.733757961783439\n0.734607218683652\n0.73588110403397\n0.736730360934183\n0.737579617834395\n0.738853503184713\n0.739702760084926\n0.740976645435244\n0.741825902335456\n0.742675159235669\n0.743949044585987\n0.7447983014862\n0.745647558386412\n0.74692144373673\n0.747770700636943\n0.748619957537155\n0.749893842887473\n0.750743099787686\n0.751592356687898\n0.752866242038217\n0.753715498938429\n0.754989384288747\n0.75583864118896\n0.756687898089172\n0.75796178343949\n0.758811040339703\n0.759660297239915\n0.760934182590234\n0.761783439490446\n0.762632696390658\n0.763906581740977\n0.764755838641189\n0.765605095541401\n0.76687898089172\n0.767728237791932\n0.768577494692144\n0.769851380042463\n0.770700636942675\n0.771974522292994\n0.772823779193206\n0.773673036093418\n0.774946921443737\n0.775796178343949\n0.776645435244161\n0.77791932059448\n0.778768577494692\n0.779617834394905\n0.780891719745223\n0.781740976645435\n0.782590233545648\n0.783864118895966\n0.784713375796178\n0.785987261146497\n0.786836518046709\n0.787685774946921\n0.78895966029724\n0.789808917197452\n0.790658174097665\n0.791932059447983\n0.792781316348195\n0.793630573248408\n0.794904458598726\n0.795753715498938\n0.796602972399151\n0.797876857749469\n0.798726114649682\n0.799575371549894\n0.800849256900212\n0.801698513800425\n0.802972399150743\n0.803821656050955\n0.804670912951168\n0.805944798301486\n0.806794055201699\n0.807643312101911\n0.808917197452229\n0.809766454352442\n0.810615711252654\n0.811889596602972\n0.812738853503185\n0.813588110403397\n0.814861995753716\n0.815711252653928\n0.816985138004246\n0.817834394904459\n0.818683651804671\n0.819957537154989\n0.820806794055202\n0.821656050955414\n0.822929936305733\n0.823779193205945\n0.824628450106157\n0.825902335456476\n0.826751592356688\n0.8276008492569\n0.828874734607219\n0.829723991507431\n0.83099787685775\n0.831847133757962\n0.832696390658174\n0.833970276008493\n0.834819532908705\n0.835668789808917\n0.836942675159236\n0.837791932059448\n0.83864118895966\n0.839915074309979\n0.840764331210191\n0.841613588110403\n0.842887473460722\n0.843736730360934\n0.844585987261147\n0.845859872611465\n0.846709129511677\n0.847983014861996\n0.848832271762208\n0.84968152866242\n0.850955414012739\n0.851804670912951\n0.852653927813164\n0.853927813163482\n0.854777070063694\n0.855626326963907\n0.856900212314225\n0.857749469214437\n0.85859872611465\n0.859872611464968\n0.86072186836518\n0.861995753715499\n0.862845010615711\n0.863694267515924\n0.864968152866242\n0.865817409766454\n0.866666666666667\n0.867940552016985\n0.868789808917197\n0.86963906581741\n0.870912951167728\n0.871762208067941\n0.872611464968153\n0.873885350318471\n0.874734607218684\n0.875583864118896\n0.876857749469214\n0.877707006369427\n0.878980891719745\n0.879830148619958\n0.88067940552017\n0.881953290870488\n0.882802547770701\n0.883651804670913\n0.884925690021231\n0.885774946921444\n0.886624203821656\n0.887898089171975\n0.888747346072187\n0.889596602972399\n0.890870488322718\n0.89171974522293\n0.892993630573248\n0.893842887473461\n0.894692144373673\n0.895966029723992\n0.896815286624204\n0.897664543524416\n0.898938428874735\n0.899787685774947\n0.900636942675159\n0.901910828025478\n0.90276008492569\n0.903609341825902\n0.904883227176221\n0.905732484076433\n0.906581740976645\n0.907855626326964\n0.908704883227176\n0.909978768577495\n0.910828025477707\n0.911677282377919\n0.912951167728238\n0.91380042462845\n0.914649681528662\n0.915923566878981\n0.916772823779193\n0.917622080679406\n0.918895966029724\n0.919745222929936\n0.920594479830149\n0.921868365180467\n0.922717622080679\n0.923991507430998\n0.92484076433121\n0.925690021231423\n0.926963906581741\n0.927813163481953\n0.928662420382166\n0.929936305732484\n0.930785562632696\n0.931634819532909\n0.932908704883227\n0.93375796178344\n0.934607218683652\n0.93588110403397\n0.936730360934183\n0.937579617834395\n0.938853503184713\n0.939702760084926\n0.940976645435244\n0.941825902335456\n0.942675159235669\n0.943949044585987\n0.9447983014862\n0.945647558386412\n0.94692144373673\n0.947770700636943\n0.948619957537155\n0.949893842887473\n0.950743099787686\n0.951592356687898\n0.952866242038217\n0.953715498938429\n0.954989384288747\n0.95583864118896\n0.956687898089172\n0.95796178343949\n0.958811040339703\n0.959660297239915\n0.960934182590234\n0.961783439490446\n0.962632696390658\n0.963906581740977\n0.964755838641189\n0.965605095541401\n0.96687898089172\n0.967728237791932\n0.968577494692144\n0.969851380042463\n0.970700636942675\n0.971974522292994\n0.972823779193206\n0.973673036093418\n0.974946921443737\n0.975796178343949\n0.976645435244161\n0.97791932059448\n0.978768577494692\n0.979617834394904\n0.980891719745223\n0.981740976645435\n0.982590233545648\n0.983864118895966\n0.984713375796178\n0.985987261146497\n0.986836518046709\n0.987685774946921\n0.98895966029724\n0.989808917197452\n0.990658174097665\n0.991932059447983\n0.992781316348195\n0.993630573248408\n0.994904458598726\n0.995753715498938\n0.996602972399151\n0.997876857749469\n0.998726114649682\n0.999575371549894"
    },
    {
      "name": "ModelComparisonData2",
      "format": {
        "type": "csv",
        "parse": {
          "TrainWeight": "number",
          "TrainLoss": "number"
        }
      },
      "values": "\"TrainWeight\",\"TrainLoss\"\n0,0\n0.000849256900212314,0.000183009724495558\n0.00191082802547771,0.000410903585801705\n0.0029723991507431,0.000750607037912802\n0.00382165605095541,0.000979703152883519\n0.00488322717622081,0.00126837761612067\n0.0059447983014862,0.00157067762088961\n0.00679405520169851,0.00181192985624653\n0.00785562632696391,0.00212531731147761\n0.0089171974522293,0.00248425682222182\n0.00997876857749469,0.00280913247913656\n0.010828025477707,0.00310034503344336\n0.0118895966029724,0.00344966651487101\n0.0129511677282378,0.00378242361247571\n0.0138004246284501,0.00404544999753538\n0.0148619957537155,0.00435589861047529\n0.0159235668789809,0.00472084938954234\n0.0169851380042463,0.00509955929388169\n0.0178343949044586,0.00537621122047315\n0.018895966029724,0.00577522585336967\n0.0199575371549894,0.00616181719839897\n0.0208067940552017,0.00643993854613602\n0.0218683651804671,0.00681637752688673\n0.0229299363057325,0.00719829344099827\n0.0239915074309979,0.00755375977448927\n0.0248407643312102,0.00783054528482124\n0.0259023354564756,0.0082302278364203\n0.026963906581741,0.00863084547420291\n0.0278131634819533,0.00896707574906081\n0.0288747346072187,0.00939360863250191\n0.0299363057324841,0.00980798539555682\n0.0309978768577495,0.0102331824415928\n0.0318471337579618,0.0105320092691086\n0.0329087048832272,0.010929955232081\n0.0339702760084926,0.0113550186943766\n0.0348195329087049,0.011596671680955\n0.0358811040339703,0.0119984915724022\n0.0369426751592357,0.0124305013892041\n0.037791932059448,0.0128168255667524\n0.0388535031847134,0.0132691401121115\n0.0399150743099788,0.0136360946472862\n0.0409766454352442,0.0140482004867524\n0.0418259023354565,0.014361721525724\n0.0428874734607219,0.0147919947538993\n0.0439490445859873,0.0151987572437453\n0.0447983014861996,0.0155496817300591\n0.045859872611465,0.0160106792185511\n0.0469214437367304,0.0164549787394798\n0.0479830148619958,0.0169135717206427\n0.0488322717622081,0.0172614237809248\n0.0498938428874735,0.0176875559131444\n0.0509554140127389,0.0181375995349148\n0.0518046709129512,0.0184973405481021\n0.0528662420382166,0.0189329571258977\n0.053927813163482,0.0193680393687313\n0.0549893842887473,0.0198163464018752\n0.0558386411889597,0.0201620611223091\n0.0569002123142251,0.0206028874659846\n0.0579617834394904,0.0210982159757871\n0.0588110403397028,0.0214029204878852\n0.0598726114649682,0.0217713444442055\n0.0609341825902335,0.0222469025604129\n0.0619957537154989,0.0226842557268351\n0.0628450106157113,0.023001784278022\n0.0639065817409766,0.0234634496852166\n0.064968152866242,0.0239217754988985\n0.0658174097664544,0.024284989689339\n0.0668789808917197,0.0247204726833941\n0.0679405520169851,0.0250938392381132\n0.0687898089171975,0.0254323404365597\n0.0698513800424628,0.0259290047837673\n0.0709129511677282,0.0263334963500247\n0.0719745222929936,0.0268160008207385\n0.0728237791932059,0.0271764097526283\n0.0738853503184713,0.0276583798883801\n0.0749469214437367,0.0281184422906886\n0.075796178343949,0.0284948812714394\n0.0768577494692144,0.0289438562232858\n0.0779193205944798,0.0293940334287967\n0.0789808917197452,0.029849286816447\n0.0798301486199575,0.0302091614133748\n0.0808917197452229,0.0306622774611769\n0.0819532908704883,0.0310944208617193\n0.0828025477707006,0.0314417385870394\n0.083864118895966,0.0318531765078031\n0.0849256900212314,0.0323065597230862\n0.0859872611464968,0.03280482707518\n0.0868365180467091,0.0331644345046267\n0.0878980891719745,0.033680468494208\n0.0889596602972399,0.0341418667339216\n0.0898089171974522,0.0345284580789509\n0.0908704883227176,0.0349957340032469\n0.091932059447983,0.0354099771825613\n0.0929936305732484,0.0359300186843578\n0.0938428874734607,0.0363240907188555\n0.0949044585987261,0.0368031220123162\n0.0959660297239915,0.0372581082324854\n0.0968152866242038,0.0376780955126416\n0.0978768577494692,0.0381449706857161\n0.0989384288747346,0.038671691374538\n0.0997876857749469,0.0390665649114787\n0.100849256900212,0.039617597841073\n0.101910828025478,0.0401391087640151\n0.102972399150743,0.0406209453160264\n0.103821656050955,0.0410432035197713\n0.104883227176221,0.0414852321171113\n0.105944798301486,0.0419589200609516\n0.106794055201699,0.0424221884730323\n0.107855626326964,0.0428997503453474\n0.108917197452229,0.0433873309982005\n0.109978768577495,0.0438332335240152\n0.110828025477707,0.0442414654350068\n0.111889596602972,0.0447183593886193\n0.112951167728238,0.0451979250170419\n0.11380042462845,0.0456000120759701\n0.114861995753716,0.0461058937012728\n0.115923566878981,0.0466307442177276\n0.116985138004246,0.0471457095373848\n0.117834394904459,0.0475353733084458\n0.118895966029724,0.0480357780003877\n0.119957537154989,0.04854419771676\n0.120806794055202,0.0489649864993593\n0.121868365180467,0.0494818219913836\n0.122929936305732,0.0500248398965474\n0.123991507430998,0.0504788910305331\n0.12484076433121,0.0508852527691575\n0.125902335456476,0.0513617459715485\n0.126963906581741,0.0518931420912882\n0.127813163481953,0.0522615660476084\n0.128874734607219,0.0527353875751893\n0.129936305732484,0.053258635086758\n0.130997876857749,0.0537665204681683\n0.131847133757962,0.0542009347922993\n0.132908704883227,0.0547259188924946\n0.133970276008493,0.0552324684364998\n0.134819532908705,0.0556870539054476\n0.13588110403397,0.0561642150265411\n0.136942675159236,0.0566997522422365\n0.137791932059448,0.0571453876005702\n0.138853503184713,0.0576399146079297\n0.139915074309979,0.0581351095339918\n0.140976645435244,0.0586572883756364\n0.141825902335456,0.0591371211715401\n0.142887473460722,0.059612278536526\n0.143949044585987,0.0601410029814555\n0.1447983014862,0.0605426892891622\n0.145859872611465,0.0611233778091492\n0.14692144373673,0.061700325984402\n0.147983014861996,0.0622546985075089\n0.148832271762208,0.062697795774773\n0.149893842887473,0.0632067498261074\n0.150955414012739,0.0636951319814035\n0.151804670912951,0.0641403665885157\n0.152866242038217,0.0646588050854261\n0.153927813163482,0.0652179866231914\n0.154989384288747,0.0657556611787349\n0.15583864118896,0.0661771178800367\n0.156900212314225,0.0666959571281687\n0.15796178343949,0.0672182695535538\n0.158811040339703,0.0676305089767606\n0.159872611464968,0.0681719238770383\n0.160934182590234,0.0687762567190952\n0.161995753715499,0.0692770621622586\n0.162845010615711,0.0697160183335669\n0.163906581740977,0.0702550287265155\n0.164968152866242,0.0707985809666413\n0.165817409766454,0.0712317930371078\n0.16687898089172,0.0718776704224626\n0.167940552016985,0.0724468707407659\n0.168789808917197,0.0729030592145997\n0.169851380042463,0.073435657588004\n0.170912951167728,0.0739773396557627\n0.171974522292994,0.0745385249496356\n0.172823779193206,0.0749761452835388\n0.173885350318471,0.0755040682260253\n0.174946921443737,0.0760595094190563\n0.175796178343949,0.0764924543220418\n0.176857749469214,0.0770319990499524\n0.17791932059448,0.0775314686557108\n0.178980891719745,0.0781140273480649\n0.179830148619958,0.0785567238641074\n0.180891719745223,0.0791098941335498\n0.181953290870488,0.0796797623705557\n0.182802547770701,0.0801298059923262\n0.183864118895966,0.0806860486878002\n0.184925690021231,0.081253512417477\n0.185987261146497,0.0817941258153117\n0.186836518046709,0.0822593979835\n0.187898089171975,0.0828631964905948\n0.18895966029724,0.0834366714885944\n0.189808917197452,0.0839006078193777\n0.190870488322718,0.0844967920532636\n0.191932059447983,0.0850891023586747\n0.192993630573248,0.0856532264948388\n0.193842887473461,0.0861107508060777\n0.194904458598726,0.0866158309289373\n0.195966029723992,0.0871931798554116\n0.196815286624204,0.087676619412309\n0.197876857749469,0.0882081491157892\n0.198938428874735,0.0887885704682952\n0.199787685774947,0.0892314005680782\n0.200849256900212,0.0898319930654009\n0.201910828025478,0.0903653929412481\n0.202972399150743,0.0909205669667982\n0.203821656050955,0.0913600574730685\n0.204883227176221,0.0919157658335806\n0.205944798301486,0.0924492992931683\n0.206794055201699,0.0929113654515845\n0.207855626326964,0.093473752999122\n0.208917197452229,0.0940436212361278\n0.209978768577495,0.094644347317191\n0.210828025477707,0.0950986656186577\n0.211889596602972,0.095658915826347\n0.212951167728238,0.096248287289467\n0.21380042462845,0.0967442837179721\n0.214861995753716,0.0973323193436871\n0.215923566878981,0.0978607766211355\n0.216985138004246,0.0984347859540972\n0.217834394904459,0.0989279771240516\n0.218895966029724,0.0994559000665381\n0.219957537154989,0.100030978069424\n0.220806794055202,0.100446690669884\n0.221868365180467,0.101026577687428\n0.222929936305732,0.101639326305137\n0.223991507430998,0.102212400551915\n0.22484076433121,0.102638532684134\n0.225902335456476,0.103243934196115\n0.226963906581741,0.103828095893355\n0.227813163481953,0.104303386842082\n0.228874734607219,0.104803657950283\n0.229936305732484,0.105403315361422\n0.230997876857749,0.105939119744599\n0.231847133757962,0.106358038354831\n0.232908704883227,0.106946341148027\n0.233970276008493,0.107575386982077\n0.234819532908705,0.108024896268886\n0.23588110403397,0.108658483950114\n0.236942675159236,0.10915728563717\n0.237791932059448,0.109621221967953\n0.238853503184713,0.110211528517257\n0.239915074309979,0.110814792689389\n0.240976645435244,0.111398286467927\n0.241825902335456,0.111826555939995\n0.242887473460722,0.112377588869589\n0.243949044585987,0.112914862673911\n0.2447983014862,0.113423148806543\n0.245859872611465,0.11405660290403\n0.24692144373673,0.114612578432023\n0.247983014861996,0.115141169293212\n0.248832271762208,0.115625009601331\n0.249893842887473,0.116190870326122\n0.250955414012739,0.116810030963375\n0.251804670912951,0.117353583203501\n0.252866242038217,0.118030051265432\n0.253927813163482,0.118682607837812\n0.254989384288747,0.119329019558129\n0.25583864118896,0.119817535297166\n0.256900212314225,0.120392746883792\n0.25796178343949,0.120964485293165\n0.258811040339703,0.121467962411138\n0.259872611464968,0.122053860697005\n0.260934182590234,0.122635751470657\n0.261995753715499,0.123253977021726\n0.262845010615711,0.123743427846947\n0.263906581740977,0.124358848139466\n0.264968152866242,0.124972799010839\n0.265817409766454,0.125496848024851\n0.26687898089172,0.126119882590579\n0.267940552016985,0.126717135494389\n0.268789808917197,0.127169316456007\n0.269851380042463,0.127759623005311\n0.270912951167728,0.128426473037925\n0.271974522292994,0.129005825720507\n0.272823779193206,0.129510237924664\n0.273885350318471,0.130103884067481\n0.274946921443737,0.130695526454189\n0.275796178343949,0.131151581344283\n0.276857749469214,0.131726659347168\n0.27791932059448,0.132319904738763\n0.278980891719745,0.132952423750067\n0.279830148619958,0.133448153011091\n0.280891719745223,0.134110728364009\n0.281953290870488,0.134713191033699\n0.282802547770701,0.135203844112584\n0.283864118895966,0.1357763840244\n0.284925690021231,0.136383388541267\n0.285987261146497,0.137032204768913\n0.286836518046709,0.137487458156563\n0.287898089171975,0.138019789362486\n0.28895966029724,0.138649770282721\n0.289808917197452,0.139196662116359\n0.290870488322718,0.139848150018815\n0.291932059447983,0.14044660517629\n0.292993630573248,0.141023286184062\n0.293842887473461,0.141523423708522\n0.294904458598726,0.142155809136086\n0.295966029723991,0.142798480511668\n0.296815286624204,0.143325735535452\n0.297876857749469,0.143998329668909\n0.298938428874735,0.144633253187542\n0.299787685774947,0.145153161105598\n0.300849256900212,0.145768714981857\n0.301910828025478,0.146416061788358\n0.302972399150743,0.147069954198143\n0.303821656050955,0.147657989823858\n0.304883227176221,0.148298256692111\n0.305944798301486,0.148956423781593\n0.306794055201699,0.149447210444218\n0.307855626326964,0.150089347484839\n0.308917197452229,0.150776769413491\n0.309978768577495,0.151460718164891\n0.310828025477707,0.152010816008301\n0.311889596602972,0.152647476115561\n0.312951167728238,0.153308715631074\n0.31380042462845,0.153804712059579\n0.314861995753715,0.154496007916707\n0.315923566878981,0.155178754414442\n0.316985138004246,0.155878733214702\n0.317834394904459,0.1564149383491\n0.318895966029724,0.157072170352398\n0.319957537154989,0.157610913577865\n0.320806794055202,0.158098494230719\n0.321868365180467,0.158813567993656\n0.322929936305732,0.159465189479853\n0.323991507430998,0.160126696162847\n0.32484076433121,0.160662767713505\n0.325902335456476,0.161296088227252\n0.326963906581741,0.161961201671239\n0.327813163481953,0.162495269465789\n0.328874734607219,0.16316372250329\n0.329936305732484,0.163833912129417\n0.330997876857749,0.164517994464557\n0.331847133757962,0.165038303133834\n0.332908704883227,0.165706489003854\n0.333970276008493,0.166361717251044\n0.334819532908705,0.166822046820834\n0.33588110403397,0.167477809402986\n0.336942675159236,0.168264350467096\n0.337791932059448,0.168797883926683\n0.338853503184713,0.169493053712285\n0.339915074309979,0.170226695615154\n0.340976645435244,0.170893812815249\n0.341825902335456,0.171454730941641\n0.342887473460722,0.172156312746788\n0.343949044585987,0.172874859686979\n0.3447983014862,0.173397172112364\n0.345859872611465,0.174082456701168\n0.34692144373673,0.174705224099415\n0.347983014861996,0.175351635819732\n0.348832271762208,0.175957972417896\n0.349893842887473,0.176615204421194\n0.350955414012739,0.177319992236113\n0.351804670912951,0.177801962371865\n0.352866242038217,0.17840856613751\n0.353927813163482,0.179086370036846\n0.354989384288747,0.179708469516391\n0.35583864118896,0.180250819502852\n0.356900212314225,0.180888013945074\n0.35796178343949,0.181635014221993\n0.358811040339703,0.182184978481663\n0.359872611464968,0.182780094045625\n0.360934182590234,0.18346537863443\n0.361995753715499,0.184116332201924\n0.362845010615711,0.184641984220821\n0.363906581740977,0.185263415781663\n0.364968152866242,0.185942555518404\n0.365817409766454,0.186491985443113\n0.36687898089172,0.187191964243373\n0.367940552016985,0.187901961824171\n0.368789808917197,0.188453529088728\n0.369851380042463,0.189142687606007\n0.370912951167728,0.189763184080665\n0.371974522292994,0.190419748165261\n0.372823779193206,0.190989215651045\n0.373885350318471,0.191667286717862\n0.374946921443737,0.192354040727812\n0.375796178343949,0.192846831146545\n0.376857749469214,0.19358995749499\n0.37791932059448,0.194288066122883\n0.378980891719745,0.195002204799637\n0.379830148619958,0.195541749527548\n0.380891719745223,0.196325752500587\n0.381953290870488,0.197067275844146\n0.382802547770701,0.197582909082505\n0.383864118895966,0.198308001625981\n0.384925690021231,0.199067692358249\n0.385987261146497,0.199769808498357\n0.386836518046709,0.200403129012104\n0.387898089171975,0.201072116384567\n0.38895966029724,0.201759003978257\n0.389808917197452,0.202224142562705\n0.390870488322718,0.202975016768099\n0.391932059447983,0.203663240199195\n0.392993630573248,0.204387264072746\n0.393842887473461,0.204925606546993\n0.394904458598726,0.20571161327614\n0.395966029723992,0.206420942938236\n0.396815286624204,0.207000696372039\n0.397876857749469,0.20769559899016\n0.398938428874735,0.208413077260427\n0.399787685774947,0.208974663305522\n0.400849256900212,0.209607315900566\n0.401910828025478,0.210434065670568\n0.402972399150743,0.211190817560545\n0.403821656050955,0.21176576197969\n0.404883227176221,0.212516502601343\n0.405944798301486,0.213222893421148\n0.406794055201699,0.21382856210061\n0.407855626326964,0.214577165382415\n0.408917197452229,0.215365443035151\n0.409978768577495,0.216037903584867\n0.410828025477707,0.216656129135936\n0.411889596602972,0.217323379919772\n0.412951167728238,0.217969390888868\n0.41380042462845,0.218544201724272\n0.414861995753716,0.219302022284172\n0.415923566878981,0.220002669003135\n0.416985138004246,0.220715071091263\n0.417834394904459,0.221349994609896\n0.418895966029724,0.222109284590942\n0.419957537154989,0.222842659326329\n0.420806794055202,0.223378330125765\n0.421868365180467,0.224140291781621\n0.422929936305733,0.224820500188286\n0.423991507430998,0.225638299847674\n0.42484076433121,0.226244770029579\n0.425902335456476,0.2270184870546\n0.426963906581741,0.227812108056956\n0.427813163481953,0.228437012795051\n0.428874734607219,0.229120427211489\n0.429936305732484,0.229861950555047\n0.430997876857749,0.230611889674257\n0.431847133757962,0.23118883784951\n0.432908704883227,0.231974710994917\n0.433970276008493,0.232685242910677\n0.434819532908705,0.233255779066386\n0.43588110403397,0.234007588357963\n0.436942675159236,0.234687796764629\n0.437791932059448,0.235320315775933\n0.438853503184713,0.236107257591263\n0.439915074309979,0.236862406476353\n0.440976645435244,0.237656561813672\n0.441825902335456,0.238254081884963\n0.442887473460722,0.238971292987749\n0.443949044585987,0.239803920442333\n0.4447983014862,0.240420943739738\n0.445859872611465,0.240936443394357\n0.44692144373673,0.241462496164477\n0.447983014861996,0.242262796353859\n0.448832271762208,0.242914685007536\n0.449893842887473,0.243727942819747\n0.450955414012739,0.244515018218818\n0.451804670912951,0.245176658485553\n0.452866242038217,0.245969210817985\n0.453927813163482,0.246715543176202\n0.454989384288747,0.247475901827172\n0.45583864118896,0.248058861270747\n0.456900212314225,0.248828971534774\n0.45796178343949,0.249608031909415\n0.458811040339703,0.250232135145067\n0.459872611464968,0.251052740063006\n0.460934182590234,0.251865730707736\n0.461995753715499,0.252730551843783\n0.462845010615711,0.253376562812878\n0.463906581740977,0.25416110012088\n0.464968152866242,0.254893139018862\n0.465817409766454,0.255580026612553\n0.46687898089172,0.256386738821479\n0.467940552016985,0.257144292213898\n0.468789808917197,0.257783490412227\n0.469851380042463,0.258497762672722\n0.470912951167728,0.259298997948287\n0.471974522292994,0.260075520231859\n0.472823779193206,0.260716187851334\n0.473885350318471,0.261512213361019\n0.474946921443737,0.262380240506838\n0.475796178343949,0.263023446217383\n0.476857749469214,0.26376470239346\n0.47791932059448,0.264622977926222\n0.478980891719745,0.265458544223098\n0.479830148619958,0.266104555192193\n0.480891719745223,0.266998631167411\n0.481953290870488,0.267788778992514\n0.482802547770701,0.268414618816793\n0.483864118895966,0.269242170089238\n0.484925690021231,0.270072526620234\n0.485987261146497,0.270872826809615\n0.486836518046709,0.271480899996407\n0.487898089171975,0.272336503854358\n0.48895966029724,0.273183291185437\n0.489808917197452,0.273767987217639\n0.490870488322718,0.27455760070778\n0.491932059447983,0.275394502842061\n0.492993630573248,0.276229534803975\n0.493842887473461,0.276934723370115\n0.494904458598726,0.277756263374237\n0.495966029723992,0.27864886992831\n0.496815286624204,0.279322666315431\n0.497876857749469,0.280134187539015\n0.498938428874735,0.280947044600004\n0.499787685774947,0.28163272994003\n0.500849256900212,0.2823614292445\n0.501910828025478,0.283169878042052\n0.502972399150743,0.28399008220877\n0.503821656050955,0.284653325480391\n0.504883227176221,0.28531630158453\n0.505944798301486,0.286139578177279\n0.506794055201699,0.286795607926913\n0.507855626326964,0.287637853410814\n0.508917197452229,0.288444164868518\n0.509978768577495,0.289273586313331\n0.510828025477707,0.289954061887477\n0.511889596602972,0.290821955449555\n0.512951167728238,0.291647102214671\n0.51380042462845,0.292286567580482\n0.514861995753715,0.29320709313632\n0.515923566878981,0.294118802165286\n0.516985138004246,0.294966925333769\n0.517834394904459,0.29566436604296\n0.518895966029724,0.296490447894259\n0.519957537154989,0.297322674597622\n0.520806794055202,0.297961071293509\n0.521868365180467,0.298809328045733\n0.522929936305732,0.299511310602101\n0.523991507430998,0.300265390817267\n0.52484076433121,0.300978060072875\n0.525902335456476,0.301838740112966\n0.526963906581741,0.302716251704362\n0.527813163481953,0.303404074384236\n0.528874734607219,0.304208782837054\n0.529936305732484,0.305101255807386\n0.530997876857749,0.306031532976282\n0.531847133757962,0.306694909831643\n0.532908704883227,0.307542632248905\n0.533970276008493,0.30843590672168\n0.534819532908705,0.30911998905682\n0.53588110403397,0.309984142274164\n0.536942675159236,0.310777763276521\n0.537791932059448,0.311527301644509\n0.538853503184713,0.312537996225191\n0.539915074309979,0.313462128542023\n0.540976645435244,0.314377978666944\n0.541825902335456,0.314997139304197\n0.542887473460722,0.315893753370485\n0.543949044585987,0.316655715026341\n0.5447983014862,0.317328442743537\n0.545859872611465,0.318289177005269\n0.54692144373673,0.319212774987139\n0.547983014861996,0.32011032413961\n0.548832271762208,0.320811104442314\n0.549893842887473,0.32164680432293\n0.550955414012739,0.32256492537144\n0.551804670912951,0.323359882211201\n0.552866242038217,0.324309929773692\n0.553927813163482,0.325215493950594\n0.554989384288747,0.326087261441147\n0.55583864118896,0.32688275261587\n0.556900212314225,0.327763737384519\n0.55796178343949,0.328556957635653\n0.558811040339703,0.329246784071635\n0.559872611464968,0.330180801585265\n0.560934182590234,0.331095315872781\n0.561995753715499,0.332077022781772\n0.562845010615711,0.332853812232824\n0.563906581740977,0.333762716003239\n0.564968152866242,0.33465505538983\n0.565817409766454,0.33535503419009\n0.56687898089172,0.33622573301072\n0.567940552016985,0.337206104082306\n0.568789808917197,0.33799130930901\n0.569851380042463,0.338927597746229\n0.570912951167728,0.339868428030624\n0.571974522292994,0.340744870952095\n0.572823779193206,0.341517385723451\n0.573885350318471,0.342494016450303\n0.574946921443737,0.343446334936383\n0.575796178343949,0.344201750988954\n0.576857749469214,0.345199621530546\n0.57791932059448,0.346072992025986\n0.578980891719745,0.347149276223256\n0.579830148619958,0.347946904737828\n0.580891719745223,0.348649421629158\n0.581953290870488,0.349640212232504\n0.582802547770701,0.350417135267296\n0.583864118895966,0.351387621142085\n0.584925690021231,0.35233619928343\n0.585987261146497,0.353332733987618\n0.586836518046709,0.353979012124194\n0.587898089171975,0.354947494242875\n0.58895966029724,0.355928800400644\n0.589808917197452,0.35670906302895\n0.590870488322718,0.357720692695815\n0.591932059447983,0.358687972560831\n0.592993630573248,0.359656989014474\n0.593842887473461,0.360388627161235\n0.594904458598726,0.361353368935181\n0.595966029723991,0.362276833333311\n0.596815286624204,0.363016620088243\n0.597876857749469,0.363987773881734\n0.598938428874735,0.36493394751575\n0.599787685774947,0.365656902719378\n0.600849256900212,0.366622445995767\n0.601910828025478,0.3674994232522\n0.602972399150743,0.368522541120749\n0.603821656050955,0.369265667469193\n0.604883227176221,0.370288785337742\n0.605944798301486,0.371193681595941\n0.606794055201698,0.371953505911949\n0.607855626326964,0.37285092148068\n0.608917197452229,0.373845318845019\n0.609978768577495,0.374707735473737\n0.610828025477707,0.37547437256051\n0.611889596602972,0.376412264002615\n0.612951167728238,0.377375937106638\n0.61380042462845,0.378196942775798\n0.614861995753715,0.37919200805884\n0.615923566878981,0.380097438652001\n0.616985138004246,0.381153418120715\n0.617834394904459,0.382019575094167\n0.618895966029724,0.383053513245696\n0.619957537154989,0.384044571016523\n0.620806794055202,0.384900442041956\n0.621868365180467,0.38586972566308\n0.622929936305732,0.386733611712943\n0.623991507430998,0.387713314865826\n0.62484076433121,0.388523500252006\n0.625902335456476,0.389592170176067\n0.626963906581741,0.390644810051268\n0.627813163481953,0.391469956816384\n0.628874734607219,0.392875257766525\n0.629936305732484,0.393926828971802\n0.630997876857749,0.394905997789723\n0.631847133757962,0.395671432622833\n0.632908704883227,0.396768823051103\n0.633970276008493,0.397699901722442\n0.634819532908705,0.398496060815868\n0.63588110403397,0.399487920089138\n0.636942675159236,0.400514911886161\n0.637791932059448,0.401305727629967\n0.638853503184713,0.402312815449654\n0.639915074309979,0.403356772381722\n0.640976645435244,0.404357047430644\n0.641825902335456,0.405165896979418\n0.642887473460722,0.406100181660529\n0.643949044585987,0.407128910046179\n0.6447983014862,0.407991727426118\n0.645859872611465,0.408998280910843\n0.64692144373673,0.409954339741657\n0.647983014861996,0.411006846033117\n0.648832271762208,0.411830256209607\n0.649893842887473,0.412863392858693\n0.650955414012739,0.413834413068444\n0.651804670912951,0.414703508884187\n0.652866242038217,0.415705119770514\n0.653927813163482,0.416756156640828\n0.654989384288747,0.417769923647541\n0.65583864118896,0.418633809697405\n0.656900212314225,0.41963755792358\n0.65796178343949,0.420714777207034\n0.658811040339703,0.421563033959258\n0.659872611464968,0.422577335300933\n0.660934182590234,0.423611006284981\n0.661995753715499,0.424693435334315\n0.662845010615711,0.425479308479722\n0.663906581740977,0.426915868025142\n0.664968152866242,0.427994556729742\n0.665817409766454,0.428815695982643\n0.66687898089172,0.429823986055995\n0.667940552016985,0.430867809404322\n0.668789808917197,0.431712592979293\n0.669851380042463,0.432778858396025\n0.670912951167728,0.433812529380074\n0.671974522292994,0.434855551225958\n0.672823779193206,0.43568216741222\n0.673885350318471,0.43675043658506\n0.674946921443737,0.437778630635748\n0.675796178343949,0.438625284383086\n0.676857749469214,0.440017494126657\n0.67791932059448,0.441164176955175\n0.678980891719745,0.442249010511838\n0.679830148619957,0.443157780698512\n0.680891719745223,0.444330111605208\n0.681953290870488,0.4454595621312\n0.682802547770701,0.446299002356551\n0.683864118895966,0.447409617575132\n0.684925690021231,0.448515557362795\n0.685987261146497,0.449520374258894\n0.686836518046709,0.450465612806727\n0.687898089171974,0.45163219961258\n0.68895966029724,0.452544576560248\n0.689808917197452,0.453391898226288\n0.690870488322718,0.454451885207217\n0.691932059447983,0.455553416731444\n0.692993630573248,0.456630636014898\n0.693842887473461,0.457480228604527\n0.694904458598726,0.458596320756469\n0.695966029723991,0.459845195146475\n0.696815286624204,0.460786559765833\n0.697876857749469,0.461878072509522\n0.698938428874735,0.462978401780083\n0.7,0.46414044673876\n0.700849256900212,0.465020496421225\n0.701910828025478,0.466136989324388\n0.702972399150743,0.467196976305317\n0.703821656050955,0.468249215429296\n0.704883227176221,0.469281283408459\n0.705944798301486,0.470393100880704\n0.706794055201698,0.471379616804354\n0.707855626326964,0.472501853808359\n0.708917197452229,0.47371252324858\n0.709978768577495,0.474972084337827\n0.710828025477707,0.475856275116248\n0.711889596602972,0.476918132269544\n0.712951167728238,0.478158323716417\n0.71380042462845,0.479053334777819\n0.714861995753715,0.480129218223868\n0.715923566878981,0.481246379045734\n0.716985138004246,0.482694426792837\n0.717834394904459,0.483636192163417\n0.718895966029724,0.484810259658739\n0.719957537154989,0.485931027241599\n0.720806794055202,0.486951740602818\n0.721868365180467,0.488055543050633\n0.722929936305732,0.489256594461538\n0.723991507430998,0.490361198411796\n0.72484076433121,0.491308440715736\n0.725902335456476,0.49252752593161\n0.726963906581741,0.493715218968464\n0.727813163481953,0.494672212885461\n0.728874734607219,0.495776282500757\n0.729936305732484,0.496872604259104\n0.730997876857749,0.49813710794675\n0.731847133757962,0.499100914634513\n0.732908704883227,0.500274180627392\n0.733970276008493,0.501421531374613\n0.734819532908705,0.502314271512426\n0.73588110403397,0.503531753723413\n0.736942675159236,0.504695535270716\n0.737791932059448,0.505861721325348\n0.738853503184713,0.507026037207614\n0.739915074309979,0.507966199573307\n0.740976645435244,0.509176468262307\n0.741825902335456,0.510070944988746\n0.742887473460722,0.511481589288508\n0.743949044585987,0.512647374591918\n0.7447983014862,0.51360557076258\n0.745859872611465,0.514781909181491\n0.74692144373673,0.516109731562138\n0.747983014861996,0.517385990618948\n0.748832271762208,0.518698851620658\n0.749893842887473,0.519997151994652\n0.750955414012739,0.521212496865792\n0.751804670912951,0.522149319637972\n0.752866242038217,0.523294265877864\n0.753927813163482,0.524493313532661\n0.754989384288747,0.525793083327801\n0.75583864118896,0.526746737651285\n0.756900212314225,0.52804784328383\n0.75796178343949,0.529271871098102\n0.758811040339703,0.530263730371372\n0.759872611464968,0.531422302152796\n0.760934182590234,0.53267852364853\n0.761995753715499,0.533868220441492\n0.762845010615711,0.534865155896901\n0.763906581740977,0.536253625295738\n0.764968152866242,0.537498492173529\n0.765817409766454,0.538815360687454\n0.76687898089172,0.540422773836983\n0.767940552016985,0.541659625690344\n0.768789808917198,0.542550896407011\n0.769851380042463,0.543783607164417\n0.770912951167728,0.54498158614929\n0.771974522292994,0.546180099469125\n0.772823779193206,0.547201213581566\n0.773885350318471,0.548363792875204\n0.774946921443737,0.549588221440698\n0.775796178343949,0.550669448236367\n0.776857749469214,0.551988854841362\n0.77791932059448,0.553215821497925\n0.778980891719745,0.554507442684894\n0.779830148619958,0.555601894270874\n0.780891719745223,0.556956566983363\n0.781953290870488,0.558201166693673\n0.782802547770701,0.559298289954463\n0.783864118895966,0.560533004467975\n0.784925690021231,0.561837850445255\n0.785987261146497,0.563080713566938\n0.786836518046709,0.564090606645177\n0.787898089171975,0.565360052931221\n0.78895966029724,0.56667785653133\n0.789808917197452,0.567721813463398\n0.790870488322718,0.569167189535691\n0.791932059447983,0.570560067197965\n0.792993630573248,0.571888290329833\n0.793842887473461,0.57302936264125\n0.794904458598726,0.574413156609169\n0.795966029723992,0.576017898083888\n0.796815286624204,0.57719971343616\n0.797876857749469,0.578660451638612\n0.798938428874735,0.580009246666518\n0.799787685774947,0.581194668779783\n0.800849256900212,0.582541727219063\n0.801910828025478,0.583879568380248\n0.802972399150743,0.585335631151782\n0.803821656050955,0.586468955606249\n0.804883227176221,0.587720234503585\n0.805944798301486,0.58906074733958\n0.806794055201699,0.590213975771383\n0.807855626326964,0.59166830195429\n0.808917197452229,0.593103792829786\n0.809978768577495,0.594521116316573\n0.810828025477707,0.595676615671964\n0.811889596602972,0.597038769073921\n0.812951167728238,0.598379549077397\n0.81380042462845,0.59958019973708\n0.814861995753716,0.600939815047968\n0.815923566878981,0.602330154619172\n0.816985138004246,0.603661316593331\n0.817834394904459,0.604866642683932\n0.818895966029724,0.606276619064991\n0.819957537154989,0.607980746842648\n0.820806794055202,0.609179527329964\n0.821868365180467,0.610658967256087\n0.822929936305733,0.612145620704197\n0.823991507430998,0.613644296688953\n0.82484076433121,0.614500969216829\n0.825902335456476,0.615968119438825\n0.826963906581741,0.617394660203707\n0.827813163481953,0.618601054964232\n0.828874734607219,0.620115226662887\n0.829936305732484,0.621639817893302\n0.83099787685775,0.623145974567526\n0.831847133757962,0.624320843565291\n0.832908704883227,0.625462717379151\n0.833970276008493,0.626947767822375\n0.834819532908705,0.62810674035502\n0.83588110403397,0.629604481253593\n0.836942675159236,0.631195330019299\n0.837791932059448,0.632143908160644\n0.838853503184713,0.633667564304875\n0.839915074309979,0.635228356728967\n0.840976645435244,0.636783672219698\n0.841825902335457,0.638068480635901\n0.842887473460722,0.639653318133285\n0.843949044585987,0.641269280642206\n0.8447983014862,0.642433730108212\n0.845859872611465,0.644029922223539\n0.84692144373673,0.645408372841838\n0.847983014861996,0.646984660979829\n0.848832271762208,0.648305136254747\n0.849893842887473,0.649901061202593\n0.850955414012739,0.651335082656943\n0.851804670912951,0.652584491381912\n0.852866242038217,0.654044027330699\n0.853927813163482,0.65565705099733\n0.854989384288747,0.657194332683092\n0.85583864118896,0.658418494081105\n0.856900212314225,0.659823127112543\n0.85796178343949,0.661384053120376\n0.858811040339703,0.662629988668091\n0.859872611464968,0.664201067040202\n0.860934182590234,0.665956891725436\n0.861995753715499,0.667504459359218\n0.862845010615711,0.668822396543067\n0.863906581740977,0.670250673896575\n0.864968152866242,0.671945317228656\n0.865817409766454,0.673277147121518\n0.86687898089172,0.674850763584698\n0.867940552016985,0.676509740058064\n0.868789808917197,0.677842505037109\n0.869851380042463,0.679554915006677\n0.870912951167728,0.681209750384087\n0.871974522292994,0.682794721465211\n0.872823779193206,0.684159947293199\n0.873885350318471,0.685780585233039\n0.874946921443737,0.687488052604208\n0.875796178343949,0.688819080994627\n0.876857749469214,0.690547521013056\n0.87791932059448,0.692221458865358\n0.878980891719745,0.693956177319591\n0.879830148619958,0.695207856968149\n0.880891719745223,0.696698117177253\n0.881953290870488,0.698293374206396\n0.882802547770701,0.699673561413321\n0.883864118895966,0.701441675802681\n0.884925690021231,0.703032390984647\n0.885987261146497,0.704678677002664\n0.886836518046709,0.706120579897704\n0.887898089171975,0.707878541922786\n0.88895966029724,0.710043667188935\n0.889808917197452,0.711394332389208\n0.890870488322718,0.713197044967359\n0.891932059447983,0.714757970975192\n0.892993630573248,0.716639230792762\n0.893842887473461,0.718010067137852\n0.894904458598726,0.719882644012289\n0.895966029723992,0.721639003032485\n0.896815286624204,0.723474577210801\n0.897876857749469,0.725237615418022\n0.898938428874735,0.726965521101489\n0.899787685774947,0.728373894477662\n0.900849256900212,0.730433889340031\n0.901910828025478,0.732292573505455\n0.902972399150743,0.734351232530419\n0.903821656050955,0.735830672456542\n0.904883227176221,0.737587432227959\n0.905944798301486,0.739502889483099\n0.906794055201699,0.740964028436772\n0.907855626326964,0.742877214768324\n0.908917197452229,0.744750993896425\n0.909978768577495,0.748294703364614\n0.910828025477707,0.74974074735561\n0.911889596602972,0.751630289365091\n0.912951167728238,0.753498858727313\n0.91380042462845,0.755027190302462\n0.914861995753716,0.758811350503564\n0.915923566878981,0.760800946734686\n0.916985138004246,0.762697835849895\n0.917834394904459,0.764361354170438\n0.918895966029724,0.766488140903061\n0.919957537154989,0.768429379820119\n0.920806794055202,0.770320658418227\n0.921868365180467,0.772256019650703\n0.922929936305733,0.774277408812065\n0.923991507430998,0.776300534562054\n0.92484076433121,0.777919168745786\n0.925902335456476,0.780000537006637\n0.926963906581741,0.782180890819204\n0.927813163481953,0.783715901581378\n0.928874734607219,0.78595289489992\n0.929936305732484,0.788115348491259\n0.930997876857749,0.790120306852539\n0.931847133757962,0.79190725654931\n0.932908704883227,0.794041791138883\n0.933970276008493,0.796284795725748\n0.934819532908705,0.797956329070721\n0.93588110403397,0.800054662466616\n0.936942675159236,0.802246905232088\n0.937791932059448,0.804108928991025\n0.938853503184713,0.806234780637464\n0.939915074309979,0.808354621015581\n0.940976645435244,0.810631823040016\n0.941825902335456,0.812570256698523\n0.942887473460722,0.814811925447983\n0.943949044585987,0.817082047534171\n0.9447983014862,0.818959032672045\n0.945859872611465,0.821369684853247\n0.94692144373673,0.823693507603119\n0.947983014861996,0.827240155913599\n0.948832271762208,0.829098973662763\n0.949893842887473,0.831428941264699\n0.950955414012739,0.833527541828075\n0.951804670912951,0.835831994935573\n0.952866242038217,0.838228353656541\n0.953927813163482,0.840727304690219\n0.954989384288747,0.843423291741145\n0.95583864118896,0.845581604236528\n0.956900212314225,0.848253279046682\n0.95796178343949,0.850851349215817\n0.958811040339703,0.853096223975049\n0.959872611464968,0.855640459896759\n0.960934182590234,0.858628861755656\n0.961995753715499,0.860928773015978\n0.962845010615711,0.86310805815862\n0.963906581740977,0.865887935798586\n0.964968152866242,0.868631478661133\n0.965817409766454,0.870723934372446\n0.96687898089172,0.873607472995045\n0.967940552016985,0.876623793855709\n0.968789808917197,0.878862390179137\n0.969851380042463,0.88157854837488\n0.970912951167728,0.884733662741932\n0.971974522292994,0.888433130851552\n0.972823779193206,0.890756953601424\n0.973885350318471,0.893536029738947\n0.974946921443737,0.89710725745768\n0.975796178343949,0.899403829124488\n0.976857749469214,0.90247478573502\n0.97791932059448,0.906567123625473\n0.978980891719745,0.909777942412317\n0.979830148619958,0.912343818899989\n0.980891719745223,0.917358552518648\n0.981953290870488,0.920959168660293\n0.982802547770701,0.923727958849796\n0.983864118895966,0.929349429817841\n0.984925690021231,0.932980235884841\n0.985987261146497,0.93704772719956\n0.986836518046709,0.941387996512396\n0.987898089171975,0.944727055690126\n0.98895966029724,0.95139007908291\n0.989808917197452,0.954969455409814\n0.990870488322718,0.959177610403288\n0.991932059447983,0.96326941395878\n0.992993630573248,0.967835172625592\n0.993842887473461,0.970943666267207\n0.994904458598726,0.974964536856489\n0.995966029723992,0.979498636176801\n0.996815286624204,0.983076275915079\n0.997876857749469,0.988097555137023\n0.998938428874735,0.993903906001931\n0.999787685774947,0.99874765243274"
    },
    {
      "name": "scale/x",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n-0.0499893842887474\n1.04977707006369"
    },
    {
      "name": "scale/y",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n-0.0499787685774947\n1.04955414012739"
    }
  ],
  "scales": [
    {
      "name": "x",
      "domain": {
        "data": "scale/x",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "width"
    },
    {
      "name": "y",
      "domain": {
        "data": "scale/y",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "height"
    }
  ],
  "marks": [
    {
      "type": "line",
      "properties": {
        "update": {
          "x": {
            "scale": "x",
            "field": "data.ValidationWeight"
          },
          "y": {
            "scale": "y",
            "field": "data.ValidationLoss"
          },
          "stroke": {
            "value": "blue"
          }
        },
        "ggvis": {
          "data": {
            "value": ".0"
          }
        }
      },
      "from": {
        "data": ".0"
      }
    },
    {
      "type": "line",
      "properties": {
        "update": {
          "x": {
            "scale": "x",
            "field": "data.ValidationWeight"
          },
          "y": {
            "scale": "y",
            "field": "data.ValidationWeight"
          },
          "stroke": {
            "value": "black"
          }
        },
        "ggvis": {
          "data": {
            "value": "ModelComparisonData1"
          }
        }
      },
      "from": {
        "data": "ModelComparisonData1"
      }
    },
    {
      "type": "line",
      "properties": {
        "update": {
          "x": {
            "scale": "x",
            "field": "data.TrainWeight"
          },
          "y": {
            "scale": "y",
            "field": "data.TrainLoss"
          },
          "stroke": {
            "value": "red"
          }
        },
        "ggvis": {
          "data": {
            "value": "ModelComparisonData2"
          }
        }
      },
      "from": {
        "data": "ModelComparisonData2"
      }
    }
  ],
  "legends": [],
  "axes": [
    {
      "type": "x",
      "scale": "x",
      "orient": "bottom",
      "layer": "back",
      "grid": true,
      "title": "TrainWeight"
    },
    {
      "type": "y",
      "scale": "y",
      "orient": "left",
      "layer": "back",
      "grid": true,
      "title": "TrainLoss"
    }
  ],
  "padding": null,
  "ggvis_opts": {
    "keep_aspect": false,
    "resizable": true,
    "padding": {},
    "duration": 250,
    "renderer": "svg",
    "hover_duration": 0,
    "width": 576,
    "height": 384
  },
  "handlers": null
};
ggvis.getPlot("plot_id524633314").parseSpec(plot_id524633314_spec);
</script><!--/html_preserve-->

###6. Conclusion
Based on the analysis, we see a significant relationship between weather related attributes and home pices. We can make some preliminary conclusions based on the analysis: \newline

1. Home price is higher when precipitation is lower, which is intuitive, as high summer precipitation may cause flood. People prefer cooler weather in summer, which drives housing demand, and increase the home price, which is consistent with our results that home price is higher when summer tempurature is lower. Based on the analysis, winter tempurature does not have as big of an impact to the home price as summer tempurater. \newline

2. Geographic location has a big impace on home price, which is also intuitive. This project used 'State' to model the impact of geographic location on home price. For future improvement, regional factors can be considered as the U.S. is typically grouped into climate regions based on similar weather patterns.  \newline

3. By comparing different models, gradient boosting model is our chosen model for this project, as it performed better than liner regression with better gains chart, and higher KS score. Gradient boosing was able to catch the non-linear relationship between our independent variables and the dependent variable, such as precipitation. Also gradient boosting does not require as much feature engineering as linear regression, which is ideal for a time-constrained project. Binning and smoothing can help to address some non-linear relationships in linear regression. However, the project only looked into weather related variables for non-linear relationship transformation, which can be improved in future study.

###7. Next Step
1. Segmentation based on geographic regions

2. Linear regression binning and smoothing

3. Missing value imputation with multiple imputation

4. Test the results on Holdout dataset.
 



