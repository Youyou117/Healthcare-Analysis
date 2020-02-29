
merge(cchs11, cchs21,sort = TRUE)
total <- rbind(cchs11, cchs21)
intersect(intersect(colnames(cchs11),colnames(cchs21)),colnames(cchs31))
levels(cchs11$GEOADPMF)
attributes(cchs11)$variable.labels
attributes(cchs31)$variable.labels

#sample code for filter or structure
filter(cchs11.sample,DHHAGAGE!=c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") )
table(sample$SDCAGRAC)

#extract 23 variables from cchs11
cchs11.sample <- cchs11[,c('CCCA_121','CCCA_051','CCCA_05A','DHHAGAGE','DHHA_SEX',
          'DHHAGMS','SDCAGRAC','SDCAFIMM','SDCAGRES','EDUADR04','INCAGHH','HWTAGBMI','PACADPAI','TWDA_5',
          'SMKADSTY','ALCADTYP','CCCA_071','CCCA_101','CCCA_91B','FVCADTOT','GENA_07','GEOAGPRV','WTSAM')]

#filter out invalid responses
library(dplyr)
cchs11.sample <- filter(cchs11.sample,
                   !CCCA_121 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                   !CCCA_05A %in% c("RHEUMATOID ARTH","OTHER"  ) &
                   DHHAGAGE %in% c("20 TO 24 YEARS","25 TO 29 YEARS" ,"30 TO 34 YEARS","35 TO 39 YEARS","40 TO 44 YEARS","45 TO 49 YEARS", 
                               "50 TO 54 YEARS","55 TO 59 YEARS","60 TO 64 YEARS") &
                   !DHHAGMS %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                   !SDCAGRAC %in% 'NOT STATED' & 
                   !EDUADR04 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                   !INCAGHH %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                   !HWTAGBMI %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                   !PACADPAI %in% c('NOT STATED','NOT APPLICABLE') &
                   !TWDA_5 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                   !SMKADSTY %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                   !ALCADTYP %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                   !CCCA_071 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                   !CCCA_101 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                   !CCCA_91B %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                   !FVCADTOT %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                   !GENA_07 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                   !GEOAGPRV %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED")
                   )




#extract 21 variables from cchs21
cchs21.sample <- cchs21[,c('CCCC_121','CCCC_051','CCCC_05A','DHHCGAGE','DHHC_SEX',
                           'DHHCGMS','SDCCGRAC','SDCCFIMM','SDCCGRES','EDUCDR04','INCCGHH','HWTCGBMI','PACCDPAI','HCUC_1AA',
                           'SMKCDSTY','ALCCDTYP','CCCC_071','CCCC_101','CCCC_91B','FVCCDTOT','GENC_07','GEOCGPRV','WTSC_M')]

#filter out invalid responses
library(dplyr)
cchs21.sample <- filter(cchs21.sample,
                        !CCCC_121 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !CCCC_05A %in% c("RHEUMATOID ARTH","OTHER" ) &
                          DHHCGAGE %in% c("20 TO 24 YEARS","25 TO 29 YEARS" ,"30 TO 34 YEARS","35 TO 39 YEARS","40 TO 44 YEARS","45 TO 49 YEARS", 
                                          "50 TO 54 YEARS","55 TO 59 YEARS","60 TO 64 YEARS") &
                          !DHHCGMS %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !SDCCGRAC %in% 'NOT STATED' & 
                          !EDUCDR04 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !INCCGHH %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !HWTCGBMI %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !PACCDPAI %in% c('NOT STATED','NOT APPLICABLE') &
                          !HCUC_1AA %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !SMKCDSTY %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !ALCCDTYP %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !CCCC_071 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !CCCC_101 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !CCCC_91B %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !FVCCDTOT %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !GENC_07 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !GEOCGPRV %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED")
)
#extract 23 variables from cchs31
cchs31.sample<-cchs31[,c('CCCE_121','CCCE_051','CCCE_05A','DHHEGAGE','DHHE_SEX','DHHEGMS','SDCEGCGT','SDCEFIMM',
                         'SDCEGRES','EDUEDR04','INCEGHH','HWTEGBMI','PACEDPAI','HCUE_1AA','SMKEDSTY','ALCEDTYP',
                         'CCCE_071','CCCE_101','CCCE_91F','FVCEDTOT','GENE_07','GEOEGPRV','WTSE_M')]

#filter
library(dplyr)
cchs31.sample <- filter(cchs31.sample,
                        !CCCE_121 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !CCCE_05A %in% c("RHEUMATOID ARTH","OTHER" ) &
                          DHHEGAGE %in% c("20 TO 24 YEARS","25 TO 29 YEARS" ,"30 TO 34 YEARS","35 TO 39 YEARS","40 TO 44 YEARS","45 TO 49 YEARS",
                                          "50 TO 54 YEARS","55 TO 59 YEARS","60 TO 64 YEARS") &
                          !DHHEGMS %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !SDCEGCGT %in% 'NOT STATED' &
                          !EDUEDR04 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !INCEGHH %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !HWTEGBMI %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !PACEDPAI %in% c('NOT STATED','NOT APPLICABLE') &
                          !HCUE_1AA %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !SMKEDSTY %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !ALCEDTYP %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !CCCE_071 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !CCCE_101 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !CCCE_91F %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !FVCEDTOT %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !GENE_07 %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED") &
                          !GEOEGPRV %in% c("NOT APPLICABLE" ,"DON'T KNOW" , "REFUSAL","NOT STATED")
)


colnames(cchs11.sample)<-c('Heart_Disease','Arthritis_or_Rheumatism','Kind_of_Arhritis','Age','Sex',
  'Marital_Status','Racial_Origin','Immigrant_Status','Time_in_Canada','Highest_Education',
  'Total_Household_Income','BMI','Physical_Activity_Index','Has_Regular_Doctor',
  'Type_of_Smoker','Type_of_Drinker','Has_high_blood_pressure','Has_Diabetes','Has_emphysema/COPD',
  'Daily_consumption','Self_perceived_stress','Province','Sampling_weight')
colnames(cchs21.sample)<-c('Heart_Disease','Arthritis_or_Rheumatism','Kind_of_Arhritis','Age','Sex',
                           'Marital_Status','Racial_Origin','Immigrant_Status','Time_in_Canada','Highest_Education',
                           'Total_Household_Income','BMI','Physical_Activity_Index','Has_Regular_Doctor',
                           'Type_of_Smoker','Type_of_Drinker','Has_high_blood_pressure','Has_Diabetes','Has_emphysema/COPD',
                           'Daily_consumption','Self_perceived_stress','Province','Sampling_weight')
colnames(cchs31.sample)<-c('Heart_Disease','Arthritis_or_Rheumatism','Kind_of_Arhritis','Age','Sex',
                           'Marital_Status','Racial_Origin','Immigrant_Status','Time_in_Canada','Highest_Education',
                           'Total_Household_Income','BMI','Physical_Activity_Index','Has_Regular_Doctor',
                           'Type_of_Smoker','Type_of_Drinker','Has_high_blood_pressure','Has_Diabetes','Has_emphysema/COPD',
                           'Daily_consumption','Self_perceived_stress','Province','Sampling_weight')

cchs11.sample$Year<-2001
cchs21.sample$Year<-2003
cchs31.sample$Year<-2005
new <- rbind(cchs11.sample,cchs21.sample,cchs31.sample)



table(new$Total_Household_Income)

table(cchs11$GENA_07)
table(cchs21$GENC_07)
table(cchs31$GENE_07)

#read data
df2 <- read.csv('/Users/reggieyang/Google 云端硬盘/Healthcare Competition/Dataset CSV/healcare_cleaned v2.csv')
levels(df2$Total_Household_Income)

table(df2$Total_Household_Income)
#change data to ordinal data
table(df2$Age)
levels(df2$Province)
levels(df2$Self_perceived_stress)
df2$Age<-factor(df2$Age, order = TRUE, 
                                  levels = c("30 TO 34 YEARS",'35 TO 39 YEARS',
                                             '40 TO 44 YEARS',"45 TO 49 YEARS", "50 TO 54 YEARS",
                                             '55 TO 59 YEARS','60 TO 64 YEARS'))
df2$Self_perceived_stress<-factor(df2$Self_perceived_stress, order = TRUE, 
       levels = c("NOT AT ALL",'A BIT','NOT VERY',"QUITE A BIT", "EXTREMELY"))
df2$Total_Household_Income <- factor(df2$Total_Household_Income, order = TRUE, 
       levels = c("NO OR <$15,000",'$15,000-$29,999','$30,000-$49,999',"$50,000-$79,999", "$80,000 OR MORE"))
df2$Physical_Activity_Index <- factor(df2$Physical_Activity_Index, order = TRUE, 
      levels = c("INACTIVE",'MODERATE','ACTIVE'))

table(df2$Total_Household_Income)

df$Kind_of_Arhritis <- NULL
#merge Imigrant_status and time_in_canada
df$Immigrant_Status <- NULL



levels(df$Time_in_Canada) <- c(levels(df$Time_in_Canada),'NOT IMMIGRANT')
df$Time_in_Canada[df$Time_in_Canada=='NOT APPLICABLE'] <- 'NOT IMMIGRANT'
df$Time_in_Canada[df$Time_in_Canada=="NOT STATED"] <- 'NOT IMMIGRANT'
levels(df$Time_in_Canada) <- c(levels(df$Time_in_Canada),'IMMIGRATED MORE THAN 10 YEARS AGO')
df$Time_in_Canada[df$Time_in_Canada=="10 OR MORE YEARS"] <- 'IMMIGRATED MORE THAN 10 YEARS AGO'
df$Time_in_Canada[df$Time_in_Canada=="10 YEARS OR MORE"] <- 'IMMIGRATED MORE THAN 10 YEARS AGO'
levels(df$Time_in_Canada) <- c(levels(df$Time_in_Canada),'RECENT IMMIGRANT')
df$Time_in_Canada[df$Time_in_Canada=="0 TO 9 YEARS"] <- 'RECENT IMMIGRANT'

#export csv
#write.csv(df2,'healcare_cleaned v3.csv')




