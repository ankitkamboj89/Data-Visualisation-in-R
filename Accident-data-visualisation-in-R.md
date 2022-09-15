Visualisation and analysis of Accidents related data in mines
================
Ankit Kamboj
14/09/2022

------------------------------------------------------------------------

**Introduction**

    The data set analysed is titled "US Accident Injury Dataset". 

    Data Contains Information about the accidents in the mines in USA.

**About the Data**

    Unique vs Duplicate variables
    Total Unique attributs: 39 
    Duplicate (Explanation) attributes: 18

    Categorical vs Continuous attributes
    Total Categorical attributes: 53 
    Total continous attributes: 4

    Total attributes: 57

    Total observations: 202815 (each indicates an accident event)

**Loading Data and Libraries**

    Libraries used:
    library(tidyverse)

    Data file Used:
    us_data.csv (138 mb file)

``` r
d<- read.csv("~/Subjects/S2_2021/Computational Data Analysis/Project 1/Data/us_data.csv")
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.3     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

**“NA” entries in data**

``` r
#NA entries
apply(is.na(d), 2, sum)
```

    ##             MINE_ID       CONTROLLER_ID     CONTROLLER_NAME         OPERATOR_ID 
    ##                   0                   0                   0                   0 
    ##       OPERATOR_NAME       CONTRACTOR_ID         DOCUMENT_NO          SUBUNIT_CD 
    ##                   0                   0                   0                   0 
    ##             SUBUNIT         ACCIDENT_DT              CAL_YR             CAL_QTR 
    ##                   0                   0                   0                   0 
    ##           FISCAL_YR          FISCAL_QTR       ACCIDENT_TIME    DEGREE_INJURY_CD 
    ##                   0                   0                   0                   0 
    ##       DEGREE_INJURY       FIPS_STATE_CD      UG_LOCATION_CD         UG_LOCATION 
    ##                   0                   0                   0                   0 
    ## UG_MINING_METHOD_CD    UG_MINING_METHOD     MINING_EQUIP_CD        MINING_EQUIP 
    ##                   0                   0                   0                   0 
    ##        EQUIP_MFR_CD      EQUIP_MFR_NAME      EQUIP_MODEL_NO    SHIFT_BEGIN_TIME 
    ##                   0                   0                  48                 991 
    ##   CLASSIFICATION_CD      CLASSIFICATION    ACCIDENT_TYPE_CD       ACCIDENT_TYPE 
    ##                   0                   0                   0                   0 
    ##         NO_INJURIES           TOT_EXPER          MINE_EXPER           JOB_EXPER 
    ##                   0               37400               34325               33746 
    ##       OCCUPATION_CD          OCCUPATION         ACTIVITY_CD            ACTIVITY 
    ##                   0                   0                   0                   0 
    ##    INJURY_SOURCE_CD       INJURY_SOURCE    NATURE_INJURY_CD       NATURE_INJURY 
    ##                   0                   0                   0                   0 
    ##    INJ_BODY_PART_CD       INJ_BODY_PART     SCHEDULE_CHARGE       DAYS_RESTRICT 
    ##                   0                   0               65006               54855 
    ##           DAYS_LOST          TRANS_TERM   RETURN_TO_WORK_DT     IMMED_NOTIFY_CD 
    ##               39676                   0                   0                   0 
    ##        IMMED_NOTIFY     INVEST_BEGIN_DT           NARRATIVE       CLOSED_DOC_NO 
    ##                   0                   0                   0              116620 
    ##      COAL_METAL_IND 
    ##                   0

<br> **Number of accidents in a years from 2000 to 2015**

``` r
##Data transformation - converting to factor
d$SUBUNIT_CD <- factor(d$SUBUNIT_CD) 

##plot on bar graph
plot(ggplot(data=d, xlab = "Calender Year") +
  geom_bar(aes(x=CAL_YR, fill=SUBUNIT_CD)) +
  theme(text = element_text(size= 10)) +
    labs(title = "Number of Accidents")) 
```

![](Accident-data-visualisation-in-R_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

    It can be observed that the accident number has constantly fallen over the years. The number of accidents drastically falls in 2015.

<br> **Location within a mine where the accident occurred**

``` r
##Plot of the histogram 
ggplot(d,aes(x=CAL_YR, fill=SUBUNIT_CD)) +
   geom_histogram (binwidth = 1, position = "fill") +
  theme(text = element_text(size= 10)) +
    labs(title = "Location of accidents") 
```

![](Accident-data-visualisation-in-R_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

    Legend: Accident type (SUBUNIT_CD) 1 is Underground, Accident 3 is Strip and Accident 30 is Mill operation/preparation Plant.

    It can be observed that the accident at location 1,3 and 30 are most prominent in across all the years.

<br> **Day of the week on which accidents had occurred**

``` r
#data transformation - date to day of the week
day_of_week<-(factor(weekdays(as.Date(d$ACCIDENT_DT))))

#Plot of the bar graph
plot(ggplot(d) +
  geom_bar(aes(x=day_of_week)) +
  theme(text = element_text(size= 10)) +
    labs(title = "Accidents on days of the week")) 
```

![](Accident-data-visualisation-in-R_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

    The number of accidents are uniformly spread over the days of week

<br> **At what time of day does accident are frequent**

``` r
#data cleaning - removing "NA" values from ACCIDENT_TIME column

d_NAr_Time <- d[which(!is.na(d$ACCIDENT_TIME)),]

#data transformation - converting continuous variable time to Categorical variable
d_NAr_Time <- within(d_NAr_Time, {
  ACCIDENT_TIME_CAT <- NA
  ACCIDENT_TIME_CAT [ACCIDENT_TIME >= 000 & ACCIDENT_TIME < 600] <- "Night"
  ACCIDENT_TIME_CAT [ACCIDENT_TIME >= 600 & ACCIDENT_TIME < 1200] <- "Morning"
  ACCIDENT_TIME_CAT [ACCIDENT_TIME >= 1200 & ACCIDENT_TIME < 1800] <- "Afternoon"
  ACCIDENT_TIME_CAT [ACCIDENT_TIME >= 1800 & ACCIDENT_TIME <= 2400] <- "Evening"
  ACCIDENT_TIME_CAT [(ACCIDENT_TIME == 9991) | (ACCIDENT_TIME == 9992) | (ACCIDENT_TIME == 9993) | (ACCIDENT_TIME == 9994) | (ACCIDENT_TIME == 9995) | (ACCIDENT_TIME == 9996) | (ACCIDENT_TIME == 9997) | (ACCIDENT_TIME == 9998) | (ACCIDENT_TIME == 9999) | (ACCIDENT_TIME == 9990) ] <- "Coded"
})

a<- which(is.na(d_NAr_Time$ACCIDENT_TIME_CAT))
d_NAr_Time2 <- d_NAr_Time[- a, ]

#Plot of the graph
plot(ggplot(d_NAr_Time2) +
  geom_bar(aes(x=ACCIDENT_TIME_CAT)) +
  theme(text = element_text(size= 10)) +
    labs(title = "Time of Accident")) 
```

![](Accident-data-visualisation-in-R_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

    Accidents happen more in the morning and afternoon. cod for entries where time is entered in range of 9991 to 9999 might have other specific meaning related to them, which cannot be known from this data.

<br> **Degree of Injury vs Time of the day**

``` r
#Data cleaning -  removing "?" in Degree of Injury
Lines.containing.questionmarks= grep("\\?",d_NAr_Time2$DEGREE_INJURY_CD)
d_NAr_Time_Q_Inj <- d_NAr_Time2[- Lines.containing.questionmarks, ]

#ploting tile graph 
counting<- count(d_NAr_Time_Q_Inj, DEGREE_INJURY_CD,  ACCIDENT_TIME_CAT)
ggplot(data=counting, mapping = aes(x = ACCIDENT_TIME_CAT, y= DEGREE_INJURY_CD)) +
  geom_tile(mapping = aes(fill=n)) +
  labs(title = "Relation of time of the day with degree of Injury")
```

![](Accident-data-visualisation-in-R_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    It can be observed that degree of injuries in range of 3 to 6 are most common special during morning and afternoon time.

<br>

**State-wise number of Accidents**

``` r
# plot of graph
plot(ggplot(d) +
  geom_bar(aes(x=FIPS_STATE_CD)) +
  theme(text = element_text(size= 10)) +
    labs(title = "State in which Accident has occured") +
    annotate("text", x = c(21, 42, 54), y = c(26000,16000, 33000), label = "High Number"))
```

![](Accident-data-visualisation-in-R_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    It can be observed that three states with code 21, 42 and 54 have highest number of acccidents. 

<br> **Circumstance of accident Vs Type of Accident**

``` r
#Data cleaning:
#CLASSIFICATION_CD and ACCIDENT_TYPE_CD have "?" as an anomaly entry and such entry i.e. "?" occur simultaneously in both of the columns
#removing"?" as it occurs in almost 2100 entries that 1% of data and hence can be removed for analysis
b = grep("\\?",(d$CLASSIFICATION_CD))
d_Qr_CLASS <- d[- b, ]

c = grep("\\?",(d_Qr_CLASS$ACCIDENT_TYPE_CD))
d_Qr_CLASS_ACCI <- d_Qr_CLASS[-c, ]

#Plot of count graph            
ggplot(data = d_Qr_CLASS_ACCI) +
  geom_count(mapping = aes(x=CLASSIFICATION_CD, y=ACCIDENT_TYPE_CD)) +
  labs(title = "Circumstance leading to type of accident")
```

![](Accident-data-visualisation-in-R_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    It can be oberved that combination of few circumstances (CLASSIFICATION_CD) and event (ACCIDENT_TYPE_CD) leading to Injury are more prominent than the other. Such as combination of circumstance 7, Fall of Roof along with event 44, i.e. Accident without injury, is one of the most common in the data. 

<br> **Experience of person involved vs Degree of Injury**

``` r
#cleaning data - removing symbol "?" from the data
d_Qr_TOT_EXPER <- d[-which(is.na(d$TOT_EXPER)),]
d_Qr_TOT_EXPER_OCC <- d_Qr_TOT_EXPER[-which(is.na(d_Qr_TOT_EXPER$MINE_EXPER)),]
d_Qr_TOT_EXPER_OCC_job <- d_Qr_TOT_EXPER_OCC[-which(is.na(d_Qr_TOT_EXPER_OCC$JOB_EXPER)),]
d_Qr_TOT_EXPER_OCC_job_Inj <- d_Qr_TOT_EXPER_OCC_job[-grep("\\?",d_Qr_TOT_EXPER_OCC_job$DEGREE_INJURY_CD),]

#this has led to cleaning of 37400 rows i.e. 18.5% of data but that is the only way to compare the experience of the workers who are involved in accidents.

#graph boxplot
ggplot((data= d_Qr_TOT_EXPER_OCC_job_Inj), mapping = aes(x=DEGREE_INJURY_CD, y=TOT_EXPER)) +
  geom_boxplot() +
  labs(title = "Experience of invovled person vs degree of Injury")
```

![](Accident-data-visualisation-in-R_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

    It can be interpreted from the above box plot that people with higher total work experience are more involved in higher degree of accident. It can be due to higher age (20+ work experience) of these workers and their reduction in spatial awareness in mining enviroment. Higher age employees might also be deficient in taking necessary action to avoid degree of injury at the time of accident. 

<br> **Type of Activities leading to Injuries**

``` r
#graph for Causes of Accidents
#Data cleaning
d_Qr_ACTI <- d[-grep("\\?",d$ACTIVITY_CD),]
d_Qr_ACTI_INJ_BODY_PART <- d_Qr_ACTI[-grep("\\?",d_Qr_ACTI$INJ_BODY_PART_CD),]
d_Qr_ACTI_INJ_BODY_PART_DEGREE_INJ<-
d_Qr_ACTI_INJ_BODY_PART[-grep("\\?",d_Qr_ACTI_INJ_BODY_PART$DEGREE_INJURY_CD),]

#graph plot
plot(ggplot(data=d_Qr_ACTI_INJ_BODY_PART_DEGREE_INJ) +
  geom_bar(aes(x=ACTIVITY_CD, fill=DEGREE_INJURY_CD)) +
  theme(text = element_text(size= 10)) +
    labs(title = "Count and Degree of injury associated with Activities") +
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=8, angle=90),  axis.text.y = element_text(face="bold", color="#993333", 
                           size=8)))
```

![](Accident-data-visualisation-in-R_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

    Maximum number of injuries happen during Activities 23, 28, 30, 39, 92 and 99. 
    23 is GET ON/OFF EQUIPMENT/MACHINES
    28 is HANDLING SUPPLIES/MATERIALS
    30 is HAND TOOLS (NOT POWERED)
    39 is MACHINE MAINTENANCE/REPAIR
    92 is WALKING/RUNNING
    99 is Unkown

    Degree of seriousness of most of the injuries lie in range of 2 to 6. surprisingly the most severe degree account maximum in activity 99, which is Unkown. It could be due activities which leads to such high degree of injury are not given under existing codes of data capturing system.   

<br> **Body part injured in activity**

``` r
#plot of graph
ggplot(data = d_Qr_ACTI_INJ_BODY_PART_DEGREE_INJ) +
  geom_count(mapping = aes(x=ACTIVITY_CD, y=INJ_BODY_PART_CD, color=DEGREE_INJURY_CD)) +
  theme(axis.text.x = element_text(face="bold", color="#993333", size=8, angle=90),  axis.text.y = element_text(face="bold", color="#993333", size=8))+
  annotate("rect", xmin = 20, xmax = 37, ymin = 23, ymax = 25,alpha = .2) +
  annotate("rect", xmin = 97, xmax = 101, ymin = 2, ymax = 45,alpha = .2, color="#993333") +
  annotate("rect", xmin = 90, xmax = 94, ymin = 26, ymax = 41,alpha = .2) +
  annotate("rect", xmin = 16, xmax = 37, ymin = 5, ymax = 7,alpha = .2) +
  annotate("rect", xmin = 16, xmax = 37, ymin = 27, ymax = 29,alpha = .2) +
  annotate("rect", xmin = 15, xmax = 24, ymin = 35, ymax = 37,alpha = .2) +
  annotate("text", x = c(28, 92, 27, 26, 19), y = c(25.5,41.5,7.5,29.5,37.5), label = "Major Contributors") +
  annotate("text", x = c(96), y = c(23.5), label = "Major Contributors to higher degree of Injuries i.e. greater than degree 6", angle=90, color="#993333") +
    labs(title = "Count and Degree of body part injury during Activities")
```

![](Accident-data-visualisation-in-R_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

    the above high-lighted Activites should be targeted such as 23, 28, 30, 39, 92 and 99. It should be seen what body part is suffering Injury and hence adequate counter measure should be taken such as specific body protective gear.

    For example, 

    there is very high number of injuries on FINGER(S)/THUMB (INJ_BODY_PART_CD number 340 on graph) which handling tools (ACTIVITY_CD number 30 on graph). 

    similarly counter measure can be taken for other major contributors to Injuries.

**Total business days lost due to Injury during Activity**

``` r
#data cleaning
d_0r_SCHE <- d[-grep("0",d$DAYS_LOST),]
d_0Br_SCHE <- d_0r_SCHE[-grep("",d_0r_SCHE$DAYS_LOST),]
d_0Br_SCHE_ACT <- d_0Br_SCHE[-grep("\\?",d_0Br_SCHE$ACTIVITY_CD),]

#plot bar graphs
ggplot(data=d_0Br_SCHE_ACT) +
  geom_bar(aes(x=ACTIVITY_CD, fill=DAYS_LOST)) +
  theme(text = element_text(size= 10)) +
    labs(title = "Loss of working days") +
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=8, angle=90),  axis.text.y = element_text(face="bold", color="#993333", 
                           size=8))
```

![](Accident-data-visualisation-in-R_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

    Activites such as 22, 28, 30, 39, 92 (where busines days lost are more due to frequent injurues) should be addressed first and adequate measure should be taken to create safe working condition for workers so that less business working days are lost

<br> **Number of Accidents reported to MSHA**

``` r
ggplot(data=d) +
  geom_bar(aes(x=IMMED_NOTIFY_CD, fill=DEGREE_INJURY_CD)) +
  theme(text = element_text(size= 10)) +
    labs(title = "Accidents reported to MSHA") +
   annotate("text", x = c(1,6), y = c(60000,30000), label = "Not reported to MSHA", angle=90)
```

![](Accident-data-visualisation-in-R_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

    Legend for X axis:
    (01) Death; (02) Serious injury; (03) Entrapment; (04) Inundation; (05) Gas of dust ignition; (06) Mine fire; (07) Explosives; (08) Roof fall; (09) Outburst; (10) Impounding dam; (11) Hoisting; (12) Offsite; (13) Not marked; (?) No value found

    Surprisingly, Majority of accidents which are of serious degree of Injury are not being reported to MSHA. This includes "?" and "13" bar in the above graph.

    on the other hand majority of incidents which are reported are of low degree of Injury.

    Such Non reporting hampers necessary action by MSHA to improve and mandate safe working conditions in Mines

<br> <br> ***Conclusion***

    Certain facts about Accidents:
    1) The number of Accidents have continously fallen over the over
    2) Accidents occur mostly underground followed by on strips and mill operations
    3) Accidents are evenly spread on days of the week
    4) Accidents mostly happen during Afternoon and Morning time of the day
    5) Degree of Injury is also more severe among Injuries which happen during Afternoon and Morning time of day
    6) Accidents occcur most in three states coded 21, 42 and 54

    Steps to prevent Accidents:
    7) There are few circumstance that lead to higher number of accidents such as roof falling. Such circumstance can be addressed by plant authorities specifically to construction of plant and equipments.
    8) People with higher work experience are more involved in more dangerous Injuries. either these people can be shifted to some other type of work than mining or can be given training.
    9) Maximum number of injuries happen during Activities 23, 28, 30, 39, 92 and 99. these activities are such as GET ON/OFF EQUIPMENT/MACHINES, HANDLING SUPPLIES/MATERIALS, etc. Special training/equipments can be provided to concerned employees to prevent future occurance of accidents.
    10) Certain Body part injury are more often than others such as on Fingers and Thumbs. Special protective gear can be provided to concerned employees to prevent injuries.

    Business Loss:
    11) Certain activities lead to more business days loss than others. these activites are number 28, 30, 92. These activites have to adopted all the sugesstion pointed in above points so that business loss can be reduced.

    Accountability and Preventive Measure:
    12) Surprisingle very few accidents are being reported to MSHA for investigation. Specially those with higher degree of Injuries are less frequently reported to MSHA for investigation. There needs to be a sound process in place so that people and process accountable for the accident can be retrained or rectified.

    Improvement in data capturing:
    13) For few attributes it is observedd that high number of event are not captured and "?" is entered. Such attributes include "Activity category", "Degree of Injury" and others. It might be due to lack of options under the attributes. Hence, it is recommended to update the data capturing system with adequate and updated number of options under each attribute.
