install.packages("dplyr")
install.packages("readxl")
install.packages("data.table")
install.packages("gt")
install.packages("psych")
install.packages("ggplot2")
install.packages("scales")
library(dplyr)
library(readxl)
library(data.table)
library(gt)
library(psych)
library(ggplot2)
library(scales)


setwd(dir="C:\\Users\\lixia\\OneDrive\\JHU\\senior thesis\\DATA")
import <- read.csv("CSS.TRENDS.94.06.csv")
rawdata <- import

#create sample data to test my code
#rawdata <- sample_n(copy, size = 100, replace = FALSE, weight = NULL)

#change column names in data to lowercase
names(rawdata) <- tolower(names(rawdata))

#variables are stored in "my variables.XLSX" 
#import "my variables.XLSX"
vars <- read_excel("my variables.xlsx")



#keep variables in data that appear in vars
#create a list of variables in vars
x <- c()
for (i in vars$var_name){
  x=c(x,i)
}
#only keep the columns with variables
data <- rawdata %>% select(all_of(x))



#construct variables
personality <- c("conscientiousness","extraversion","neuroticism","competitiveness","confidence")
#agreeableness


#conscientiousness
#hpw12,hpw13,hpw14,act03,act04,-act08,-act10,-act11,act12,act13,-act14
cons_vars <- c("hpw12","hpw13","hpw14","act03","act04","act08","act10","act11","act12","act13","act14")
for (i in c("act08","act10","act11","act14")) {
  if (sum(is.na(data[[i]]))!=nrow(data)) {      #do not recode columns that are all na's
    data[[i]] <- recode(data[[i]],`2`=2,`1`=3,`3`=1)   #recode the rest of the columns
  }
} 
normdata <- data %>% mutate_at(cons_vars,~(scale(.) %>% as.vector))
normdata$conscientiousness <- rowMeans(normdata[,cons_vars],na.rm=TRUE)


#openness


#extraversion
#rate09,rate10,hpw01,hpw05,hpw07
extr_vars <- c("rate09","rate10","hpw01","hpw05","hpw07")
normdata <- normdata %>% mutate_at(extr_vars,~(scale(.) %>% as.vector))
normdata$extraversion <- rowMeans(normdata[,extr_vars],na.rm=TRUE)


#neuroticism
#rate05
neur_vars <- "rate05"
normdata$rate05 <- recode(normdata$rate05,`1`=5,`2`=4,`3`=3,`4`=2,`5`=1)
normdata <- normdata %>% mutate_at(neur_vars,~(scale(.) %>% as.vector))
normdata$neuroticism <- normdata$rate05
 

#competitiveness
#rate03,rate04
comp_vars <- c("rate03","rate04")
normdata <- normdata %>% mutate_at(comp_vars,~(scale(.) %>% as.vector))
normdata$competitiveness <- rowMeans(normdata[,comp_vars],na.rm=TRUE)


#confidence
#rate12,rate13
conf_vars <- c("rate12","rate13")
normdata <- normdata %>% mutate_at(conf_vars,~(scale(.) %>% as.vector))
normdata$confidence <- rowMeans(normdata[,conf_vars],na.rm=TRUE)


#locus of control


#demographics
#sex,loanamt,poliview,race1-8,natengsp
demo_vars <- c("sex","loanamt","poliview","race1","race2","race3","race4","race5","race6","race7","race8","natengsp")


#dependent variable
#collgpa



#only keep the variables I need. create new dataset 
keep_vars <- unique(c("conscientiousness","extraversion","neuroticism","competitiveness","confidence",demo_vars,"collgpa")) #create list containing all potential useful variables. 
keepdata <- normdata %>% select(all_of(keep_vars)) #create dataset containing only useful variables. need to use all_of due to potential error message



#percentages of missing data in personality
personality_subset <- keepdata %>% select(all_of(personality))
percent_complete <- label_percent()(mean(complete.cases(personality_subset)))
percent_complete



#reshape from wide to long. GPA is measurement (dependent variable), all else are conditions (independent variables)

#add identification column to data. each identification (i.e. each row) describes a participant's response
keepdata$id <- seq.int(nrow(keepdata))
#move the id column to the front
keepdata <- keepdata %>% select(id, everything())

#convert from wide to long
long <- tidyr::gather(keepdata,"condition","measurement","conscientiousness":"collgpa",factor_key=FALSE)



#create descriptive statistics
#demographics
#overall, use keepdata
overall_pers <- colMeans(keepdata[,personality],na.rm=TRUE)

#personality by sex, use keepdata
male <- keepdata[keepdata$sex == 1,]
female <- keepdata[keepdata$sex == 2,]

male_pers <- colMeans(male[,personality],na.rm=TRUE)
female_pers <- colMeans(female[,personality],na.rm=TRUE)

#personality by race, use keepdata
white <- keepdata[keepdata$race1 == 2,]
nonwhite <- keepdata[keepdata$race1 != 2,]

white_pers <- colMeans(white[,personality],na.rm=TRUE)
nonwhite_pers <- colMeans(nonwhite[,personality],na.rm=TRUE)


#create dataframe
names <- c("Overall","Male","Female","White","Non-White")
frame_pers <- transpose(data.frame(overall_pers,male_pers,female_pers,white_pers,nonwhite_pers))
frame_pers <- cbind(names,frame_pers)

table_pers <-
  frame_pers %>%
    gt(rowname_col = "names") %>%
    tab_header(
      title = "Personality by Sex and Race",
      subtitle = "Data from CSS 1994 to 2006"
    ) %>%
    tab_spanner(
      label = "Personality",
      columns = vars(V1,V2,V3,V4,V5)
    ) %>%
    cols_label(
      V1 = personality[[1]],
      V2 = personality[[2]],
      V3 = personality[[3]],
      V4 = personality[[4]],
      V5 = personality[[5]]
    ) %>%
    fmt_number(
      columns = vars(V1,V2,V3,V4,V5),
      decimals = 2
    ) %>%
    tab_footnote(
      footnote = "Data is standardized.",
      locations = cells_column_labels(
        columns = vars(V1,V2,V3,V4,V5)
      )
    )
    
table_pers

#summary statistics
cons_des <- describe(keepdata$conscientiousness)
extr_des <- describe(keepdata$extraversion)
neur_des <- describe(keepdata$neuroticism)
comp_des <- describe(keepdata$competitiveness)
conf_des <- describe(keepdata$confidence)

frame_des <- rbind(cons_des,extr_des,neur_des,comp_des,conf_des)
frame_des <- cbind(personality,frame_des)

table_des <-
  frame_des %>%
  gt(rowname_col = "personality") %>%
  tab_header(
    title = "Personality Summary Statistics",
    subtitle = "Data from CSS 1994 to 2006"
  ) %>%
  fmt_number(
    columns = vars(mean,sd,median,trimmed,mad,min,max,range,skew,kurtosis,se),
    decimals = 2
  ) 

table_des



#graph
plot_cons <- ggplot2::ggplot(keepdata,ggplot2::aes(x = sex, y = conscientiousness)) + 
  ggplot2::geom_point() +
  ggplot2::ggtitle("Conscientiousness by Sex") +
  ggplot2::xlab("Female-Male")
  ggplot2::ylab("Conscientiousness")

plot_extr <- ggplot2::ggplot(keepdata,ggplot2::aes(x = sex, y = extraversion)) + 
  ggplot2::geom_point() +
  ggplot2::ggtitle("Extraversion by Sex") +
  ggplot2::xlab("Female-Male")
  ggplot2::ylab("Extraversion")
  
plot_neur <- ggplot2::ggplot(keepdata,ggplot2::aes(x = sex, y = neuroticism)) + 
  ggplot2::geom_point() +
  ggplot2::ggtitle("Neuroticism by Sex") +
  ggplot2::xlab("Female-Male")
  ggplot2::ylab("Neuroticism")
  
plot_comp <- ggplot2::ggplot(keepdata,ggplot2::aes(x = sex, y = competitiveness)) + 
  ggplot2::geom_point() +
  ggplot2::ggtitle("Competitiveness by Sex") +
  ggplot2::xlab("Female-Male")
  ggplot2::ylab("Competitiveness")
  
plot_conf <- ggplot2::ggplot(keepdata,ggplot2::aes(x = sex, y = confidence)) + 
  ggplot2::geom_point() +
  ggplot2::ggtitle("Confidence by Sex") +
  ggplot2::xlab("Female-Male")
  ggplot2::ylab("Confidence")
  

plot_cons
plot_extr
plot_neur
plot_comp
plot_conf

  
  
  
  