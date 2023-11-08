
##########################################################
# Data Preparation
##########################################################
# install package if missing
if(!require(readr))     install.packages("readr"    , repos = "http://cran.us.r-project.org")
if(!require(dplyr))     install.packages("dplyr"    , repos = "http://cran.us.r-project.org")
if(!require(tidyr))     install.packages("tidyr"    , repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(janitor))   install.packages("janitor"  , repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(janitor)
library(tidyverse)

# Get DC characters:
comic_characters_dc <- 
    paste("https://github.com/fivethirtyeight/data/raw",
          "/master/comic-characters/dc-wikia-data.csv", sep="") %>% 
    read_csv() %>% 
    clean_names() %>% 
    mutate(publisher = "DC")

# Get Marvel characters:
comic_characters_marvel <- 
    paste("https://github.com/fivethirtyeight/data/raw",
          "/master/comic-characters/marvel-wikia-data.csv", sep="") %>% 
    read_csv() %>% 
    clean_names() %>% 
    mutate(publisher = "Marvel")

# Merge two dataset and perform further data wrangling:
comic_characters <-
    comic_characters_dc %>% 
    bind_rows(comic_characters_marvel) %>% 
    separate(first_appearance, c("year2", "month"), ", ", remove = FALSE) %>%
    mutate(
        # If month was missing, set as January and day as 01:
        month = ifelse(is.na(month), "01", month),
        day = "01",
        # Note some years missing:
        date = ymd(paste(year, month, day, sep = "-")),
        align = factor(
            align, 
            levels = c("Bad Characters", "Reformed Criminals", 
                       "Netural Characters", "Good Characters"),
            ordered = TRUE)
            ) %>%
    select(publisher, everything(), -c(year2, day))

rm(comic_characters_marvel,comic_characters_dc)

# --------------------------------------------------------
# Save dataframe to data_comics.RData
# --------------------------------------------------------
# To save time when running the file multiple times the dataset was stored in an extra file using the following line. 
# It can also be downloaded from the github repository.
# The created file has to be placed in the same folder as the code and the working directory has to be set to this folder!
# save(comic_characters, file = "data_comics.RData")
# Afterwards it can be loaded using the following line 68.

# --------------------------------------------------------
# Load dataframe from data_comics.RData
# --------------------------------------------------------
# If you want to load the data from file uncommend the next line to load it. 
# load("data_comics.RData")
# The dataset has to be downloaded from the github repository or stored in a separate file before (see lines 58-61 above) 


##########################################################
# Data Wrangling
##########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)

# --------------------------------------------------------
# General Design of the Dataframe
# --------------------------------------------------------
head(comic_characters)
summary(comic_characters)

# --------------------------------------------------------
# Transform Character Data to Factors
# --------------------------------------------------------
comic_characters2 <- comic_characters %>% 
                     mutate(across(c(1,5,6,7,8,9,10,11,14),as.factor))

# Remove original comic_characters dataframe as it is not needed anymore
rm(comic_characters)

# --------------------------------------------------------
# Correct False Entries
# --------------------------------------------------------

# Levels of eye show some "auburn hair" entries  
levels(comic_characters2$eye)

# The following entries are affected:
comic_characters2 %>% filter(!is.na(eye)) %>% filter(eye =="Auburn Hair")

# A quick search on the DC database (on which the dataset is based) shows, that this is an error and should be corrected
# https://dc.fandom.com/wiki/DC_Comics_Database
# The hair information of these entries is corrected to "Auburn Hair" and the eye information is added based on pictures in the database
comic_characters2[comic_characters2$page_id == 80676,  "hair"] <- "Auburn Hair"
comic_characters2[comic_characters2$page_id == 80676,  "eye"]  <- "Blue Eyes"

comic_characters2[comic_characters2$page_id == 146812, "hair"] <- "Auburn Hair"
comic_characters2[comic_characters2$page_id == 146812, "eye"]  <- "Brown Eyes"

comic_characters2[comic_characters2$page_id == 114487, "hair"] <- "Auburn Hair"
comic_characters2[comic_characters2$page_id == 114487, "eye"]  <- "Green Eyes"

comic_characters2[comic_characters2$page_id == 192614, "hair"] <- "Auburn Hair"
comic_characters2[comic_characters2$page_id == 192614, "eye"]  <- "Yellow Eyes"

comic_characters2[comic_characters2$page_id == 130938, "hair"] <- "Auburn Hair"
comic_characters2[comic_characters2$page_id == 130938, "eye"]  <- "Blue Eyes"

comic_characters2[comic_characters2$page_id ==  71092, "hair"] <- "Auburn Hair"
comic_characters2[comic_characters2$page_id ==  71092, "eye"]  <- "Brown Eyes"

comic_characters2[comic_characters2$page_id ==   4935, "hair"] <- "Auburn Hair"
comic_characters2[comic_characters2$page_id ==   4935, "eye"]  <-  NA

# drop levels in dataframe that are not used anymore
comic_characters2 <- droplevels(comic_characters2)

# "Auburn Hair" is no level in the eye variable anymore
levels(comic_characters2$eye)

# --------------------------------------------------------
# Extract RGB Information of Eye Color
# --------------------------------------------------------

# get all levels of eye color
levels(comic_characters2$eye)

# The hex values that correspond to the given color names are stored in new column eye_HEX
# The hex values are based on:
# https://en.wikipedia.org/wiki/List_of_colors_by_shade
# https://www.color-name.com

comic_characters2$eye_HEX <- NA
comic_characters2$eye_HEX[comic_characters2$eye == "Amber Eyes"]         <- "#FFBF00"
comic_characters2$eye_HEX[comic_characters2$eye == "Black Eyeballs"]     <- "#000000"
comic_characters2$eye_HEX[comic_characters2$eye == "Black Eyes"]         <- "#000000"
comic_characters2$eye_HEX[comic_characters2$eye == "Blue Eyes"]          <- "#0000FF"
comic_characters2$eye_HEX[comic_characters2$eye == "Brown Eyes"]         <- "#964B00"
comic_characters2$eye_HEX[comic_characters2$eye == "Compound Eyes"]      <-  NA  
comic_characters2$eye_HEX[comic_characters2$eye == "Gold Eyes"]          <- "#FFD700"
comic_characters2$eye_HEX[comic_characters2$eye == "Green Eyes"]         <- "#00FF00"
comic_characters2$eye_HEX[comic_characters2$eye == "Grey Eyes"]          <- "#808080"
comic_characters2$eye_HEX[comic_characters2$eye == "Hazel Eyes"]         <- "#C9C789"
comic_characters2$eye_HEX[comic_characters2$eye == "Magenta Eyes"]       <- "#FF00FF"
comic_characters2$eye_HEX[comic_characters2$eye == "Multiple Eyes"]      <-  NA
comic_characters2$eye_HEX[comic_characters2$eye == "No Eyes"]            <-  NA
comic_characters2$eye_HEX[comic_characters2$eye == "One Eye"]            <-  NA
comic_characters2$eye_HEX[comic_characters2$eye == "Orange Eyes"]        <- "#FF8000"
comic_characters2$eye_HEX[comic_characters2$eye == "Photocellular Eyes"] <-  NA
comic_characters2$eye_HEX[comic_characters2$eye == "Pink Eyes"]          <- "#FFC0CB"
comic_characters2$eye_HEX[comic_characters2$eye == "Purple Eyes"]        <- "#800080"
comic_characters2$eye_HEX[comic_characters2$eye == "Red Eyes"]           <- "#FF0000"
comic_characters2$eye_HEX[comic_characters2$eye == "Silver Eyes"]        <- "#C0C0C0"
comic_characters2$eye_HEX[comic_characters2$eye == "Variable Eyes"]      <-  NA
comic_characters2$eye_HEX[comic_characters2$eye == "Violet Eyes"]        <- "#8000FF"
comic_characters2$eye_HEX[comic_characters2$eye == "White Eyes"]         <- "#FFFFFF"
comic_characters2$eye_HEX[comic_characters2$eye == "Yellow Eyeballs"]    <- "#FFFF00"
comic_characters2$eye_HEX[comic_characters2$eye == "Yellow Eyes"]        <- "#FFFF00"

# convert HEX to RGB values and store them in separate columns
comic_characters2$eye_R <- col2rgb(comic_characters2$eye_HEX)[1,]
comic_characters2$eye_G <- col2rgb(comic_characters2$eye_HEX)[2,]
comic_characters2$eye_B <- col2rgb(comic_characters2$eye_HEX)[3,]

# col2rgb interprets NA values as 255-255-255. They are set to median value manually 
comic_characters2$eye_R[is.na(comic_characters2$eye_HEX)] <- median(comic_characters2$eye_R, na.rm = TRUE)
comic_characters2$eye_G[is.na(comic_characters2$eye_HEX)] <- median(comic_characters2$eye_G, na.rm = TRUE)
comic_characters2$eye_B[is.na(comic_characters2$eye_HEX)] <- median(comic_characters2$eye_B, na.rm = TRUE)

# --------------------------------------------------------
# Extract RGB Information of Hair Color
# --------------------------------------------------------

# get all levels of eye color
levels(comic_characters2$hair)

# The hex values that correspond to the given color names are stored in new column eye_HEX
# The hex values are based on:
# https://en.wikipedia.org/wiki/List_of_colors_by_shade
# https://www.color-name.com

comic_characters2$hair_HEX <- NA
comic_characters2$hair_HEX[comic_characters2$hair == "Auburn Hair"]           <- "#A52A2A"
comic_characters2$hair_HEX[comic_characters2$hair == "Bold"]                  <-  NA
comic_characters2$hair_HEX[comic_characters2$hair == "Black Hair"]            <- "#000000"
comic_characters2$hair_HEX[comic_characters2$hair == "Blond Hair"]            <- "#F5DEB3"
comic_characters2$hair_HEX[comic_characters2$hair == "Blue Hair"]             <- "#0000FF"
comic_characters2$hair_HEX[comic_characters2$hair == "Bronze Hair"]           <- "#CD7F32"
comic_characters2$hair_HEX[comic_characters2$hair == "Brown Hair"]            <- "#964B00"
comic_characters2$hair_HEX[comic_characters2$hair == "Gold Hair"]             <- "#FFD700"
comic_characters2$hair_HEX[comic_characters2$hair == "Green Hair"]            <- "#00FF00"
comic_characters2$hair_HEX[comic_characters2$hair == "Grey Hair"]             <- "#808080"
comic_characters2$hair_HEX[comic_characters2$hair == "Light Brown Hair"]      <- "#BD9A7A"
comic_characters2$hair_HEX[comic_characters2$hair == "Magenta Hair"]          <- "#FF00FF"
comic_characters2$hair_HEX[comic_characters2$hair == "No Hair"]               <-  NA
comic_characters2$hair_HEX[comic_characters2$hair == "Orange Hair"]           <- "#FF8000"
comic_characters2$hair_HEX[comic_characters2$hair == "Orange-brown Hair"]     <- "#C36F29"
comic_characters2$hair_HEX[comic_characters2$hair == "Pink Hair"]             <- "#FFC0CB"
comic_characters2$hair_HEX[comic_characters2$hair == "Platinum Blond Hair"]   <- "#F3ECDE"
comic_characters2$hair_HEX[comic_characters2$hair == "Purple Hair"]           <- "#800080"
comic_characters2$hair_HEX[comic_characters2$hair == "Red Hair"]              <- "#FF0000"
comic_characters2$hair_HEX[comic_characters2$hair == "Reddish Blond Hair"]    <- "#FF7F50"
comic_characters2$hair_HEX[comic_characters2$hair == "Reddish Brown Hair"]    <- "#A52A2A"
comic_characters2$hair_HEX[comic_characters2$hair == "Silver Hair"]           <- "#C0C0C0"
comic_characters2$hair_HEX[comic_characters2$hair == "Strawberry Blond Hair"] <- "#FFE5B4"
comic_characters2$hair_HEX[comic_characters2$hair == "Variable Hair"]         <-  NA
comic_characters2$hair_HEX[comic_characters2$hair == "Violet Hair"]           <- "#8000FF"
comic_characters2$hair_HEX[comic_characters2$hair == "White Hair"]            <- "#FFFFFF"
comic_characters2$hair_HEX[comic_characters2$hair == "Yellow Hair"]           <- "#FFFF00"

# convert HEX to RGB values and store them in separate columns
comic_characters2$hair_R <- col2rgb(comic_characters2$hair_HEX)[1,]
comic_characters2$hair_G <- col2rgb(comic_characters2$hair_HEX)[2,]
comic_characters2$hair_B <- col2rgb(comic_characters2$hair_HEX)[3,]

# col2rgb interprets NA values as 255-255-255. They are set to median value manually 
comic_characters2$hair_R[is.na(comic_characters2$hair_HEX)] <- median(comic_characters2$hair_R, na.rm = TRUE)
comic_characters2$hair_G[is.na(comic_characters2$hair_HEX)] <- median(comic_characters2$hair_G, na.rm = TRUE)
comic_characters2$hair_B[is.na(comic_characters2$hair_HEX)] <- median(comic_characters2$hair_B, na.rm = TRUE)

# --------------------------------------------------------
# Create Optimized Dataframe
# --------------------------------------------------------

# Remove columns that are not used in the analysis
comic_characters2 <- comic_characters2 %>% 
  subset(select = -c(urlslug, first_appearance,date,eye_HEX,hair_HEX)) 

# percent of NA values in align variable
share_NA_in_align <- sum(is.na(comic_characters2$align)) / nrow(comic_characters2) * 100
share_NA_in_align

# Final goal is to estimate the align of the characters
# NA values in align are removed, as those cannot be estimated later
comic_characters2 <- comic_characters2 %>% 
  filter(!is.na(align))

# Final Dataframe
head(comic_characters2)
summary(comic_characters2)


##########################################################
# Partitioning of the Dataset
##########################################################
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)

set.seed(1)
test_index <- createDataPartition(comic_characters2$align, 
                                  times = 1, 
                                  p = 0.1, 
                                  list = FALSE)
test  <- comic_characters2[test_index,]     # 10% of data
train <- comic_characters2[-test_index,]    # 90% of data

nrow(train)
nrow(test)

##########################################################
# Data Exploration and Visualization
##########################################################

# --------------------------------------------------------
# Number of NA Values
# --------------------------------------------------------

# Plot of number of NA values the variables contain
if(!require(fmsb)) install.packages("fmsb", repos = "http://cran.us.r-project.org")
library(fmsb)

# summarize number of NA values (without RGB variables)
data_NA <- train %>% 
           subset(select = -c(eye_R,eye_G,eye_B,hair_R,hair_G,hair_B)) %>% 
           summarise(across(everything(), ~ sum(is.na(.)))) 

# calculate percent values
data_NA <- data_NA/nrow(train)*100 
# results as table
data_NA %>% pivot_longer(everything()) %>% arrange(desc(value))

# results in radar plot
data_NA <- rbind(rep(100,13) , rep(0,13) , data_NA)
radarchart(data_NA, axistype=1 , 
           pcol=rgb(0,0.2,0.7,0.9), 
           pfcol=rgb(0,0.2,0.7,0.2),
           plwd=4, 
           cglcol="grey", 
           cglty=1, 
           axislabcol="grey", 
           caxislabels=seq(0,100,25), 
           cglwd=0.8,
           vlcex=0.6 
)

# --------------------------------------------------------
# Eye and Hair Colors
# --------------------------------------------------------

# Treemap of eye colors
if(!require(treemap)) install.packages("treemap", repos = "http://cran.us.r-project.org")
library(treemap)

# Treemap of eye colors
data_eye <- train %>% group_by(eye) %>%   summarize(count = n()) %>% arrange(desc(count))

treemap(data_eye,
        index="eye",
        vSize="count",
        type="index"
        )

# Treemap of hair colors
data_hair <- train %>% group_by(hair) %>%   summarize(count = n()) %>% arrange(desc(count))

treemap(data_hair,
        index="hair",
        vSize="count",
        type="index"
        )

# --------------------------------------------------------
# Align vs. Sex of the Characters
# --------------------------------------------------------

# Table: Align vs  Sex 
train %>% group_by(align,sex) %>% 
          summarize(count = n()) %>% 
          arrange(desc(count))

# Barplot Sex and Align (without "Reformed Criminals")
train %>% filter(align!="Reformed Criminals") %>% 
  ggplot(aes(x=sex, fill=align)) + 
  geom_bar() + 
  theme(legend.position = "bottom") +
  scale_x_discrete(labels = function(sex) str_wrap(sex, width = 10))

# Scaled Barplot Sex and Align (without "Reformed Criminals")
train %>% filter(align!="Reformed Criminals") %>% 
  ggplot(aes(x=sex, fill=align)) + 
  geom_bar(position="fill") + 
  theme(legend.position = "bottom") +
  scale_x_discrete(labels = function(sex) str_wrap(sex, width = 10))

# --------------------------------------------------------
# Align vs. Eye and Hair Color Values
# --------------------------------------------------------

# Violin plots align vs. RGB values of eye color (without "Reformed Criminals")
train %>% filter(align!="Reformed Criminals") %>% 
          select(align, eye_R, eye_G, eye_B) %>%
          pivot_longer(., cols = c(eye_R,eye_G,eye_B), names_to = "Color", values_to = "RGB_value") %>% 
          ggplot( aes(x=align, y=RGB_value, fill=Color)) + 
          geom_violin()

# Violin plots align vs. RGB values of hair color (without "Reformed Criminals")
train %>% filter(align!="Reformed Criminals") %>% 
          select(align, hair_R, hair_G, hair_B) %>%
          pivot_longer(., cols = c(hair_R,hair_G,hair_B), names_to = "Color", values_to = "RGB_value") %>% 
          ggplot( aes(x=align, y=RGB_value, fill=Color)) + 
          geom_violin()


##########################################################
# Association Analysis
##########################################################

# --------------------------------------------------------
# Implementation of the Association Analysis
# --------------------------------------------------------

if(!require(rcompanion)) install.packages("rcompanion", repos = "http://cran.us.r-project.org")
library(rcompanion)

# function to determine if data is nominal
is_nominal_value <- function(val){
    is.factor(val) || is.character(val)
    }

# function to determine if data is numeric 
is_numeric_value <- function(val){
    is.double(val) || is.integer(val)
    }

# function to calculate all association values depending on data type
calc_association <- function(pos1,pos2){
    xdata <- pull(train_assoc,pos1)
    xname <- col_names[pos1]
        
    ydata <- pull(train_assoc,pos2)
    yname <- col_names[pos2]
        
    # if both vectors are nominal calculate cramersV
    if(is_nominal_value(xdata) && is_nominal_value(ydata)){
        cv <-  cramerV(as.character(xdata), as.character(ydata))#, bias.correct = FALSE)
        data.frame(xname, yname, assoc_value=cv, type="cramersV")
        }
    # if both vectors are numeric, calculate correlation
    else if(is_numeric_value(xdata) && is_numeric_value(ydata)){
        corr <- cor(xdata, ydata,use="complete.obs")#, method="spearman", use="complete.obs")
        data.frame(xname, yname, assoc_value=corr, type="correlation")
        }   
    # if first vector is numeric and second nominal calculate ANOVA
    else if(is_numeric_value(xdata) && is_nominal_value(ydata)){
        r_squared = summary(lm(xdata ~ ydata))$r.squared  
        data.frame(xname, yname, assoc_value=r_squared, type="anova")
        } 
    # if first vector is nominal and second numeric calculate ANOVA
    else if(is_nominal_value(xdata) && is_numeric_value(ydata)){
        r_squared = summary(lm(ydata ~ xdata))$r.squared  
        data.frame(xname, yname, assoc_value=r_squared, type="anova")
        }
    else{
        print("not defined")
        }
    }

# remove page_id and name from dataset
train_assoc <- train %>% subset(select = -c(page_id,name))

# calculate all possible variable combinations
combi <-crossing(1:ncol(train_assoc),1:ncol(train_assoc))
col_names <- names(train_assoc)

# calculate association for all combinations
if(!require(fBasics)) install.packages("fBasics", repos = "http://cran.us.r-project.org")
library(fBasics)

association_matrix <- map2_df(rowVec(combi[,1]), rowVec(combi[,2]), calc_association)

# --------------------------------------------------------
# Results of Association Analysis
# --------------------------------------------------------

# Correlation
association_matrix %>%
  filter(type=="correlation") %>%
  ggplot(aes(xname,yname,fill=assoc_value))+
  geom_tile()+
  geom_text(aes(xname,yname,label=round(assoc_value,2)))+
  scale_fill_gradient(low="red", high="yellow")+
  theme(legend.position = "bottom", 
        legend.text = element_text(angle = 45, vjust = 1, hjust=1),
        legend.text.align = 1,
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
        )
                                   
# ANOVA
association_matrix %>%
  filter(type=="anova") %>%
  ggplot(aes(xname,yname,fill=assoc_value))+
  geom_tile()+
  geom_text(size=3,aes(xname,yname,label=round(assoc_value,2)))+
  scale_fill_gradient(low="red", high="yellow")+
  theme(legend.position = "bottom", 
        legend.text = element_text(angle = 45, vjust = 1, hjust=1),
        legend.text.align = 1,
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
        )

# CramersV
association_matrix %>%
  filter(type=="cramersV") %>%
  ggplot(aes(xname,yname,fill=assoc_value))+
  geom_tile()+
  geom_text(aes(xname,yname,label=round(assoc_value,2)))+
  scale_fill_gradient(low="red", high="yellow")+
  theme(legend.position = "bottom", 
        legend.text = element_text(angle = 45, vjust = 1, hjust=1),
        legend.text.align = 1,
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
        )


##########################################################
# Modelling Approach
##########################################################

# --------------------------------------------------------
# (1) Guessing the Outcome
# --------------------------------------------------------

set.seed(1)

timeStart <- Sys.time() # get start time of calculation

    y_hat1 <- sample(c("Bad Characters", "Good Characters"), 
                     length(test_index), replace = TRUE) %>%
              ordered(levels = levels(test$align)) 
  
timeEnd   <- Sys.time() # get end time of calculation
timeDiff1 <- timeEnd - timeStart

# Overall Accuracy
acc1 <- mean(y_hat1 == test$align) 
acc1  # 0.4883041


# --------------------------------------------------------
# (2) Estimation using Numeric Data: Loess Estimation
# --------------------------------------------------------

set.seed(1)

# 2a - loess - hair and eye RGB values
# -----------------------

# install package if missing
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(splines)) install.packages("splines", repos = "http://cran.us.r-project.org")
if(!require(foreach)) install.packages("foreach", repos = "http://cran.us.r-project.org")

library(gam)
library(splines)
library(foreach)

timeStart <- Sys.time() # get start time of calculation

    # Calculate Fit
    fit2a <- train(align ~ hair_R + hair_G + hair_B + eye_R + eye_G + eye_B, 
                   data = train, 
                   method = "gamLoess")
    # Prediction
    y_hat2a <- predict(fit2a, newdata=test) %>% factor(levels = levels(test$align))

timeEnd <- Sys.time() # get end time of calculation
timeDiff2a <- timeEnd - timeStart    
    
acc2a <- mean(y_hat2a == test$align) 
acc2a # 0.4637427

# values in estimation and test set
results_mod2a  <- data.frame(y_hat2a,test$align) %>% pivot_longer(everything()) 
results_mod2a %>% ggplot(aes(x=name,fill=value)) + 
                  geom_bar() +
                  theme(legend.position = "bottom")
# --> accuracy worse than guessing, algorithm estimates lots of reformed criminals


# 2b - loess - train model without level "Reformed Criminals"
# -----------------------------------------------

# Create train2 dataframe without entries with align "Reformed Criminals"
train2b <- train %>% filter(align!="Reformed Criminals") 
train2b <- droplevels(train2b)

timeStart <- Sys.time() # get start time of calculation

    # Calculate Fit
    fit2b <- train(align ~ hair_R + hair_G + hair_B + eye_R + eye_G + eye_B, 
                   data = train2b, 
                   method = "gamLoess")
    # Prediction
    y_hat2b <- predict(fit2b, newdata=test) %>% factor(levels = levels(test$align))

timeEnd <- Sys.time() # get end time of calculation
timeDiff2b <- timeEnd - timeStart        
    
acc2b <- mean(y_hat2b == test$align) 
acc2b # 0.6052632

# values in estimation and test set
results_mod2b  <- data.frame(y_hat2b,test$align) %>% pivot_longer(everything()) 
results_mod2b %>% ggplot(aes(x=name,fill=value)) + 
                  geom_bar() +
                  theme(legend.position = "bottom")
# --> better estimation


# --------------------------------------------------------
# (3) Estimation using Numeric and Categorical Data: Decision Trees
# --------------------------------------------------------

# 3a - decision tree - hair and eye information
# -----------------------------

if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
library(rpart)

set.seed(1)

timeStart <- Sys.time() # get start time of calculation

  # Calculate Fit
  fit3a <- rpart(align ~ hair + eye, 
                 data = train, 
                 method = "class", 
                 cp = 0.01)
  # Prediction
  y_hat3a <- predict(fit3a, newdata=test,type = "vector")

timeEnd <- Sys.time() # get end time of calculation
timeDiff3a <- timeEnd - timeStart

# Plot Tree
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
library(rpart.plot)
prp(fit3a, type=2, extra=0)

# Overall Accuracy
acc3a <- mean(y_hat3a == as.numeric(test$align)) 
acc3a  # 0.5982456


# 3b - decision tree - hair and eye RGB
# ------------------------------------

set.seed(1)

timeStart <- Sys.time() # get start time of calculation

  # Calculate Fit
  fit3b <- rpart(align ~ hair_R + hair_G + hair_B + eye_R + eye_G + eye_B, 
                 data = train, 
                 method = "class", 
                 cp = 0.01)

  # Prediction
  y_hat3b <- predict(fit3b, newdata=test,type = "vector")
  
timeEnd <- Sys.time() # get end time of calculation
timeDiff3b <- timeEnd - timeStart  
  
# Plot Tree
prp(fit3b, type=2, extra=0)

# Overall Accuracy
acc3b <- mean(y_hat3b == as.numeric(test$align)) 
acc3b  # 0.6040936
# --> calculates fit much faster and better results

# 3c - decision tree - hair and eye RGB and sex, alive, appearances, id
# ----------------------------------------------------------------

set.seed(1)

timeStart <- Sys.time() # get start time of calculation

  # Calculate Fit
  fit3c <- rpart(align ~ hair_R + hair_G + hair_B + eye_R + eye_G + eye_B + 
                         sex + alive + appearances + year + publisher + id, 
                 data = train, 
                 method = "class", 
                 cp = 0.01)

  # Prediction
  y_hat3c <- predict(fit3c, newdata=test,type = "vector")

timeEnd <- Sys.time() # get end time of calculation
timeDiff3c <- timeEnd - timeStart

# Plot Tree
prp(fit3c, type=2, extra=0)

# Overall Accuracy
acc3c <- mean(y_hat3c == as.numeric(test$align)) 
acc3c  # 0.6637427
# --> quite fast and even better results


# 3d - decision tree - hair and eye RGB and sex, alive, appearances with optimized cp
# -----------------------------------------------------------------------------------

# Optimize cp value for best model
set.seed(1)
# create new data partitions for optimization with train dataset
test_index <- createDataPartition(train$align, times = 1, p = 0.2, list = FALSE)
test_from_train  <- train[test_index,]     # 10% of data
train_from_train <- train[-test_index,]    # 90% of data

# Create vector of possible cp values
cp_values =seq(0,0.015, len = 25)   

# Calculate accuracies for all possible cp values
acc_values <- sapply(cp_values, function(cp_val){
  fit_opt <- rpart(align ~ hair_R + hair_G + hair_B + eye_R + eye_G + eye_B +
                           sex + alive + appearances + year + publisher + id, 
                   data = train_from_train, 
                   method = "class", 
                   cp = cp_val)
  
  # Prediction
  y_hat_opt <- predict(fit_opt, newdata=test_from_train,type = "vector")
  # Overall Accuracy
  mean(y_hat_opt == as.numeric(test_from_train$align))
})

# plot results
results_cp <- data.frame(cp_values,acc_values)
results_cp %>% ggplot(aes(x=cp_values, y=acc_values)) + geom_point()

# get optimal cp for highest accuracy
cp_opt <- cp_values[which.max(acc_values)]
cp_opt


# Recalculate with optimal cp

set.seed(1)

timeStart <- Sys.time() # get start time of calculation

    # Calculate Fit
    fit3d <- rpart(align ~ hair_R + hair_G + hair_B + eye_R + eye_G + eye_B + 
                           sex + alive + appearances + year + publisher + id, 
                   data = train, 
                   method = "class", 
                   cp = cp_opt)

    # Prediction
    y_hat3d <- predict(fit3d, newdata=test,type = "vector")

timeEnd <- Sys.time() # get end time of calculation
timeDiff3d <- timeEnd - timeStart

# Overall Accuracy
acc3d <- mean(y_hat3d == as.numeric(test$align)) 
acc3d  # 0.7017544
# --> quite fast and even better results

# Plot Tree
prp(fit3d, type=2, extra=0)
