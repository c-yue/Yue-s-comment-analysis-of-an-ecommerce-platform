#library import
install.packages("openxlsx")
library("openxlsx")
library(dplyr)
library(sqldf)
library(ggplot2)

#VIEW DATA
#just to view the comment data
browse <- read.csv("csv-cmtDtl/1006001.csv")
browse
dim(browse)
names(browse)
head(browse)
summary(browse)
#to test if there are lots of "0" in the "star" column
tail(browse) 
#just to view the attr data
browse <- read.xlsx("commAttri.xlsx",sheet = 1)
dim(browse)
names(browse)
head(browse)
summary(browse)


#READ DATA
#read attr data
attrData <- read.xlsx("commAttri.xlsx",sheet = 1)
#read comment data
dir <- "csv-cmtDtl"
files = list.files(dir) 
n <- length(files)
chart_list<- vector()
cmtData <- vector()

for (i in 1:n)
        {
                sub_direc <- files[i]
                new_direc <- paste(dir, sub_direc, sep = "/")
                new_data <- read.csv(new_direc,row.names=1)
                #chart_list[i] <- sqldf("SELECT COUNT(*) FROM new_data")
                cmtData <- rbind(data,new_data)
        }


#CLEAN DATA
#change the data using tbl_df
cmtTbl <- tbl_df(cmtData)
attrTbl <- tbl_df(attrData)
#extract core information from comment data
summary(cmtTbl)
#what 'star = 0' means
#it shows that customers miss the star comment
#because there are positive comments in these columns which means that they won't give a 0 point 
cmtTbl %>%
        select(itemId,star,content,createTime) %>%
        filter(star == 0)
#delete unvalueable stars from cmtTbl
cmtTbl_flt <- 
        cmtTbl%>%
        select(itemId,star,content,createTime) %>%
        filter(star != 0)

#BASIC ANALYSIS
#calculate star of all items
item_star <- 
        cmtTbl_flt %>%
        select(itemId,star) %>%
        group_by(itemId) %>%
        summarize(star_mean = mean(star), star_median = median(star), n = n())
#view the star distribution of all items
hist(item_star$star_median,col="lightblue",border="black",labels=TRUE,ylim=c(0,2000))
hist(item_star$star_mean,col="lightblue",border="black",labels=TRUE,ylim=c(0,200))
#test if all items have 5 median star  --  RIGHT
summary(item_star)
#top 10 comment 
top_comment <- 
        cmtTbl %>%
        select(itemId,content) %>%
        group_by(content) %>%
        summarize(num = n(content))






#GROUP ANALYSIS
#join comment data and item information to explore more
cmt_attr <- 
        left_join(cmtTbl_flt, attrTbl, by = 'itemId')
#view star_point and comment num by group
#meanwhile replace "star_mean" with "n" to see the item num of diff star_point
cat_star <- 
        cmt_attr %>%
        select(category,star) %>%
        group_by(category) %>%
        summarize(star_mean = mean(star), star_median = median(star), n = n()) %>%
        filter(!is.na(category))

        ggplot(data=cat_star, mapping=aes(x=category,y=star_mean))+
                geom_bar(stat="identity",width=0.5, color='black',fill='lightblue')+
                geom_text(aes(label=round(star_mean,2)), vjust=-0.3, color="black", size=3.5)+
                coord_cartesian(ylim = c(4.5,5))+
                theme(axis.line = element_line(size=1, colour = "black"),
                      axis.text.x = element_text(family = "myFont", vjust = 0.5, hjust = 0.5, angle = 45))        

tag_list_star <- 
        cmt_attr %>%
        select(itemTagList,star) %>%
        group_by(itemTagList) %>%
        summarize(star_mean = mean(star), star_median = median(star), n = n())%>%
        filter(!is.na(itemTagList))
        
        ggplot(data=tag_list_star, mapping=aes(x=itemTagList,y=star_mean))+
                geom_bar(stat="identity",width=0.5, color='black',fill='lightblue')+
                geom_text(aes(label=round(star_mean,2)), vjust=-0.3, color="black", size=3.5)+
                coord_cartesian(ylim = c(4.5,5))+
                theme(axis.line = element_line(size=1, colour = "black"),
                      axis.text.x = element_text(family = "myFont", vjust = 0.5, hjust = 0.5, angle = 45))        

tag_star <- 
        cmt_attr %>%
        select(promTag,star) %>%
        group_by(promTag) %>%
        summarize(star_mean = mean(star), star_median = median(star), n = n()) %>%
        filter(!is.na(promTag))
        
        ggplot(data=tag_star, mapping=aes(x=promTag,y=star_mean))+
                geom_bar(stat="identity",width=0.5, color='black',fill='lightblue')+
                geom_text(aes(label=round(star_mean,2)), vjust=-0.3, color="black", size=3.5)+
                coord_cartesian(ylim = c(4.5,5))+
                theme(axis.line = element_line(size=1, colour = "black"),
                      axis.text.x = element_text(family = "myFont", vjust = 0.5, hjust = 0.5, angle = 45))        

place_star <-
        cmt_attr %>%
        select(productPlace,star) %>%
        group_by(productPlace) %>%
        summarize(star_mean = mean(star), star_median = median(star), n = n()) %>%
        filter(!is.na(productPlace))
        
        ggplot(data=place_star, mapping=aes(x=productPlace,y=star_mean))+
                geom_bar(stat="identity",width=0.5, color='black',fill='lightblue')+
                geom_text(aes(label=round(star_mean,2)), vjust=-0.3, color="black", size=3.5)+
                coord_cartesian(ylim = c(4.8,5))+
                theme(axis.line = element_line(size=1, colour = "black"),
                      axis.text.x = element_text(family = "myFont", vjust = 0.5, hjust = 0.5, angle = 45))



