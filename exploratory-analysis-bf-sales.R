# First regression in R
# Author: Amit Gunnika Karan Ravinder Sparsh
# Date: May 22, 2019

# -----------------------------------------------------------------------
# Install relevant libraries
install.packages("ggplot2",dependencies = TRUE)
install.packages("evaluate")
install.packages("magrittr") # only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
install.packages("plotly")
install.packages("caret")  # cross validation
update.packages("ggplot2")
install.packages("ggplot")
install.packages("DataExplorer")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("forcats")
install.packages("plyr")
install.packages("tidyverse")
install.packages("scales")
# -----------------------------------------------------------------------
# Load in relevant libraries
library(plotly)
library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr) 
library(ggplot)# alternative, this also loads %>%
library(ggplot2)
library(evaluate)
library(digest)
library(DataExplorer)
library(rpart)
library(caret)
library(dplyr)
library(ggpubr)
library(forcats)
library(plyr)
library(tidyverse)
library(scales)
# -----------------------------------------------------------------------
# Load in the data
bf <- read.csv("C:/Users/karan/OneDrive/Desktop/BlackFriday.csv")
# -----------------------------------------------------------------------
# Clean the data
# Cast the data objects into a data frame
bf <- data.frame(bf)
# ------------------------------------------------------------------------
# Confirm the column names
colnames(bf)
str(bf)
levels(bf$Gender)  # for a categorical variable it will give factor
levels(bf$Age)
levels(bf$City_Category)
levels(bf$Occupation)
levels(bf$Stay_In_Current_City_Years)
levels(bf$Marital_Status)

bf$Occupation <- as.factor(bf$Occupation) #confirm categorical
bf$Marital_Status <- as.factor(bf$Marital_Status)

bf$Purchase <- as.numeric(bf$Purchase) #confirm numeric

str(bf)
# -----------------------------------------------------------------------
# Review the data
View(bf)
summary(bf) #Show point summary
str(bf) #structure of the data and allows us to see data types
dim(bf) # Dimensions of data
class(bf) #Numeric, matrix, data frame

#-------------------------------------------------------
#Histogram of Purchase
bp <- ggplot(bf, aes(bf$Purchase)) + 
  geom_histogram(bins=40,colour = "white", fill='lightslateblue') +
  theme_minimal() +
  theme(legend.position = "top")
bp <- bp + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="black", face="bold")
) + labs(title = "Purchase Amount Distribution" , x = "Amount Spent in Purchase(in $)", y = "Number of Buyers") 
bp

#-------------------------------------------------------
#BarChart of Occupation 
fill <- c("#1F77B4", "#FF7F0E")
bp <- ggplot(bf, aes(x = fct_infreq(Occupation), fill=Gender)) + 
  geom_bar(colour = "white") +
  theme_minimal()
bp <- bp + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="black", face="bold")
) + labs(title = "Occupation Categories " , x = "Occupation", y = "Number of Buyers") + scale_fill_manual(values=fill)
bp

#-------------------------------------------------------
#PieChart of Gender 
t <- list(size = 20)
bp <- plot_ly(bf, labels = bf$Gender, values = bf$Purchase, type = 'pie')%>%
  layout(title = 'Purchase Distribution by Gender',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         font=t)
bp

#-------------------------------------------------------
#BarPlot of Age 
fill <- c("#1F77B4", "#FF7F0E")
bp <- ggplot(bf, aes(x = fct_rev(fct_infreq(Age)),fill=Gender)) + 
  geom_bar(colour = "white") +
  theme_minimal()
bp <- bp + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="black", face="bold")
)+ labs(title = "Different Age Groups" , x = "Age Groups", y = "Number of Buyers") + coord_flip() + scale_fill_manual(values=fill)
bp

#-------------------------------------------------------
#BarChart of Marital Status 
fill <- c("#1F77B4", "#FF7F0E")
bp <- ggplot(bf, aes(bf$Marital_Status, fill=Marital_Status)) + 
  geom_bar(colour = "white") +
  theme_minimal()
bp <- bp + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="black", face="bold")
) + labs(title = "Distribution of Marriage" , x = "Marital Status", y = "Number of Buyers") + scale_fill_manual(values=fill)
bp

#--------------------------------------------------------
#Analysis Status and Purchase 
fill <- c("#1F77B4", "#FF7F0E")
bp <- ggplot(bf, aes(Marital_Status, Purchase)) + 
  geom_bar(colour = "white", stat = "identity") +
  theme_minimal()
bp <- bp + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="black", face="bold")
) + labs(title = "Occupation Categories " , x = "Occupation", y = "Number of Buyers") + scale_fill_manual(values=fill)
bp
#-------------------------------------------------------
#PieChart of City Category 
t <- list(size = 20)
bp <- plot_ly(bf, labels = bf$City_Category, values = bf$Purchase, type = 'pie')  %>%
  layout(title = 'Purchase Distribution by Cities',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         font=t)
bp

#-------------------------------------------------------
#BarChart of City Category 
fill <- c("#1F77B4", "#FF7F0E")
bp <- ggplot(bf, aes(bf$City_Category, fill=Gender)) + 
  geom_bar(colour = "white") +
  theme_minimal()
bp <- bp +  theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="black", face="bold")) +
labs(title = "Distribution by City Categories " , x = "City Category", y = "Number of Buyers") + scale_fill_manual(values=fill)
bp


#-------------------------------------------------------
#BarChart of Stay in City
fill <- c("#1F77B4", "#FF7F0E")
bp <- ggplot(bf, aes(bf$Stay_In_Current_City_Years, fill=Gender)) + 
  geom_bar(colour = "white") +
  theme_minimal()
bp <- bp +  theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="black", face="bold")) +
  labs(title = "Distribution by No. of Years in City " , x = "Number of Years in City", y = "Number of Buyers") + scale_fill_manual(values=fill)
bp


# install.packages("lattice")
# library(lattice)
# barchart(bf$Purchase~ factor(bf$Marital_Status), data = bf, origin = 0,
#          groups = bf$Marital_Status, auto.key = TRUE)
# ------------------------------------------------------------------------
# Basket Analysis Number
data_basket= bf %>% group_by(bf$basket) %>% summarise(number = n())
data_basket= data_basket[with(data_basket,order(-data_basket$number)),] %>% head(data_basket,n=10)
data_basket
bp <- ggplot(data_basket, aes(x = reorder(data_basket$`bf$basket`, data_basket$number),y = data_basket$number)) + 
  geom_bar(colour = "white", stat="identity", fill = "#FF7F0E") +
  theme_minimal()
bp <- bp + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="black", face="bold")
)+ labs(title = "" , x = "Baskets", y = "Number of Buyers") + coord_flip()
bp

data_basket$percent <- ((data_basket$number/537578)*100)


# ------------------------------------------------------------------------
# Basket Analysis Purhcase

data_purchase <- bf %>% group_by(basket) %>% summarise(purchase = sum(Purchase))
data_purchase
data_purchase= data_purchase[with(data_purchase,order(-data_purchase$purchase)),] %>% head(data_purchase,n=10)
data_purchase
bp <- ggplot(data_purchase, aes(x = reorder(data_purchase$basket, data_purchase$purchase),y = data_purchase$purchase)) + 
  geom_bar(colour = "white", stat="identity", fill="#1F77B4") +
  theme_minimal()
bp <- bp + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="black", face="bold")
)+ labs(title = "" , x = "Baskets", y = "Purhcase (in $)") + coord_flip() 
bp

data_purchase$percent <- ((data_purchase$purchase/5017668378)*100)
data_purchase

# ------------------------------------------------------------------------
#COUNT
# ------------------------------------------------------------------------
# Basket Analysis Female
dataF= bf %>% group_by(bf$basket,bf$Gender) %>% summarise(number = n())
dataF=data.frame(dataF) %>% filter(dataF$`bf$Gender` == 'F') 
dataF= dataF[with(dataF,order(-dataF$number)),] %>% head(dataF,n=5)
dataF
dataF$bf.basket <- factor(dataF$bf.basket, levels = dataF$bf.basket[order(dataF$number)])

basket_f_desc <- ggplot(data=dataF, aes(x=bf.basket, y=number)) +
  geom_bar(stat="identity",fill = "#FF7F0E", color="white") 
basket_f_desc <- basket_f_desc + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="black", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
  
)+ labs(title = "" , x = "Baskets", y = "Number of Buyers") + coord_flip() 
basket_f_desc


# ------------------------------------------------------------------------
# Basket Analysis Male
data_M= bf %>% group_by(bf$basket,bf$Gender) %>% summarise(number = n()) 
data_M=data.frame(data_M) %>% filter(data_M$`bf$Gender` == 'M') 
data_M= data_M[with(data_M,order(-data_M$number)),] %>% head(data_M,n=5)
data_M
data_M$bf.basket <- factor(data_M$bf.basket, levels = data_M$bf.basket[order(data_M$number)])


basket_m_desc <- ggplot(data=data_M, aes(x=bf.basket, y=number)) +
  geom_bar(stat="identity",fill="#FF7F0E", color="white") 

basket_m_desc <- basket_m_desc + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Number of Buyers") + coord_flip() 
basket_m_desc


# ------------------------------------------------------------------------
#PURCHASE
# ------------------------------------------------------------------------
# Basket Analysis Female
dataF= bf %>% group_by(bf$basket,bf$Gender) %>% summarise(number = sum(Purchase))
dataF=data.frame(dataF) %>% filter(dataF$`bf$Gender` == 'F') 
dataF= dataF[with(dataF,order(-dataF$number)),] %>% head(dataF,n=5)
dataF
dataF$bf.basket <- factor(dataF$bf.basket, levels = dataF$bf.basket[order(dataF$number)])

basket_f_desc <- ggplot(data=dataF, aes(x=bf.basket, y=number)) +
  geom_bar(stat="identity",fill = "#1F77B4", color="white") 
basket_f_desc <- basket_f_desc + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="black", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Purhcase (in $)") + coord_flip() 
basket_f_desc


# ------------------------------------------------------------------------
# Basket Analysis Male
data_M= bf %>% group_by(bf$basket,bf$Gender) %>% summarise(number = sum(Purchase)) 
data_M=data.frame(data_M) %>% filter(data_M$`bf$Gender` == 'M') 
data_M= data_M[with(data_M,order(-data_M$number)),] %>% head(data_M,n=5)
data_M
data_M$bf.basket <- factor(data_M$bf.basket, levels = data_M$bf.basket[order(data_M$number)])


basket_m_desc <- ggplot(data=data_M, aes(x=bf.basket, y=number)) +
  geom_bar(stat="identity",fill="#1F77B4", color="white") 

basket_m_desc <- basket_m_desc + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Purhcase (in $)") + coord_flip() 
basket_m_desc








# ------------------------------------------------------------------------
# Basket Analysis Age group

data_age= bf %>% group_by(bf$Age,bf$basket) %>% summarise(number = n())%>%
# ------------------------------------------------------------------------  
# Basket Analysis Age group 0-17
data_age_0_17 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "0-17")
data_age_0_17= data_age_0_17[with(data_age_0_17,order(-data_age_0_17$number)),] %>% head(data_age_0_17,n=5)
data_age_0_17$bf.basket <- factor(data_age_0_17$bf.basket, levels = data_age_0_17$bf.basket[order(data_age_0_17$number)])
data_age_0_17
basket_age_0_17<-ggplot(data=data_age_0_17, aes(x=data_age_0_17$bf.basket, y=data_age_0_17$number, fill=data_age_0_17$bf.Age)) +
  geom_bar(stat="identity",fill="#FF7F0E", color="white")
basket_age_0_17 <- basket_age_0_17 + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Number of Buyers") + coord_flip() 
basket_age_0_17

#print(basket_age_0_17+ggtitle("Basket analysis for 0 to 17 age group")+ coord_flip())
# ------------------------------------------------------------------------
# Basket Analysis Age group 18-25
data_age_18_25 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "18-25")
data_age_18_25= data_age_18_25[with(data_age_18_25,order(-data_age_18_25$number)),] %>% head(data_age_18_25,n=5)
data_age_18_25$bf.basket <- factor(data_age_18_25$bf.basket, levels = data_age_18_25$bf.basket[order(data_age_18_25$number)])
data_age_18_25
basket_age_18_25<-ggplot(data=data_age_18_25, aes(x=data_age_18_25$bf.basket, y=data_age_18_25$number, fill=data_age_18_25$bf.Age)) +
  geom_bar(stat="identity",fill="#FF7F0E", color="white")
basket_age_18_25 <- basket_age_18_25 + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Number of Buyers") + coord_flip() 
basket_age_18_25



# ------------------------------------------------------------------------
# Basket Analysis Age group 26-35
data_age_26_35 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "26-35")
data_age_26_35= data_age_26_35[with(data_age_26_35,order(-data_age_26_35$number)),] %>% head(data_age_26_35,n=5)
data_age_26_35$bf.basket <- factor(data_age_26_35$bf.basket, levels = data_age_26_35$bf.basket[order(data_age_26_35$number)])
data_age_26_35
basket_age_26_35<-ggplot(data=data_age_26_35, aes(x=data_age_26_35$bf.basket, y=data_age_26_35$number, fill=data_age_26_35$bf.Age)) +
  geom_bar(stat="identity",fill="#FF7F0E", color="white")
basket_age_26_35 <- basket_age_26_35 + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Number of Buyers") + coord_flip() 
basket_age_26_35


# ------------------------------------------------------------------------
# Basket Analysis Age group 36-45
data_age_36_45 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "36-45")
data_age_36_45= data_age_36_45[with(data_age_36_45,order(-data_age_36_45$number)),] %>% head(data_age_36_45,n=5)
data_age_36_45$bf.basket <- factor(data_age_36_45$bf.basket, levels = data_age_36_45$bf.basket[order(data_age_36_45$number)])
data_age_36_45
basket_age_36_45<-ggplot(data=data_age_36_45, aes(x=data_age_36_45$bf.basket, y=data_age_36_45$number, fill=data_age_36_45$bf.Age)) +
  geom_bar(stat="identity",fill="#FF7F0E", color="white")
basket_age_36_45 <- basket_age_36_45 + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Number of Buyers") + coord_flip() 
basket_age_36_45
# ------------------------------------------------------------------------
# Basket Analysis Age group 46-50
data_age_46_50 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "46-50")
data_age_46_50= data_age_46_50[with(data_age_46_50,order(-data_age_46_50$number)),] %>% head(data_age_46_50,n=5)
data_age_46_50$bf.basket <- factor(data_age_46_50$bf.basket, levels = data_age_46_50$bf.basket[order(data_age_46_50$number)])
data_age_46_50
basket_age_46_50<-ggplot(data=data_age_46_50, aes(x=data_age_46_50$bf.basket, y=data_age_46_50$number, fill=data_age_46_50$bf.Age)) +
  geom_bar(stat="identity",fill="#FF7F0E", color="white")
basket_age_46_50 <- basket_age_46_50 + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Number of Buyers") + coord_flip() 
basket_age_46_50
# ------------------------------------------------------------------------
# Basket Analysis Age group 51-55
data_age_51_55 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "51-55")
data_age_51_55= data_age_51_55[with(data_age_51_55,order(-data_age_51_55$number)),] %>% head(data_age_51_55,n=5)
data_age_51_55$bf.basket <- factor(data_age_51_55$bf.basket, levels = data_age_51_55$bf.basket[order(data_age_51_55$number)])
data_age_51_55
basket_age_51_55<-ggplot(data=data_age_51_55, aes(x=data_age_51_55$bf.basket, y=data_age_51_55$number, fill=data_age_51_55$bf.Age)) +
  geom_bar(stat="identity",fill="#FF7F0E", color="white")
basket_age_51_55 <- basket_age_51_55 + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Number of Buyers") + coord_flip() 
basket_age_51_55
# ------------------------------------------------------------------------
# Basket Analysis Age group 55+
data_age_56 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "55+")
data_age_56= data_age_56[with(data_age_56,order(-data_age_56$number)),] %>% head(data_age_56,n=5) 
data_age_56$bf.basket <- factor(data_age_56$bf.basket, levels = data_age_56$bf.basket[order(data_age_56$number)])
data_age_56
basket_age_56<-ggplot(data=data_age_56, aes(x=data_age_56$bf.basket, y=data_age_56$number, fill=data_age_56$bf.Age)) +
  geom_bar(stat="identity",fill="#FF7F0E", color="white")
basket_age_56 <- basket_age_56 + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Number of Buyers") + coord_flip() 
basket_age_56
  







sum(Purchase)
#1F77B4
Purhcase (in $)






# ------------------------------------------------------------------------
# Basket Analysis Age group Purchase

data_age= bf %>% group_by(bf$Age,bf$basket) %>% summarise(number = sum(Purchase))%>%

# ------------------------------------------------------------------------  
# Basket Analysis Age group 0-17
data_age_0_17 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "0-17")
data_age_0_17= data_age_0_17[with(data_age_0_17,order(-data_age_0_17$number)),] %>% head(data_age_0_17,n=5)
data_age_0_17$bf.basket <- factor(data_age_0_17$bf.basket, levels = data_age_0_17$bf.basket[order(data_age_0_17$number)])
data_age_0_17
basket_age_0_17<-ggplot(data=data_age_0_17, aes(x=data_age_0_17$bf.basket, y=data_age_0_17$number, fill=data_age_0_17$bf.Age)) +
  geom_bar(stat="identity",fill="#1F77B4", color="white")
basket_age_0_17 <- basket_age_0_17 + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Purhcase (in $)") + coord_flip() 
basket_age_0_17

#print(basket_age_0_17+ggtitle("Basket analysis for 0 to 17 age group")+ coord_flip())
# ------------------------------------------------------------------------
# Basket Analysis Age group 18-25
data_age_18_25 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "18-25")
data_age_18_25= data_age_18_25[with(data_age_18_25,order(-data_age_18_25$number)),] %>% head(data_age_18_25,n=5)
data_age_18_25$bf.basket <- factor(data_age_18_25$bf.basket, levels = data_age_18_25$bf.basket[order(data_age_18_25$number)])
data_age_18_25
basket_age_18_25<-ggplot(data=data_age_18_25, aes(x=data_age_18_25$bf.basket, y=data_age_18_25$number, fill=data_age_18_25$bf.Age)) +
  geom_bar(stat="identity",fill="#1F77B4", color="white")
basket_age_18_25 <- basket_age_18_25 + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Purhcase (in $)") + coord_flip() 
basket_age_18_25



# ------------------------------------------------------------------------
# Basket Analysis Age group 26-35
data_age_26_35 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "26-35")
data_age_26_35= data_age_26_35[with(data_age_26_35,order(-data_age_26_35$number)),] %>% head(data_age_26_35,n=5)
data_age_26_35$bf.basket <- factor(data_age_26_35$bf.basket, levels = data_age_26_35$bf.basket[order(data_age_26_35$number)])
data_age_26_35
basket_age_26_35<-ggplot(data=data_age_26_35, aes(x=data_age_26_35$bf.basket, y=data_age_26_35$number, fill=data_age_26_35$bf.Age)) +
  geom_bar(stat="identity",fill="#1F77B4", color="white")
basket_age_26_35 <- basket_age_26_35 + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Purhcase (in $)") + coord_flip() 
basket_age_26_35


# ------------------------------------------------------------------------
# Basket Analysis Age group 36-45
data_age_36_45 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "36-45")
data_age_36_45= data_age_36_45[with(data_age_36_45,order(-data_age_36_45$number)),] %>% head(data_age_36_45,n=5)
data_age_36_45$bf.basket <- factor(data_age_36_45$bf.basket, levels = data_age_36_45$bf.basket[order(data_age_36_45$number)])
data_age_36_45
basket_age_36_45<-ggplot(data=data_age_36_45, aes(x=data_age_36_45$bf.basket, y=data_age_36_45$number, fill=data_age_36_45$bf.Age)) +
  geom_bar(stat="identity",fill="#1F77B4", color="white")
basket_age_36_45 <- basket_age_36_45 + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Purhcase (in $)") + coord_flip() 
basket_age_36_45
# ------------------------------------------------------------------------
# Basket Analysis Age group 46-50
data_age_46_50 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "46-50")
data_age_46_50= data_age_46_50[with(data_age_46_50,order(-data_age_46_50$number)),] %>% head(data_age_46_50,n=5)
data_age_46_50$bf.basket <- factor(data_age_46_50$bf.basket, levels = data_age_46_50$bf.basket[order(data_age_46_50$number)])
data_age_46_50
basket_age_46_50<-ggplot(data=data_age_46_50, aes(x=data_age_46_50$bf.basket, y=data_age_46_50$number, fill=data_age_46_50$bf.Age)) +
  geom_bar(stat="identity",fill="#1F77B4", color="white")
basket_age_46_50 <- basket_age_46_50 + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Purhcase (in $)") + coord_flip() 
basket_age_46_50
# ------------------------------------------------------------------------
# Basket Analysis Age group 51-55
data_age_51_55 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "51-55")
data_age_51_55= data_age_51_55[with(data_age_51_55,order(-data_age_51_55$number)),] %>% head(data_age_51_55,n=5)
data_age_51_55$bf.basket <- factor(data_age_51_55$bf.basket, levels = data_age_51_55$bf.basket[order(data_age_51_55$number)])
data_age_51_55
basket_age_51_55<-ggplot(data=data_age_51_55, aes(x=data_age_51_55$bf.basket, y=data_age_51_55$number, fill=data_age_51_55$bf.Age)) +
  geom_bar(stat="identity",fill="#1F77B4", color="white")
basket_age_51_55 <- basket_age_51_55 + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Purhcase (in $)") + coord_flip() 
basket_age_51_55
# ------------------------------------------------------------------------
# Basket Analysis Age group 55+
data_age_56 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "55+")
data_age_56= data_age_56[with(data_age_56,order(-data_age_56$number)),] %>% head(data_age_56,n=5) 
data_age_56$bf.basket <- factor(data_age_56$bf.basket, levels = data_age_56$bf.basket[order(data_age_56$number)])
data_age_56
basket_age_56<-ggplot(data=data_age_56, aes(x=data_age_56$bf.basket, y=data_age_56$number, fill=data_age_56$bf.Age)) +
  geom_bar(stat="identity",fill="#1F77B4", color="white")
basket_age_56 <- basket_age_56 + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="white", face="bold"),
  axis.text.x = element_text(color = "black", size = 10, face = "bold"),
  axis.text.y = element_text(color = "black", size = 10, face = "bold"),
)+ labs(title = "" , x = "Baskets", y = "Purhcase (in $)") + coord_flip() 
basket_age_56

boxplot(bf$Purchase~bf$Product_Category_1, main="Boxplot", xlab="Product Category 1", ylab="Price", col="lightslateblue")
boxplot(bf$Purchase~bf$Product_Category_2, main="Boxplot", xlab="Product Category 2", ylab="Price", col="lightslateblue")
boxplot(bf$Purchase~bf$Product_Category_3, main="Boxplot", xlab="Product Category 3", ylab="Price", col="lightslateblue")




