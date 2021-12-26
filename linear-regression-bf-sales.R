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
#import data set
bf <- read.csv("C:/Users/karan/OneDrive/Desktop/BlackFriday.csv")
bf
# Cast the data objects into a data frame
bf <- data.frame(bf)

# -----------------------------------------------------------------------
# 80-20 train and test
set.seed(42)
data(bf)
n = nrow(bf)
trainIndex <- round(nrow(bf) * .80)
train = bf[1:trainIndex ,]
test = bf[(trainIndex+1):n ,]
#------------------------------------------------------------
train$Gender<-as.factor(train$Gender)
train$Age<-as.factor(train$Age)
train$Occupation<-as.factor(train$Occupation)
train$City_Category<-as.factor(train$City_Category)
train$Marital_Status<-as.factor(train$Marital_Status)
train$Purchase<-as.numeric(train$Purchase)
train$basket<-as.factor(train$basket)
#----------------------------------------------------------
#Correlation
cor(train$Purchase,train$Gender)
#regression
fit1<-lm((train$Purchase)~train$Gender+train$Age+train$Occupation+train$City_Category+train$Stay_In_Current_City_Years+train$Marital_Status,data=train)
summary(fit1)
anova(fit1)

  
pe<-step(fit1,direction="both")
adjusted_fit=lm((train$Purchase)~train$Gender+train$Occupation+train$City_Category,data=train)
summary(adjusted_fit)
mean((test$Purchase - predict.lm(adjusted_fit, test)) ^ 2)
#mean squared prediction error is very high, hence the model is not a good fit to the data

#----------------------------------------------------------
#regression (linear)
a<-lm(train$Purchase~train$Gender)
summary(a)

#-------------------------------------------------------
#Histogram of Purchase
hist(bf$Purchase,breaks=40, xlim=c(185,25000),col="darkorange", main="Purchase made in $ by shoppers(Breaks=40)", xlab="Amount", ylab="Frequency of people")

#Histogram of Purchase Column by Gender
ggplot(bf, aes(x = bf$Purchase)) +
  geom_histogram(bins = 75) +
  facet_grid(. ~ bf$Gender) +
  labs(title= "Purchases Histogram by Gender")

#=----------------------------------
ggplot(bf, aes(x = bf$Purchase)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ bf$Age) +
  labs(title= "Purchases Histogram by Age") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#-----------------------------------------------
ggplot(bf, aes(x = bf$basket)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ bf$basket) +
  labs(title= "Purchases Histogram by basket") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(train, aes(x = train$Purchase)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ train$Product_Category_2) +
  labs(title= "Purchases Histogram by Product Category_2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(train, aes(x = train$Purchase)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ train$Product_Category_3) +
  labs(title= "Purchases Histogram by Product Category_3") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


model<-lm(bf$Purchase~. -bf$User_ID -bf$Product_ID, data=bf)
summary(model)
#as we can see marital status is insignificant
model1<-lm(bf$Purchase~. -bf$User_ID -bf$Product_ID -bf$Marital_Status, data=bf)
summary(model1)

#Histogram of Purchase Column by City Category
ggplot(train, aes(x = train$Purchase)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ City_Category) +
  labs(title= "Purchases Histogram by City Category") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_histogram(fill='darkorange', color="black")

#Boxplot of Purchase Amounts by Age
ggplot(train, aes(x = bf$Product_Category_1, y = bf$Purchase)) + 
  geom_boxplot(stat="identity") + 
  ggtitle("Purchase amounts by Age")

#boxplot of Purchase Amounts by Gender
barplot(table(bf$Product_Category_1), col="darkorange", main="Black Friday Shoppers by Gender", xlab="Gender", ylab="Count")
ggplot(train, aes(x = Product_Category_1, y = Purchase)) + 
  geom_boxplot() + 
  ggtitle("Purchase Amounts by Gender")+geom_boxplot(fill='darkorange', color="black")


#Barplot of Black Friday by Age Range
barplot(table(train$Age), col="darkorange", main="Black Friday Shoppers by Age Range", xlab="Age Ranges", ylab="Count")

#print("RESULT : People withing range of 26-35 shopped most")
#print("While people in age-range 0-17 or 55+ shopped least and almost none compared to 26-35")
#print("Also, overall people within age range 18-45 are the group which makes maximum population of shooping")

#Histogram
hist(bf$Purchase,breaks=10, xlim=c(185,25000),col="darkorange", main="Purchase made in $ by shoppers(Breaks=10)", xlab="Amount", ylab="Frequency of people")


SalesRaw$Marital_Status <- as.factor(SalesRaw$Marital_Status)
#
ggplot(bf,aes(x = bf$basket)) + geom_bar(aes(fill = bf$Gender))


bf$Product_Category_1 <- as.factor(bf$Product_Category_1)
bf$Product_Category_2 <- as.factor(bf$Product_Category_2)
bf$Product_Category_3 <- as.factor(bf$Product_Category_3)
bf$Occupation <- as.factor(bf$Occupation)
bf$User_ID <- as.factor(bf$User_ID)
SalesRawSummary <- summary(bf)
SalesRawSummary

GenderAge <- xtabs(~bf$Gender + bf$Age )
GenderAge

prop.table(GenderAge,2)*100


chisqtestGenPurchase <- chisq.test(bf$Gender, bf$Purchase)
chisqtestGenPurchase

GenderOccupation <- xtabs(~bf$Gender + bf$Occupation )
GenderOccupation


round(prop.table(GenderOccupation,2)*100,2)


par(mfrow = c(3,2))
CitySales <- plot(bf$City_Category, bf$Purchase, xlab = "city")
OccupationSales <- plot(bf$Occupation, bf$Purchase, xlab = "Occupation")
GenderSales <- plot(bf$Gender, bf$Purchase, xlab = "Gender")
AgeSales <- plot(bf$Age, bf$Purchase , xlab = "Age")
StaySales <- plot(bf$Stay_In_Current_City_Years, bf$Purchase , xlab = "Stay Duration Years")
MartialstatusSales <- plot(bf$Marital_Status, bf$Purchase , xlab = "Marital Status")

age <- bf %>%
  group_by(Age) %>%
  summarise(Total=n())

#make a pie chart with plotly package
plot_ly(age, labels = ~Age, values = ~Total, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste(Total, 'People'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE) %>%
  layout(title = 'Age Distribution', titlefont = list(size = 18, color = 'black'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# 
data= bf %>% group_by(bf$basket,bf$Gender) %>% summarise(number = n())
data=data.frame(data) %>% filter(data$`bf$Gender` == 'F') 
data= data[with(data,order(-data$number)),] %>% head(data,n=10)
data
#basket_f<-ggplot(data=data, aes(x=data$bf.basket, y=data$number, fill=data$`bf$Gender`)) +
#  geom_bar(stat="identity",fill='darkorange', color="black")
#print(basket_f+ggtitle("Basket analysis for Females")+ coord_flip())

data$bf.basket <- factor(data$bf.basket, levels = data$bf.basket[order(data$number)])

basket_f_desc <- ggplot(data=data, aes(x=bf.basket, y=number)) +
  geom_bar(stat="identity",fill='darkorange', color="black") 
print(basket_f_desc +ggtitle("Basket analysis for Females")+ coord_flip())

data_M= bf %>% group_by(bf$basket,bf$Gender) %>% summarise(number = n()) 
data_M=data.frame(data_M) %>% filter(data_M$`bf$Gender` == 'M') 
data_M= data_M[with(data_M,order(-data_M$number)),] %>% head(data_M,n=10)
data_M

data_M$bf.basket <- factor(data_M$bf.basket, levels = data_M$bf.basket[order(data_M$number)])

basket_m_desc <- ggplot(data=data_M, aes(x=bf.basket, y=number)) +
  geom_bar(stat="identity",fill='darkorange', color="black") 
print(basket_m_desc +ggtitle("Basket analysis for Males")+ coord_flip())
summary(bf)
data_age= bf %>% group_by(bf$Age,bf$basket) %>% summarise(number = n())%>% 
data_age_0_17 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "0-17")
data_age_0_17= data_age_0_17[with(data_age_0_17,order(-data_age_0_17$number)),] %>% head(data_age_0_17,n=5)
data_age_18_25 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "18-25")
data_age_18_25= data_age_18_25[with(data_age_18_25,order(-data_age_18_25$number)),] %>% head(data_age_18_25,n=5)
data_age_26_35 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "26-35")
data_age_26_35= data_age_26_35[with(data_age_26_35,order(-data_age_26_35$number)),] %>% head(data_age_26_35,n=5)
data_age_36_45 =  data.frame(data_age=) %>% filter(data_age$`bf$Age` == "36-45")
data_age_36_45= data_age_36_45[with(data_age_36_45,order(-data_age_36_45$number)),] %>% head(data_age_36_45,n=5)
data_age_46_50 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "46-50")
data_age_46_50= data_age_46_50[with(data_age_46_50,order(-data_age_46_50$number)),] %>% head(data_age_46_50,n=5)
data_age_51_55 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "51-55")
data_age_51_55= data_age_51_55[with(data_age_51_55,order(-data_age_51_55$number)),] %>% head(data_age_51_55,n=5)
data_age_56 =  data.frame(data_age) %>% filter(data_age$`bf$Age` == "55+")
data_age_56= data_age_56[with(data_age_56,order(-data_age_56$number)),] %>% head(data_age_56,n=5)

data_age_36_45 =  data.frame(bf) %>% filter(bf$Age == "51-55")
count(data_age_36_45)
#0-17
data_age_0_17$bf.basket <- factor(data_age_0_17$bf.basket, levels = data_age_0_17$bf.basket[order(data_age_0_17$number)])

basket_age_0_17<-ggplot(data=data_age_0_17, aes(x=data_age_0_17$bf.basket, y=data_age_0_17$number, fill=data_age_0_17$bf.Age)) +
  geom_bar(stat="identity",fill='darkorange', color="black")
print(basket_age_0_17+ggtitle("Basket analysis for 0 to 17 age group")+ coord_flip())

#18-25
data_age_18_25$bf.basket <- factor(data_age_18_25$bf.basket, levels = data_age_18_25$bf.basket[order(data_age_18_25$number)])

basket_age_18_25<-ggplot(data=data_age_18_25, aes(x=data_age_18_25$bf.basket, y=data_age_18_25$number, fill=data_age_18_25$bf.Age)) +
  geom_bar(stat="identity",fill='darkorange', color="black")
print(basket_age_18_25+ggtitle("Basket analysis for 18 to 25 age group")+ coord_flip())

#26-35
data_age_26_35$bf.basket <- factor(data_age_26_35$bf.basket, levels = data_age_26_35$bf.basket[order(data_age_26_35$number)])

basket_age_26_35<-ggplot(data=data_age_26_35, aes(x=data_age_26_35$bf.basket, y=data_age_26_35$number, fill=data_age_26_35$bf.Age)) +
  geom_bar(stat="identity",fill='darkorange', color="black")
print(basket_age_26_35+ggtitle("Basket analysis for 26 to 35 age group")+ coord_flip())

#36-45
data_age_36_45$bf.basket <- factor(data_age_36_45$bf.basket, levels = data_age_36_45$bf.basket[order(data_age_36_45$number)])

basket_age_36_45<-ggplot(data=data_age_36_45, aes(x=data_age_36_45$bf.basket, y=data_age_36_45$number, fill=data_age_36_45$bf.Age)) +
  geom_bar(stat="identity",fill='darkorange', color="black")
print(basket_age_36_45+ggtitle("Basket analysis for 36 to 45 age group")+ coord_flip())

#46-50
data_age_46_50$bf.basket <- factor(data_age_46_50$bf.basket, levels = data_age_46_50$bf.basket[order(data_age_46_50$number)])

basket_age_46_50<-ggplot(data=data_age_46_50, aes(x=data_age_46_50$bf.basket, y=data_age_46_50$number, fill=data_age_46_50$bf.Age)) +
  geom_bar(stat="identity",fill='darkorange', color="black")
print(basket_age_36_45+ggtitle("Basket analysis for 46 to 50 age group")+ coord_flip())

#51-55
data_age_51_55$bf.basket <- factor(data_age_51_55$bf.basket, levels = data_age_51_55$bf.basket[order(data_age_51_55$number)])

basket_age_51_55<-ggplot(data=data_age_51_55, aes(x=data_age_51_55$bf.basket, y=data_age_51_55$number, fill=data_age_51_55$bf.Age)) +
  geom_bar(stat="identity",fill='darkorange', color="black")
print(basket_age_51_55+ggtitle("Basket analysis for 51 to 55 age group")+ coord_flip())

#55 or more
data_age_56$bf.basket <- factor(data_age_56$bf.basket, levels = data_age_56$bf.basket[order(data_age_56$number)])

basket_age_56<-ggplot(data=data_age_56, aes(x=data_age_56$bf.basket, y=data_age_56$number, fill=data_age_56$bf.Age)) +
  geom_bar(stat="identity",fill='darkorange', color="black")
print(basket_age_56+ggtitle("Basket analysis for 56+ age group")+ coord_flip())

newdf <- rbind(df, de)

  data_age= data_age[with(data_age,order(data_age$bf.Age)),] %>% head(data_M,n=10)
data_age

data1=data.frame(aggregate(bf$Purchase, by=list(bf$Gender), FUN=sum))
library(corrplot)
data <- train[, c(as.factor(3),as.factor(4))]
res <- cor(data, use = "complete.obs")
res <- round(res, 2)
corrplot(res, tl.col = 'black', tl.cex = .7, tl.srt = 45)


###########################################################################################################
############
########################
plot(aggregate(bf$Purchase ~ bf$basket, bf, sum))
MaxBasket <- aggregate(bf$Purchase ~ bf$basket, bf, sum)
max(MaxBasket)
View(MaxBasket)                       

groupBasket <- data.frame(table(bf$basket))
View(groupBasket)
top10_basket <- top_n(groupBasket, n = 10)
top10_basket
top10_basket <- top10_basket[order(-Freq)]
colnames(top10_basket) <- c("Baskets", "Count")
View(top10_basket)
top10_basket$Count

fill <- c("#1F77B4", "#FF7F0E")
bp <- ggplot(top10_basket, aes(x = fct_rev(fct_infreq(Count)))) + 
  geom_bar(colour = "white") +
  theme_minimal()
bp <- bp + theme(
  axis.title.x = element_text(color = "black", size = 12, face = "bold"),
  axis.title.y = element_text(color = "black", size = 12, face = "bold"),
  axis.title = element_text(color="black", face="bold")
)+ labs(title = "Different Age Groups" , x = "Age Groups", y = "Number of Buyers") + coord_flip() + scale_fill_manual(values=fill)
bp
