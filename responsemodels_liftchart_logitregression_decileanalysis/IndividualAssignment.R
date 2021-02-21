mydata <- read.csv("book_transaction.csv")
head(mydata)
typeof(mydata$DATE)
mydata$DATE <- as.Date(mydata$DATE,format = '%m/%d/%Y')
mydata
calibration_sample <- mydata[which(mydata$DATE <= as.Date("2017-09-30"),),]
View(calibration_sample)
validation_sample <- mydata[which(mydata$DATE > as.Date("2017-09-30"),),]
View(validation_sample)

#Step 2: For both samples restructure the data so that there is 
#one row per customer with the following columns: 
#“ID”, “frequency” (number of purchases), 
#“monetary” (average dollar value of purchases), 
#“last.purchase” (the date of the customer’s most recent purchase)
library(data.table)
calibration_table <- data.table(calibration_sample)
calibration_table[ , c("frequency", "monetary", "last.purchase") := list(sum(BOOKS), mean(DOLLARS), max(DATE)), by = ID][]
calibration_table[ , c("BOOKS", "DOLLARS", "DATE"):=list(NULL, NULL, NULL)][]

validation_table <- data.table(validation_sample)
validation_table[ , c("frequency", "monetary", "last.purchase") := list(sum(BOOKS), mean(DOLLARS), max(DATE)), by = ID][]
validation_table[ , c("BOOKS", "DOLLARS", "DATE"):=list(NULL, NULL, NULL)][]


#Step 3: For the calibration sample, 
#create a column called “recency” which is a numeric variable that represents how many days since the most recent purchase. 
#A good way to do this is to use the last date in the whole calibration sample as an end date. 
#So a purchase on that date would have a recency score of 0. Repeat this step for the validation sample.
Cal_lastDate <- max(calibration_table$last.purchase)
calibration_table[, recency:=(Cal_lastDate-last.purchase)]
calibration_tableUnique <- unique(calibration_table)
data_calibration <- data.frame(unique(calibration_table))

Val_lastDate <- max(validation_table$last.purchase)
validation_table[, recency:=(Val_lastDate-last.purchase)]
validation_tableUnique <- unique(validation_table)
data_validation <- data.frame(unique(validation_table))

#Step 4: Merge the calibration and validation samples into one data frame with one row per customer ID. 
Merged_table <- merge(calibration_tableUnique, validation_tableUnique, all=TRUE, by="ID", suffix=c("_cal", "_val"))

#Step 5: Create a column that takes value 1 if the customer was retained 
#(i.e. if the customer made at least one purchase in the validation period) and a 0 if not. 
Merged_table[, "retained":=ifelse(is.na(frequency_val), 0, 1)]

View(Merged_table)
#View(Merged_table[1:20,])

new.linearfit <-lm(retained ~ recency_cal + frequency_cal + monetary_cal, data=Merged_table)
summary(new.linearfit)
Merged_table[, linearfit:=predict(new.linearfit, type="response")]

hist(Merged_table$linearfit)



monetaryprediction = expand.grid(recency_val=mean(Merged_table$recency_val),
                                 frequency_val=mean(Merged_table$frequency_val),
                                 monetary_val=seq(min(Merged_table$monetary_val),max(Merged_table$monetary_val)))
monetaryprediction$linearfit <- predict(new.linearfit, monetaryprediction,type="response")
plot(monetaryprediction$monetary_val,monetaryprediction$linearfit,col=3)
