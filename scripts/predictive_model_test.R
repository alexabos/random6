#----------------------
# 1. Open data
#
#
#----------------------


## Packages
library(data.table)
library(lubridate)

# Read
data <- as.data.table(read.table(paste0(getwd(),'/dades/train.txt'), header = T, sep = '|', stringsAsFactors = F))


## ----- FIRST ANALYSIS 
data[,uniqueN(ID_Customer)] == nrow(data) ## Not unique customers 
data[,uniqueN(Cod_Prod)] ## Total of 96 different products 

## How many products per customer
data[,uniqueN(Cod_Prod), by = ID_Customer][,summary(V1)] #From 1 product to a max of 32 per customer 
data[,uniqueN(Cod_Prod), by = ID_Customer][,hist(V1)]

## Extract data

# - Date
data[,year := unlist(strsplit(data[,Cod_Fecha],'-'))[seq(1,length(unlist(strsplit(data[,Cod_Fecha],'-'))),2)]]
data[,month := unlist(strsplit(data[,Cod_Fecha],'-'))[seq(2,length(unlist(strsplit(data[,Cod_Fecha],'-'))),2)]]

# - Other products bought before 
data[ID_Customer == 1, ][order(year,month)][,.(previous_product = c(NA,Reduce(paste, as.character(Cod_Prod[2:.N]), accumulate = TRUE)))]
data <- data[order(year,month,ID_Customer)]
data[,previous_product := c(NA,Reduce(paste, as.character(Cod_Prod[2:.N]), accumulate = TRUE)), by = ID_Customer]
data[,count_previous_product := cumsum(0:.N), by = ID_Customer]

# - Time between the first product 

# - Time between the last product

# - Product sold together 



## Convert to product bought YES/NO 
# Create a table with all products and customers 
products_bought <- data.table(expand.grid(Cod_Prod = data[,unique(Cod_Prod)], ID_Customer = data[,unique(ID_Customer)]))
## add if it was bough or not 
products_bought_by_customer <- data[,.(Cod_Prod,ID_Customer)][,bought := 1]
setkey(products_bought_by_customer,Cod_Prod,ID_Customer)
products_bought_by_customer[,id := products_bought_by_customer[,paste0(Cod_Prod,'_',ID_Customer)]]
# Duplicated
products_bought_by_customer <- products_bought_by_customer[!duplicated(id)]

## JOin 
products_bought <- products_bought_by_customer[products_bought, on = c('Cod_Prod','ID_Customer')]
products_bought[is.na(bought), bought := 0]

## Let see the dataset (super imbalaced dataset!!)
products_bought[,prop.table(table(bought))]

merged <- merge(products_bought,data[,.(ID_Customer,Socio_Demo_01,Socio_Demo_02,Socio_Demo_03,Socio_Demo_04,Socio_Demo_05)][!duplicated(ID_Customer)], by = 'ID_Customer', all = T)


## Try first simple model 
library(h2o)
h2o.init()

## Create partition 
library(caret)
train.samples <- createDataPartition(merged[,bought], p = 0.8)
train <- merged[train.samples$Resample1]
test <- merged[!train.samples$Resample1]

## Load h2o 
train.h2o <- LoadInH2o(train, factors = c('ID_Customer','Cod_Prod','bought'), destination_frame = 'data.train')
test.h2o <- LoadInH2o(test, factors = c('ID_Customer','Cod_Prod','bought'), destination_frame = 'data.test')

## First simple randomforest model 
predictors <- c('Socio_Demo_01','Socio_Demo_02','Socio_Demo_03','Socio_Demo_04','Socio_Demo_04','Cod_Prod')

# Balance parameter
balanced_param <- 1/merged[bought == 1, .N]/merged[bought == 0,.N]

rf_model <- h2o.randomForest(x = predictors, y = 'bought', training_frame = train.h2o, validation_frame = test.h2o, 
                             balance_classes = T, max_after_balance_size = 1)

## AUC 
h2o.auc(rf_model, valid = T)


