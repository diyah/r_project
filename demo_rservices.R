# Credit Card Fraud Prediction

setwd('D:/PROJECTS/DEMO')


connStr <- "Driver={SQL Server};
Server=localhost;
Database=Demo;
Uid=sa;
Pwd=Passcode123"

# Define the type of the 
colClass <- c('gender'= "factor",
              'cardholder' = "numeric",
              'fraudRisk' = "factor",
              'balance' = "numeric",
              'numTrans' = "numeric",
              'numIntlTrans' = "numeric",
              'creditLine' = "numeric")

# Define the query
sqlquery <- 'SELECT [gender]
,[cardholder]
,[balance]
,[numTrans]
,[numIntlTrans]
,[fraudRisk]
FROM [Demo].[dbo].[ccFraudSmall]'


# Create R object that pointing to table on SQL Server
ccFraudsql <- RxSqlServerData(sqlQuery = sqlquery,
                              connectionString = connStr,
                              stringsAsFactors = TRUE,
                              rowsPerRead = 5000
)

# Import data from SQL Server
ccDataset <- rxImport(ccFraudsql,colClasses = colClass)
rxGetInfo(ccDataset, getVarInfo = TRUE, numRows = 5)


# Modeling
nnModel <- rxNeuralNet(fraudRisk ~ gender+cardholder+balance+numTrans+numIntlTrans,
                       data = ccDataset,
                       type = 'binary',
                       numHiddenNodes = 3
)


# Validation and accuracy 
prediction = rxPredict(modelObject = nnModel, 
                       data = ccDataset, 
                       writeModelVars = TRUE, 
                       overwrite = TRUE)

pred =as.data.frame(cbind(actual = as.numeric(prediction$fraudRisk),
                          prediction =  as.numeric(prediction$PredictedLabel),
                          probs = prediction$Probability.1))

results <- table(prediction$PredictedLabel,prediction$fraudRisk)


# Operationalize to web service
library(mrsdeploy)

ccFraudOp <- function(gender , cardholder , balance , numTrans, numIntlTrans, fraudRisk){
  newdata <- data.frame(gender = gender, cardholder = cardholder , balance = balance,numTrans = numTrans, numIntlTrans = numIntlTrans, fraudRisk = fraudRisk )
  rxPredict(nnModel, newdata, extraVarsToWrite = "fraudRisk")
}

print(ccFraudOp('1',2,3000,3,1,0)$PredictedLabel)

remoteLogin("http://localhost:12800", 
            username = "admin", 
            password = "P@ssw0rdsusah123",
            session = FALSE)

serviceName <- paste0("mtService", round(as.numeric(Sys.time()), 0))

# Publish as service using publishService() from
# mrsdeploy package. Name service "mtService" and provide
# unique version number. Assign service to the variable 'api'

api <- publishService(
  serviceName,
  code = ccFraudOp,
  model = nnModel,
  inputs = list(gender = "character",
                cardholder = "numeric",
                balance = "numeric",
                numTrans = "numeric",
                numIntlTrans = "numeric",
                fraudRisk = "numeric"
  ),
  outputs = list(answer = "data.frame"),
  v = "v1.0.0"
)

# Consume Service in R

# Print capabilities that define the service holdings: service
# name, version, descriptions, inputs, outputs, and the
# name of the function to be consumed

print(api$capabilities())
# Consume service by calling function, 'ccFraudOp'
# contained in this service
results <- api$ccFraudOp('1', 2, 300, 15, 3, 1)
print(results$output("answer")$PredictedLabel)

