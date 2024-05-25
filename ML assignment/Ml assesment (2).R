install.packages("tsDyn")
library(tsDyn)


test_data = tail(UoW_load, n =20)
train_data = head(UoW_load, n =480)


#https://stackoverflow.com/questions/23660518/neural-network-time-series-prediction-tsdyn-nnetts


test_data

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#When the data is not in a specific range it would be hard optimize the moidel
scaled_data <- as.data.frame(lapply(UoW_load[2:4], min_max_norm))
scaled_data <- cbind(scaled_data, UoW_load[c(4)])

names(scaled_data)[1] <- "var1"
names(scaled_data)[2] <- "var2"
names(scaled_data)[3] <- "scaled_pred"
names(scaled_data)[4] <- "Pred"

scaled_test_data = tail(scaled_data, n =20)
scaled_train_data = head(scaled_data, n =480)

install.packages("neuralnet")
library(neuralnet)

unnormalizing <- function(x, min, max) { 
  return( (max - min)*x + min )
}

min1 <- min(scaled_data[4])
max1 <- max(scaled_data[4])


#https://www.geeksforgeeks.org/how-neural-networks-are-used-for-regression-in-r-programming/

#Model 1
NN_model_1<- neuralnet(scaled_pred~var1+var2 ,hidden=c(3,4) , data = scaled_train_data 
                         ,linear.output=TRUE)
plot(NN_model_1)
#Evaluation model performance
model1Result <- predict(NN_model_1, scaled_test_data[1:2])
model1Result
renormormalized_prediction_value1 <- unnormalizing(model1Result, min1, max1)
renormormalized_prediction_value1 = unlist(as.list(renormormalized_prediction_value1),recursive=F)
renormormalized_prediction_value1

#Model 2
NN_model_2<- neuralnet(scaled_pred~var1+var2 ,hidden=c(10,30,10) , data = scaled_train_data 
                       ,linear.output=TRUE)
plot(NN_model_2)
#Evaluation model performance
model2Result <- predict(NN_model_2, scaled_test_data[1:2])
model2Result
renormormalized_prediction_value2 <- unnormalizing(model2Result, min1, max1)
renormormalized_prediction_value2 = unlist(as.list(renormormalized_prediction_value2),recursive=F)
renormormalized_prediction_value2



#Model 3
NN_model_3<- neuralnet(scaled_pred~var1+var2 ,hidden=c(10,50,25,10) , data = scaled_train_data 
                       ,linear.output=TRUE)
plot(NN_model_3)
#Evaluation model performance
model3Result <- predict(NN_model_3, scaled_test_data[1:2])
model3Result
renormormalized_prediction_value3 <- unnormalizing(model3Result, min1, max1)
renormormalized_prediction_value3 = unlist(as.list(renormormalized_prediction_value3),recursive=F)
renormormalized_prediction_value3




mod1.nnet<- nnetTs(scaled_train_data[c(3)],m=5, size=3,steps=30)
mod1.nnet
renormormalized_prediction_value4 <- unnormalizing(predict(mod1.nnet,steps=5,n.ahead=20), min1, max1)
renormormalized_prediction_value4 = unlist(as.list(renormormalized_prediction_value4),recursive=F)
renormormalized_prediction_value4
plot.ts(renormormalized_prediction_value4)
plot.ts(test_data[c(2)])

mod2.nnet<- nnetTs(scaled_train_data[c(3)], m = 4,  size=3,steps=20)
mod2.nnet
renormormalized_prediction_value5 <- unnormalizing(predict(mod2.nnet,steps=5,n.ahead=20), min1, max1)
renormormalized_prediction_value5 = unlist(as.list(renormormalized_prediction_value5),recursive=F)
renormormalized_prediction_value5
plot.ts(renormormalized_prediction_value5)


mod3.nnet<- nnetTs(scaled_train_data[c(3)], m = 5, size=8,steps=10)
mod3.nnet
renormormalized_prediction_value6 <- unnormalizing(predict(mod3.nnet,steps=5,n.ahead=20), min1, max1)
renormormalized_prediction_value6 = unlist(as.list(renormormalized_prediction_value6),recursive=F)
renormormalized_prediction_value6
plot.ts(renormormalized_prediction_value6)

install.packages("Metrics")
library("Metrics")


y_test = as.list(test_data[4])

#RMSE
rmse(list(renormormalized_prediction_value1),as.list(test_data[4]))
#MSE
MSE(list(renormormalized_prediction_value1),as.list(test_data[4]))
#MAPE
MAPE(list(renormormalized_prediction_value1),as.list(test_data[4]))



# Plot regression line
plot(test_data[4], renormormalized_prediction_value1, col = "red", 
     main = 'Real vs Predicted')
abline(0, 1, lwd = 2)


plot.ts(x = test_data[4] ,y = renormormalized_prediction_value1)
abline(0, 1, lwd = 2,col = "red")
