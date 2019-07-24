library('VGAM')
classifier_regression_logistic <- function (data_train, data_test, true_class, false_class){
  
  class <- factor(data_train[,'class'])
  
  trained_model_regression_logistic <- glm(class ~ . ,family =  binomial("logit"), data = data_train)
  classification <- predict(trained_model_regression_logistic, data_test[,colnames(data_test) != 'class'], type="response")
  
  matrix_confusion = matrix(0, ncol=2, nrow=2)
  colnames(matrix_confusion) <- c("P", "N")
  row.names(matrix_confusion) <- c('V', 'F')
  
  for(i in 1:length(classification)){
    
    if(data_test[i,'class'] == true_class && classification[i] >= 0.5){
      matrix_confusion[1,1] <- matrix_confusion[1,1]+1
    }
    
    if(data_test[i,'class'] == false_class && classification[i] < 0.5){
      matrix_confusion[2,2] <- matrix_confusion[2,2]+1
    }
    
    if(data_test[i,'class'] == true_class && classification[i] < 0.5){
      matrix_confusion[1,2] <- matrix_confusion[1,2]+1
    }
    
    if(data_test[i,'class'] == false_class && classification[i] >= 0.5){
      matrix_confusion[2,1] <- matrix_confusion[2,1]+1
    }
  }
  
  return (matrix_confusion)
  
}

average_accuracy_performance_evaluation <- function (matrix_conf_k_folds) {
  
  accuracy <- 0
  k <- length(matrix_conf_k_folds)
  
  for(i in 1:length(matrix_conf_k_folds)){
    
    matrix_conf <- matrix_conf_k_folds[[i]]
    accuracy <- accuracy + ((matrix_conf[1,1]+matrix_conf[2,2])/(matrix_conf[1,1]+matrix_conf[1,2]+matrix_conf[2,1]+matrix_conf[2,2]))
    
  }
  
  return (accuracy/k)
}


average_balanced_accuracy_performance_evaluation <- function (matrix_conf_k_folds) {
  
  balanced_accuracy <- 0
  k <- length(matrix_conf_k_folds)
  
  for(i in 1:length(matrix_conf_k_folds)){
    
    matrix_conf <- matrix_conf_k_folds[[i]]
    aux1 = ((matrix_conf[1,1])/(matrix_conf[1,1]+matrix_conf[1,2]))
    aux2 = ((matrix_conf[2,1])/(matrix_conf[2,1]+matrix_conf[2,2]))
    balanced_accuracy <- balanced_accuracy + ((1/2)*(((matrix_conf[1,1])/(matrix_conf[1,1]+matrix_conf[1,2]))+((matrix_conf[2,1])/(matrix_conf[2,1]+matrix_conf[2,2]))))
    
  }
  
  return (balanced_accuracy/k)
}


average_sensitivity_performance_evaluation <- function (matrix_conf_k_folds) {
  
  sensitivity <- 0
  k <- length(matrix_conf_k_folds)
  
  for(i in 1:length(matrix_conf_k_folds)){
    
    matrix_conf <- matrix_conf_k_folds[[i]]
    sensitivity <- sensitivity + ((matrix_conf[1,1])/(matrix_conf[1,1]+matrix_conf[1,2]))
    
  }
  
  return (sensitivity/k)
}


average_specificity_performance_evaluation <- function (matrix_conf_k_folds) {
  
  specificity <- 0
  k <- length(matrix_conf_k_folds)
  
  for(i in 1:k){
    
    matrix_conf <- matrix_conf_k_folds[[i]]
    specificity <- specificity + ((matrix_conf[2,2])/(matrix_conf[2,1]+matrix_conf[2,2]))
    
  }
  
  return (specificity/k)
}


sd_accuracy_performance_evaluation <- function (matrix_conf_k_folds) {
  
  accuracy <- c()
  k <- length(matrix_conf_k_folds)
  
  for(i in 1:k){
    
    matrix_conf <- matrix_conf_k_folds[[i]]
    accuracy[i] <- ((matrix_conf[1,1]+matrix_conf[2,2])/(matrix_conf[1,1]+matrix_conf[1,2]+matrix_conf[2,1]+matrix_conf[2,2]))
    
  }
  
  return (sd(accuracy))
}


sd_balanced_accuracy_performance_evaluation <- function (matrix_conf_k_folds) {
  
  balanced_accuracy <- c()
  k <- length(matrix_conf_k_folds)
  
  for(i in 1:k){
    
    matrix_conf <- matrix_conf_k_folds[[i]]
    balanced_accuracy[i] <- (1/2)*((matrix_conf[1,1]/(matrix_conf[1,1]+matrix_conf[1,2]))+(matrix_conf[2,1]/(matrix_conf[2,1]+matrix_conf[2,2])))
    
  }
  
  return (sd(balanced_accuracy))
}


sd_sensitivity_performance_evaluation <- function (matrix_conf_k_folds) {
  
  sensitivity <- c()
  k <- length(matrix_conf_k_folds)
  
  for(i in 1:k){
    
    matrix_conf <- matrix_conf_k_folds[[i]]
    sensitivity[i] <- ((matrix_conf[1,1])/(matrix_conf[1,1]+matrix_conf[1,2]))
    
  }
  
  return (sd(sensitivity/k))
}


sd_specificity_performance_evaluation <- function (matrix_conf_k_folds) {
  
  specificity <- c()
  k <- length(matrix_conf_k_folds)
  
  for(i in 1:k){
    
    matrix_conf <- matrix_conf_k_folds[[i]]
    specificity[i] <- ((matrix_conf[2,2])/(matrix_conf[2,1]+matrix_conf[2,2]))
    
  }
  
  return (sd(specificity))
}


crossValidation_k_folds_logistic <- function(data, number_folds, t, f){
  
  number_rows_data <- nrow(data)
  number_cols_data <- ncol(data)
  number_rows_each_fold <- trunc(number_rows_data/number_folds)
  folds_test <- c()
  folds_train <- c()
  matrix_conf <- c()
  
  for (i in 1:number_folds){
    
    rows_test <- number_rows_each_fold*i
    init_rows_test <- ((rows_test-number_rows_each_fold)+1)
    folds_test <- data[init_rows_test:rows_test,]
    folds_train <- data[-(init_rows_test:rows_test),]
    matrix_conf[[i]] <- classifier_regression_logistic(data_train = folds_train, data_test = folds_test, t = t, f = f)
    
  }
  
  return (matrix_conf)  
  
} 


data <- read.csv('C:/Users/eduar/Downloads/DataSets/wdbc.data', header = TRUE, sep =",")
#cross-validation k-folds
data <- data[,2:32]
k<-10
matrix_conf <- crossValidation_k_folds_logistic(data = data, number_folds = k, t = 'M', f = 'B')
accuracy <- average_accuracy_performance_evaluation(matrix_conf)
sensitivity <- average_sensitivity_performance_evaluation(matrix_conf)
specificity <- average_specificity_performance_evaluation(matrix_conf)
balanced_accuracy <- average_balanced_accuracy_performance_evaluation(matrix_conf)
sd_accuracy <- sd_accuracy_performance_evaluation(matrix_conf)
sd_sensitivity <- sd_sensitivity_performance_evaluation(matrix_conf)
sd_specificity <- sd_specificity_performance_evaluation(matrix_conf)
sd_balanced_accuracy <- sd_balanced_accuracy_performance_evaluation(matrix_conf)




