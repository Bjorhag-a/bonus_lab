#' @title Reference Class representing a Ridge Linear regression using least square 
#' 
#' @description ridgereg is used to fit linear models with a penalty lambda and
#'   using ordinary linear algebra to calculate the Regressions coefficients and
#'   fitted values
#' 
#' 
#' @param formula a formula object.
#' @param data a data frame.
#' @param lambda a numeric number.
#' 
#' @returns an object of class `ridgereg`
#' 
#' @examples
#' # data(iris)
#' # ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 0.5)
#' # l$coef()
#' 
#'  
#' @import ggplot2
#' @import methods
#'
#' @export
ridgereg <- setRefClass("ridgereg", 
                        fields = list(
                          formula = "formula",
                          data_name = "name",
                          data = "data.frame",
                          lambda = "numeric",
                          y = "numeric",
                          beta = "array",
                          y_hat = "array",
                          X = "array",
                          x_norm = "array"
                          
                        ),
                        methods = list(
                          initialize = function(formula, data, lambda){
                            # save the name of the dataset for later usage
                            .self$data_name <<- substitute(data)
                            
                            .self$formula <<- formula
                            .self$lambda <<- lambda
                            
                            .self$data <<- data
                            
                            X <<- model.matrix(formula, data)
                            .self$x_norm <<- cbind(X[, 1], scale(X[, -1]))
                            
                            .self$y <<- data[[all.vars(formula)[1]]]
                            lambda_I <- diag(lambda, ncol(.self$x_norm))
                            
                            
                            
                            .self$beta <<- solve(t(.self$x_norm) %*% .self$x_norm + lambda_I) %*% t(.self$x_norm) %*% .self$y
                            .self$y_hat <<- .self$x_norm %*% .self$beta
                            
                            
                          },
                          show = function() {
                            cat("Call:")
                            cat("\n")
                            
                            # format call output
                            cat(paste("ridgereg(formula = ", deparse(.self$formula), ", data = ", .self$data_name, ", lambda = ", .self$lambda, ")",  sep = ""))
                            cat("\n")
                            
                            cat("Coefficients:")
                            cat("\n")
                            
                            col_names <- c("(Intercept)", all.vars(.self$formula)[-1])
                            max_length <- max(nchar(col_names))
                            
                            # format output for table shape
                            line1 <- paste(sprintf(paste0("%-", max_length, "s"), col_names), collapse = " ")
                            line2 <- paste(sprintf(paste0("%-", max_length, "s"), format(.self$beta, digits = 3)), collapse = " ")
                            
                            cat(line1)
                            cat("\n")
                            cat(line2)
                            
                            
                            
                          },
                          predict = function(newdata = NULL){
                            if (is.null(newdata)) {
                              return(.self$y_hat)
                              
                            }
                            else{
                              standard_dev <- apply(X[, -1], 2, sd)
                              normalized_new_data <- scale(newdata, colMeans(X[, -1]), standard_dev)
                              prediction <- as.matrix(cbind(1, normalized_new_data)) %*% .self$beta
                              
                              return(prediction)
                            }
                            
                          },
                          coef = function(){
                            return(.self$beta)
                          }
                          
                        ))
ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 0.5)
print(ridgereg_mod)
ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 0.5)
# class(ridgereg_mod)[1] == "ridgereg"
ridgereg_mod$predict()
#ridgereg_mod$print()
ridgereg_mod$coef()
