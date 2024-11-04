#' Title
#'
#' @field formula formula. 
#' @field data_name name. 
#' @field data data.frame. 
#' @field lambda numeric. 
#' @field y numeric. 
#' @field beta array. 
#' @field y_hat array. 
#' @field X array. 
#' @field x_norm array. 
#'
#' @return
#' @export
#'
#' @examples
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
                          print = function() {
                            cat("Call:")
                            cat("\n")
                            
                            # format call output
                            cat(paste("ridgereg(formula = ",deparse(.self$formula), ", data = ", .self$data_name, ", lambda = ", .self$lambda, ")",  sep = ""))
                            cat("\n")
                            
                            cat("Coefficients:")
                            cat("\n")
                            
                            col_names <- c("(Intercept)",all.vars(.self$formula)[-(1:2)])
                            max_length <- pmax(nchar(col_names))
                            
                            # format output for table shape
                            line1 <- paste(sprintf(paste0("%-", max_length, "s"), col_names), collapse = " ")
                            line2 <- paste(sprintf(paste0("%-", max_length, "s"), format(beta, digits = 3)), collapse = " ")
                            
                            cat(line1)
                            cat("\n")
                            cat(line2)
                            
                            
                            
                            
                          },
                          predict = function(newdata = NULL){
                            if (is.null(newdata)) {
                              return(.self$y_hat)
                              
                            }
                            else{
                              x_new <- model.matrix(.self$formula, newdata)
                              x_new_scale <- cbind(x_new[, 1], scale(x_new[, -1]))
                              
                              pred <- x_new_scale %*% .self$beta
                              return(pred)
                            }
                            
                          },
                          coef = function(){
                            return(.self$beta)
                          }
                          
                        ))


model <- ridgereg$new(Petal.Length~ Sepal.Length + Petal.Width , data = iris, lambda = 0.5)
model
model$coef()
model$print()
model$predict()

