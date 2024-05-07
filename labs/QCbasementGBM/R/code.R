#' Gradient boosting
#'
#' Generates a gradient boosting model based on your choices of base learner and objective function
#' 
#' @param X                         A data frame representing the features. It is of size n x p. No need for an intercept column.
#' @param y                         A vector of length n. It either will be real numbers (for regression) or binary (for classification).
#' @param g_base_learner_alg        A function with arguments X, y and ... and returns a function that takes X as an argument. The default is YARFCART
#'                                  with nodesize 10\% of the total length.
#' @param neg_grad_objective_function   The negative gradient of the function to be minimized. It takes arguments y, yhat that returns a vector. The default objective function is SSE for
#'                                  regression and logistic loss for classification.
#' @param M                         The number of base learners to be summed. Default is 50 for regression and 100 for classification.
#' @param eta                       The step size in the gradient descent. Default is 0.3
#' @param verbose                   Messages are printed out during construction. Default is TRUE.
#' @param ...                       Optional arguments to be passed into the g_base_learner_alg function.
#'
#' @return                          A "qc_basement_gbm" gradient boosting model which can be used for prediction
#'
#' @author Adam Kapelner
#' @export
gbm_fit = function(X, y, g_base_learner_alg = NULL, neg_grad_objective_function = NULL, M = NULL, eta = 0.3, verbose = TRUE, ...){
  assert_data_frame(X)
  n = nrow(X)
  assert_numeric(y)
  assert(length(y) == n)
  assert_function(g_base_learner_alg, args = c("X", "y"), null.ok = TRUE)
  assert_function(neg_grad_objective_function, args = c("y", "yhat"), null.ok = TRUE)
  assert_count(M, positive = TRUE, null.ok = TRUE)
  assert_numeric(eta, lower = .Machine$double.eps)
  assert_logical(verbose)
  
  if (is.null(g_base_learner_alg)){
    g_base_learner_alg = function(X0, y0){
      YARFCART(X0, y0, nodesize = round(.1 * nrow(X0)), calculate_oob_error = FALSE, bootstrap_indices = list(1 : nrow(X0)), verbose = FALSE)
    }
  }
  
  if (identical(sort(names(table(y))), c("0", "1"))){
    #classification
    pred_type = "classification"
    if (is.null(M)){
      M = 100
    }
    if (is.null(neg_grad_objective_function)){
      neg_grad_objective_function = function(y, y_hat){
        y - exp(y_hat) / (1+exp(y_hat))
      }
    }
    g_0 = function(X_star){
      rep(exp(mean(y))/ (1 + exp(mean(y))), nrow(X_star)) # convert y_hat, which is in log_odds form, back to the probability  
    }
  } else {
    #regression
    pred_type = "regression"
    if (is.null(M)){
      M = 50
    }
    if (is.null(neg_grad_objective_function)){
      neg_grad_objective_function = function(y, y_hat){
        2 * (y - y_hat)
      }
    }
    g_0 = function(X_star){
      rep(mean(y), nrow(X_star))
    }
  }
  if (verbose){cat("building gradient boosted model for", pred_type, "\n")}
  
  g_tildes = list()
  g_tilde_yhats = matrix(NA, nrow = n, ncol = M + 1) 
  neg_gradient_ms = matrix(NA, nrow = n, ncol = M)
  for (m in 1 : M) {
    if (verbose){cat("fitting base learner", m, "of", M, "\n")}
    cum_y_hat_m = if (m == 1){
      g_tilde_yhat_m = g_0(X)
      g_tilde_yhat_m
    } else {
      g_tilde_yhat_m = predict(g_tildes[[m - 1]], X)
      cum_y_hat_m + eta * g_tilde_yhat_m
    }
    #cat("  cum_y_hat_m: ", head(cum_y_hat_m), "\n")
    neg_gradient_m = neg_grad_objective_function(y, cum_y_hat_m) # obtain negative gradient 
    #cat("    neg_gradient_m: ", head(neg_gradient_m), "\n")
    g_tildes[[m]] = g_base_learner_alg(X, neg_gradient_m)
    #cat("    g_tilde_yhat_m", head(g_tilde_yhat_m), "\n")
    
    neg_gradient_ms[, m] = neg_gradient_m
    g_tilde_yhats[, m] = g_tilde_yhat_m
  }
  g_tilde_yhats[, M + 1] = predict(g_tildes[[M]], X)
  
  gbm = list(
    pred_type = pred_type,
    g_0 = g_0, 
    g_tildes = g_tildes, 
    neg_gradient_ms = neg_gradient_ms,
    X = X, 
    y = y, 
    g_base_learner_alg = g_base_learner_alg, 
    neg_grad_objective_function = neg_grad_objective_function, 
    g_tilde_yhats = g_tilde_yhats,
    M = M, 
    eta = eta
  )
  class(gbm) = "qc_basement_gbm"
  gbm
}

#' Compute all iterative boosting predictions
#' 
#' Returns all predictions for each iteration of the gradient boosting
#'
#' @param gbm     A gradient boosting model of class "qc_basement_gbm"
#' @param X_star  The data to predict for (as a data frame). It has n_* rows and p columns
#'
#' @return        A matrix with n_* rows and M+1 columns where each column are the iterative
#'                predictions across all base learners beginning with g_0. For regression, the
#'                unit is in the units of the original response. For probability estimation for 
#'                binary response, the unit is the logit of the probability estimate.
#'
#' @author Adam Kapelner
#' @export
gbm_all_predictions = function(gbm, X_star){
  assert_class(gbm, "qc_basement_gbm")
  assert_data_frame(X_star)
  
  all_y_hat_star = matrix(NA, nrow = nrow(X_star), ncol = gbm$M + 1)
  all_y_hat_star[, 1] = gbm$g_0(X_star)
  for (m in 1 : gbm$M){
    all_y_hat_star[, m + 1] = all_y_hat_star[, m] + gbm$eta * predict(gbm$g_tildes[[m]], X_star)
  } 
  all_y_hat_star
}


#' GBM Predict
#' 
#' Returns final predictions for the gradient boosting model
#'
#' @param gbm     A gradient boosting model of class "qc_basement_gbm"
#' @param X_star  The data to predict for (as a data frame). It has n_* rows and p columns
#'
#' @return        A vector of length n_* rows with each row's predictions. For regression, the
#'                unit is in the units of the original response. For probability estimation for 
#'                binary response, the unit is the logit of the probability estimate.
#' @author Adam Kapelner
#' @method predict qc_basement_gbm
#' @export
predict.qc_basement_gbm = function(gbm, X_star){
  gbm_all_predictions(gbm, X_star)[, gbm$M + 1] #simply return the final prediction column
}

#' Prints a summary of a \code{qc_basement_gbm} object
#' 
#' @param object		The \code{qc_basement_gbm} object to be summarized in the console
#' @param ...			  Other parameters to pass to the default print function
#' 
#' @author Adam Kapelner
#' @method print qc_basement_gbm
#' @export
print.qc_basement_gbm = function(x, ...){
  cat("Gradient boosting model fit with", x$M, "base learners.\n")
}

#' Prints a summary of a \code{qc_basement_gbm} object
#' 
#' @param object		The \code{qc_basement_gbm} object to be summarized in the console
#' @param ...			Other parameters to pass to the default summary function
#' 
#' @author Adam Kapelner
#' @method summary qc_basement_gbm
#' @export
summary.qc_basement_gbm = function(object, ...){
  print(object, ...)
}
