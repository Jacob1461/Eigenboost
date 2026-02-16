#' Adaboost on reflected datasets
#'
#' An implementation of adaboost using eigenvector based dataset reflections
#' using PCA and householder matrices.
#'
#' @param X A dataframe or matrix of predictors values.
#' @param y A response vector of 1 or -1.
#' @param n_rounds The number of trees in the ensemble.
#' @param reflection_every Perform a Dataset reflection every this many rounds.
#' @param verbose Print additional output.
#' @param control Additional settings for the CART trees
#' @param reflections_type The type of Dataset reflection to perform. Options include:
#' identity, for no data reflections (identical to normal Adaboost),
#' eigen, for eigenvector based reflections based on the dominant eigenvector of the class which reduces training error the most,
#' random, reflect the dataset in a random direction,
#' eigen-random, choose either eigen or random reflections with equal probability.
#'
#' @param tree_depth The max depth of the CART trees.
#'
#'
#' @references Freund, Y. and Schapire, R. (1997). A decision-theoretic
#' generalization of online learning and an application to boosting, Journal of
#'  Computer and System Sciences 55: 119-139.
#'
#' @return Returns a eigenboost object containing the alpha values for each tree,
#' each tree object, the householder matrix corresponding to that tree.
#'
#'
#' @note Adaboost code adapted from JOUSboost implementation.
#'
#' @examples
#' \dontrun{
#'   set.seed(123)
#'
#' # Make iris into a binary class classification dataset
#' iris2 <- iris[iris$Species != "setosa", ]
#' rownames(iris2) <- NULL
#' iris2$Species <- ifelse(iris2$Species == "versicolor", 1, -1)
#'
#' # Make training and testing splits
#' train_indx <- sample(nrow(iris2), 65)
#' X_train <- iris2[train_indx, 1:4]
#' y_train <- as.numeric(iris2$Species[train_indx])
#' X_test <- iris2[-train_indx, 1:4]
#' y_test <- as.numeric(iris2$Species[-train_indx])
#'
#' # Fit model
#' model <- eigenboost(X = X_train, y = y_train, reflection_every = 3, tree_depth = 1)
#'
#' # Make predictions
#' preds <- predict.eigenboost(model, X_test)
#'
#' # Evaluate
#' accuracy <- mean(preds == y_test)
#' print(accuracy)
#' }
#' @export
eigenboost = function(X, y, n_rounds = 10, reflection_every = 3, verbose = FALSE,
                         control = NULL, reflections_type = "eigen",tree_depth = 1){

  if(any(is.na(X))){
    stop("There are missing values, these must be removed.")
  }

  column_names <- colnames(X)
  new_column_names <- make.names(column_names)

  if(reflections_type == "eigen-random"){
    # If eigen-random reflection type, then set this bool to True, before each
    # Reflection is done, it will change reflections_type to either random or eigen with p=0.5
    Eigen_Random <- TRUE
  }else{
    Eigen_Random <- FALSE
  }

  if(!identical(column_names, new_column_names)){
    warning("Some column names have been changed.")
    column_names <- new_column_names

  }

  X <- as.matrix(X)
  n_col <- ncol(X)

  if (!all(y %in% c(-1, 1))) {
    print(unique(y[!(y %in% c(-1, 1))]))
    stop("y must only contain values in c(-1,1)")
  }

  # check for presence of rpart control
  if(is.null(control)){
    control = rpart::rpart.control(minsplit = 0, minbucket = 1, cp = -1,
                                   maxcompete = 0, maxsurrogate = 0,
                                   usesurrogate = 0, xval = 0,
                                   maxdepth = tree_depth)
  } else if(control$maxdepth != tree_depth){
    warning(paste('tree_depth set to: ', control$maxdepth))
  }

  trees = list()
  alphas = list()
  householders <- list()

  w = rep(1/nrow(X), nrow(X))
  zi_s <- list()

  # Indices of the dataset that correspond to either pos or neg class example
  pos_index <- which(y == 1)
  neg_index <- which(y == -1)

  # This will change to T if adaboost's early termination condition happens and will be included in model information
  terminated_early <- "no"

  for(i in seq(n_rounds)){



    if(reflections_type == "identity"){
      # If reflections_type is identity, then the reflection rounds parameter does not apply.
      # Then set it to 1 so that it just multiples with Identity matrix every round
      reflection_every = 1}

    if(Eigen_Random){
      #Randomly pick between an eigen reflection type and a random reflection type
      reflections_type <- sample(c("eigen", "random"), 1)
    }
    if(i %% reflection_every == 0 || i == 1){
      switch(reflections_type,
             "eigen" = {
               u_pos <- domiant_eigenvector(X[pos_index,], w[pos_index])
               u_neg <- domiant_eigenvector(X[neg_index,], w[neg_index])

               if(!is.null(u_pos) && !is.null(u_neg)){
                 DH_pos <- X - 2*(X %*% u_pos) %*% t(u_pos)
                 DH_neg <- X - 2*(X %*% u_neg) %*% t(u_neg)
                 colnames(DH_pos) <- column_names
                 colnames(DH_neg) <- column_names

                 t_pos = rpart::rpart(y ~ ., data = as.data.frame(DH_pos), weights = w,
                                      method = "class", control = control, x=FALSE, y=FALSE,
                                      model=FALSE)

                 t_neg = rpart::rpart(y ~ ., data = as.data.frame(DH_neg), weights = w,
                                      method = "class", control = control, x=FALSE, y=FALSE,
                                      model=FALSE)

                 pred_pos = as.integer(as.character(stats::predict(t_pos, data.frame(DH_pos), type="class")))
                 e_pos = sum(w*(pred_pos != y))

                 pred_neg = as.integer(as.character(stats::predict(t_neg, data.frame(DH_neg), type="class")))
                 e_neg = sum(w*(pred_neg != y))

                 if(e_pos > e_neg){
                   Z <- DH_neg
                   householders[[i]] <- u_neg
                 }else{
                   Z <- DH_pos
                   householders[[i]] <- u_pos
                 }
               }else if(is.null(u_pos) && !is.null(u_neg)){
                 DH_neg <- X - 2*(X %*% u_neg) %*% t(u_neg)
                 colnames(DH_neg) <- column_names
                 Z <- DH_neg
                 householders[[i]] <- u_neg
               }else if(!is.null(u_pos) && is.null(u_neg)){
                 DH_pos <- X - 2*(X %*% u_pos) %*% t(u_pos)
                 colnames(DH_pos) <- column_names
                 Z <- DH_pos
                 householders[[i]] <- u_pos
               }else{
                 householders[[i]] <- diag(n_col)
               }
             }
             ,
             "random" = {
               v <- MASS::mvrnorm(n = 1,
                            mu = rep(0, n_col),
                            Sigma = diag(n_col))

               e1 <- matrix(c(1, rep(0, n_col - 1)), ncol = 1)

               u <- (v - e1) / sqrt(sum((v - e1)^2))
               Z = X - 2*(X %*% u)%*%t(u)
               householders[[i]] <- u

             },
             "identity" = {
               # Wasteful computation, but is sort of a foundational part of the project
               Z <- X %*% diag(n_col)
               # Rather than inserting a u_vector here, it will be a matrix
               householders[[i]] <- diag(n_col)
             })



    }else if((i %% reflection_every != 0) && (i >= 2)){
      # Not a reflection round
      # Since the round is 2 or more, then copy in the last rounds Zi, and let H be the last householder of the last round
      if(verbose){cat("i = ", i, "Not a reflection round, not transforming dataset")}
      Z <-  zi_s[[i-1]]
      householders[[i]] <- householders[[i-1]]
    }else{
      # Not a reflection round and there is no previous round, so take X (original data) as the current data.
      # This would be the same as doing an "identity" reflection
      if(verbose){cat("i = ", i, "Not a reflection round, not transforming dataset")}
      Z <-  X
      householders[[i]] <- ncol(X) |> diag()
    }

    colnames(Z) <- column_names #Z needs to have the column names for rpart and predict
    zi_s[[i]] <- Z

    # This is partly wasteful as we have done this before if its a reflection round, but idc
    tree = rpart::rpart(y ~ ., data = as.data.frame(Z), weights = w,
                        method = "class", control = control, x=FALSE, y=FALSE,
                        model=FALSE)

    # trim tree object
    tree$where=NULL
    tree$call=NULL
    tree$cptable=NULL
    tree$functions=NULL
    tree$control=NULL
    tree$variable.importance=NULL
    tree$parms=NULL

    pred = as.integer(as.character(stats::predict(tree, data.frame(Z), type="class")))

    e = sum(w*(pred != y))

    # If tree perfectly gets data, boosting terminates
    if(abs(e) < 1e-8){
      terminated_early <- 'yes'
      # handle the case where first base classifier fits data perfectly
      if(i == 1){
        trees[[i]] = tree
        alphas[[i]] = 1
        terms = tree$terms
        cat("|e| < 1e-8 and i==1. Adaboost breaking loop")
        break
      }
      cat("|e| < 1e-8, adaboost breaking")
      break
    }

    alpha = 1/2*log((1-e)/e)
    w = w*exp(-alpha*pred*y)
    w = w/sum(w)


    # kill formulas since they waste memory
    if(i == 1){
      terms = tree$terms
    } else{
      tree$terms = NULL
    }

    trees[[i]] = tree
    alphas[[i]] = alpha

  }
  #############
  model_info <- list(
    Reflection_type = reflections_type,
    Reflection_every = reflection_every,
    Tree_depth = tree_depth,
    Early_Termination = terminated_early
  )

  out = list(alphas = unlist(alphas), trees = trees,
             terms=terms, householders = householders, model_info = model_info)

  class(out) = "eigenboost"
  out

}


#' Create predictions from Eigenboost model
#'
#' Aggregates predictions from all trees in the ensemble.
#'
#'
#' @param object An object of type eigenboost
#' @param new_data A data frame or data matrix with which to create new predictions
#' @param n_tree Only take the first n_tree many trees in the ensemble to create new prediction
#'
#' @return Returns the predicted class in c(-1, 1)
#'
#' @export
predict.eigenboost <- function(object, new_data, n_tree = NULL){
  if(!inherits(object, "eigenboost")){
    warning("Object is not of type adaboost_reflections, it is", class(object))
    stop("Stopping")
  }
  new_data_matrix <- as.matrix(new_data)

  cols_name <- colnames(new_data)
  new_cols_name <- make.names(cols_name)

  if(!identical(cols_name, new_cols_name)){
    warning("Not all column names are Valid and have been replaced with make.names()")
    cols_name <- new_cols_name
  }

  if(is.null(n_tree)){
    tree_seq = seq_along(object$alphas)
  } else{
    if(n_tree > length(object$alpha))
      stop('n_tree must be less than the number of trees used in fit')
    tree_seq = seq(1, n_tree)
  }

  householders <- object$householders
  alphas <- object$alphas

  ff = 0
  for (i in tree_seq) {
    transformation <- householders[[i]]

    if(is.vector(transformation)){
      # Then transformation must be a unit vector u, to make a householder with
      Z <- new_data_matrix - 2 * (new_data_matrix %*% transformation) %*% t(transformation)
    }else{
      #Then transformation must be an identity matrix, therefore no transform needed
      Z <- new_data_matrix
      stopifnot(is.matrix(transformation))
    }

    colnames(Z) <- cols_name
    current_tree <- object$trees[[i]]
    current_tree$terms <- object$terms
    preds <- as.integer(as.character(stats::predict(object = current_tree, data.frame(Z),type="class")))
    ff = ff + object$alphas[i]*preds
  }
  return(sign(ff))
}




