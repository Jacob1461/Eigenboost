#' @export
print.eigenboost = function(x, ...){
  print("===========================================")
  print("=========Adaboost With Reflections=========")
  print("===========================================")
  cat("Number of rounds:", length(x$alphas), "\n \n",
      "Alphas:", x$alphas,"\n \n",
      "Reflections_Type:", x$model_info$Reflection_type,"\n",
      "Reflect Dataset every:", x$model_info$Reflection_every,"\n",
      "Tree Depth:", x$model_info$Tree_depth,"\n",
      "Adaboost Terminated Early:", x$model_info$Early_Termination
  )
  invisible(x)
}
