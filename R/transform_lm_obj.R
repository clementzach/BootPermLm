#' @importFrom stats lm

transform_lm_obj <- function(input_lm){
  if(class(input_lm) != "lm"){
  stop("input_lm must be a lm object")
  }
  
  if ((! ("x" %in% names(input_lm)) & 
      ("y" %in% names(input_lm)) ) & 
      !("model" %in% names(input_lm)) ){
    stop("input_lm must be created by either specifying model = T (the default) or x = T and y = T")
  }

if (!("x" %in% names(input_lm)) | !("y" %in% names(input_lm))){
  
  
  call_list <- as.list(input_lm$call[-1])
  
  call_list$x <- T
  call_list$y <- T
  call_list$data = as.data.frame(input_lm$model)
  
  input_lm <- do.call(
    lm,
    
    call_list
    
    
  )
  
}
return(input_lm)
}

