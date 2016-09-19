between <-
function(in.data, ge=NULL, lt=NULL, gt=NULL, le=NULL) {
  
  if (!is.null(ge)) {
    lower.logic <- in.data >= ge
  } else if (!is.null(gt)) {
    lower.logic <- in.data > gt
  } else {
    stop("you must specify a lower bound.")
  }
  
  if (!is.null(lt)) {
    upper.logic <- in.data < lt
  } else if (!is.null(le)) {
    upper.logic <- in.data <= le
  } else {
    stop("you must specify an upper bound.")
  }
  
  out.logic <- lower.logic & upper.logic
  
  return(out.logic)
}
