#' @export
get_tree_depth = function (tree)
{
  depth=0
  N=NROW(tree$edge)
  for(i in 1:N){
    ichild=tree$edge[i,2]
    depth=depth+tree$edge.length[i]
    if(ichild<=20) return(depth)
  }
  return(depth)
}
