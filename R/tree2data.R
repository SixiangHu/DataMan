#' Prepare data for sankeyPlot from GBM or randomForest model object
#'
#' @description Prepare data for sankeyPlot from GBM or randomForest model object. 
#' @usage tree2data(tree_model,treeInd = 1)
#' @param tree_model This could be either GBM or randomForest model object.
#' @param treeInd An integer to specify the tree index from forest.
#' @author Sixiang Hu
#' @export
#' @examples
#' \dontrun{
#' data(iris)
#' iris.mod <- gbm(Species ~ ., distribution="multinomial", data=iris, n.trees=2000, shrinkage=0.01, cv.folds=5, verbose=FALSE, n.cores=1)
#' tree_data <- tree2data(iris.mod,1)
#' library(networkD3)
#' sankeyNetwork(tree_data[[1]],tree_data[[2]],Source="src",Target="tar",Value="value",NodeID="name")
#' }
tree2data <- function(tree_model,treeInd = 1){
  UseMethod("tree2data",tree_model)
}

#' @export
tree2data.default<- function(tree_model,treeInd = 1){
   stop("Tree to data for sankeyPlot doesn't support current model type.")
}

#' @export
tree2data.randomForest <- function(tree_model,treeInd = 1){
  if ( ! ("randomForest" %in% class(tree_model)) ) stop("Please provide a randomForest object.")
  
  tree_data <- randomForest::getTree(tree_model,treeInd,labelVar = TRUE)
  if(tree_data[1,1]==0) stop("The chosen tree has only a root without nodes.")
  
  node_num <- dim(tree_data)[1]
  
  for (i in 1:node_num){
    if( i==1 ) {
      tree_src <- as.integer(rownames(tree_data)[i])-1
      tree_trg <- tree_data[i,1]-1
      tree_node <- data.frame(src = tree_src,tar = tree_trg,value = 1)
      tree_trg <- tree_data[i,2]-1
      tree_node <- rbind(tree_node,data.frame(src = tree_src,tar = tree_trg,value = 1))
      
      tree_name <- data.frame(name=ifelse(is.na(tree_data[i,3]),tree_data[i,6],as.character(tree_data[i,3])),
                              stringsAsFactors = FALSE)
    }
    else {
      if(tree_data[i,1] != 0) {
        tree_src <- as.integer(rownames(tree_data)[i])-1
        tree_trg <- tree_data[i,1]-1
        tree_node <- rbind(tree_node,data.frame(src = tree_src,tar = tree_trg,value = 1))
        tree_trg <- tree_data[i,2]-1
        tree_node <- rbind(tree_node,data.frame(src = tree_src,tar = tree_trg,value = 1))
      }
      
      tree_name <- rbind(tree_name,
                         data.frame(name=ifelse(is.na(tree_data[i,3]),tree_data[i,6],as.character(tree_data[i,3])),
                                    stringsAsFactors = FALSE))
    }
    
  }
  return (list(Links=tree_node,Nodes=tree_name))
}

#' @export
tree2data.gbm <- function(tree_model,treeInd = 1){
  if (! ("gbm" %in% class(tree_model)) ) stop("Please provide a gbm object.")
  
  tree_data <- gbm::pretty.gbm.tree(tree_model,treeInd)
  if(tree_data[1,1]==-1) stop("The chosen tree has only a root without nodes.")
  
  node_num <- dim(tree_data)[1]
  
  name <- data.frame(name=tree_model$var.names,stringsAsFactors = F)
  
  for (i in 1:node_num){
    if( i==1 ) {
      tree_src <- as.integer(rownames(tree_data)[i])
      tree_trg <- tree_data[i,3]
      tree_node <- data.frame(src = tree_src,tar = tree_trg,value = 1)
      tree_trg <- tree_data[i,4]
      tree_node <- rbind(tree_node,data.frame(src = tree_src,tar = tree_trg,value = 1))
    }
    else {
      if(tree_data[i,1] != -1) {
        tree_src <- as.integer(rownames(tree_data)[i])
        tree_trg <- tree_data[i,3]
        tree_node <- rbind(tree_node,data.frame(src = tree_src,tar = tree_trg,value = 1))
        tree_trg <- tree_data[i,4]
        tree_node <- rbind(tree_node,data.frame(src = tree_src,tar = tree_trg,value = 1))
      }
      else name <- rbind(name,as.character(tree_data[i,2]))
    }
    
  }
  return (list(Links=tree_node,Nodes=name))
}
