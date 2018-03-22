

#load 4 x 4 CTM values
fourByFourCTM <- read.csv("./data/K-4x4.csv",
                          stringsAsFactors=FALSE, 
                          colClasses = 
                            c("character", "numeric"),
                          header = FALSE)

colnames(fourByFourCTM) <- c("square", "CTM")
rownames(fourByFourCTM) <- fourByFourCTM$square
fourByFourCTM$square <- NULL

#load 3 x 3 CTM values
threeByThreeCTM <- read.csv("./data/K-3x3.csv",
                            stringsAsFactors=FALSE, 
                            colClasses = 
                              c("character", "numeric"),
                            header = FALSE)

colnames(threeByThreeCTM) <- c("square", "CTM")
rownames(threeByThreeCTM) <- threeByThreeCTM$square
threeByThreeCTM$square <- NULL


##split matrices in blocks
require(purrr)
ind <- function(matDim, blockSize, offset) {
  Map(`:`, seq(1, matDim-blockSize+1, by = offset), 
      seq(blockSize, matDim, by = offset))
}

# this is a helper function that generates subset indexing
# according to dimension of the 
# matrix, the first sequence constructs the starting point of the subset index considering 
# the offset while the second sequence constructs 
# the ending point of the subset index
myPartition <- function(mat, blockSize, offset) {
  lapply(cross2(ind(nrow(mat),blockSize,offset), 
                ind(ncol(mat),blockSize,offset)), 
         function(i) mat[i[[1]], i[[2]]])
}

#used to lookup entries in fourByFourCTM and threeByThreeCTM
stringify <- function(smallBlock){
  paste0(c(t(smallBlock)), collapse ="")
}

blockEntropy <- function(mat, blockSize, offset){
  
  parts <- myPartition(mat, blockSize, offset)
  
  flatSquares <- unlist(lapply(parts, stringify))
  
  squaresTally <- as.data.frame(table(flatSquares))
  
  rownames(squaresTally) <- squaresTally$flatSquare
  
  squaresTally$flatSquares <- NULL
  
  probs = squaresTally[, 1]/nrow(squaresTally)
  
  return(-sum(probs*log2(probs)))
  
}

bdm2D <- function(mat, blockSize, offset){
  
  parts <- myPartition(mat, blockSize, offset)
  
  flatSquares <- unlist(lapply(parts, stringify))
  
  squaresTally <- as.data.frame(table(flatSquares))
  
  rownames(squaresTally) <- squaresTally$flatSquares
  
  squaresTally$flatSquares <- NULL
  
  if(blockSize == 4){
    bdm <- (sum(fourByFourCTM[rownames(squaresTally),]) 
            + sum(log2(squaresTally$Freq)))
  } else{
    bdm <- (sum(threeByThreeCTM[rownames(squaresTally),]) 
            + sum(log2(squaresTally$Freq)))
  }
  
  return(bdm)
}

## tests
# set.seed(42)
# m99 <- apply(matrix(0, 9, 9), c(1,2), function(x) sample(c(0,1),1))
# m99

# testResult1 <- bdm2D(m88, 4, 4)
# testResult1
# 
# 
# testResult2 <- bdm2D(m99, 3, 3)
# testResult2


