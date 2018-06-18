library(purrr)

fourByFourCTM <- read.csv("./data/K-4x4.csv", stringsAsFactors = FALSE, 
                          colClasses = c("character", "numeric"), header = FALSE)

colnames(fourByFourCTM) <- c("square", "CTM")
rownames(fourByFourCTM) <- fourByFourCTM$square
fourByFourCTM$square    <- NULL

threeByThreeCTM <- read.csv("./data/K-3x3.csv", stringsAsFactors = FALSE, 
                            colClasses = c("character", "numeric"), header = FALSE)

colnames(threeByThreeCTM) <- c("square", "CTM")
rownames(threeByThreeCTM) <- threeByThreeCTM$square
threeByThreeCTM$square    <- NULL

ind <- function(matDim, blockSize, offset) {
  
    Map(`:`, seq(1, matDim-blockSize+1, by = offset), 
        seq(blockSize, matDim, by = offset))
}

myPartition <- function(mat, blockSize, offset) {
  
    lapply(cross2(ind(nrow(mat), blockSize, offset), 
                  ind(ncol(mat), blockSize, offset)), 
           function(i) mat[i[[1]], i[[2]]])
}

stringify <- function(smallBlock){
  
    paste0(c(t(smallBlock)), collapse ="")
}

bdm2D <- function(mat, blockSize, offset){
  
    parts <- myPartition(mat, blockSize, offset)
    
    flatSquares <- unlist(lapply(parts, stringify))
    
    squaresTally <- as.data.frame(table(flatSquares))
    
    rownames(squaresTally) <- squaresTally$flatSquares
    
    squaresTally$flatSquares <- NULL
    
    if(blockSize == 4) {
        bdm <- (sum(fourByFourCTM[rownames(squaresTally), ]) 
                + sum(log2(squaresTally$Freq)))
    } else {
        bdm <- (sum(threeByThreeCTM[rownames(squaresTally), ]) 
                + sum(log2(squaresTally$Freq)))
    }
  
    return(bdm)
}