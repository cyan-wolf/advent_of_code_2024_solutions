
genGrid <- function(filename) {
  data <- read.csv(filename, header=FALSE, sep=",",
                   colClasses=c("integer", "integer"))
  
  size <- 7
  grid <- matrix(0, nrow=size, ncol=size)
  
  for(i in 1:nrow(data)) {
    row <- data[i,]
    
      r <- row[1, 1]
      c <- row[1, 2]
      
      grid[r + 1, c + 1] <- 1
  }
  
  grid
}

main <- function() {
  grid <- genGrid("input_test.txt")
  
  # TODO: ...
  print(grid)
}

main()