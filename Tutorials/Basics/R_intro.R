# This is a comment

# This is a number
42

# This is a Boolean
TRUE

# This is a vector of length 3
c("a","b","c")

# Actually, this is a vector containing one number
42

# This is a function being called with two arguments
paste("a","b")

# This is a function with an optional named argument
paste("a","b",sep=",")

# This is assignment
x <- c("a","b","c","d","e")
y <- seq(1,10,by=2)  # the first five odd numbers

# This is how to extract parts of a vector
x[2]  # "b"
x[c(1,2,4)]  # c("a","b","d")
x[3:5]  # c("c","d","e")

# This is how to bind two vectors to form a matrix
m <- rbind(x,y)
m <- cbind(x,y)

# This is how to slice a matrix
m[1,]  # the first row
m[,1]  # the first column
m[2:3,1]  # the first element of the second and third row

# This is a data frame (think of it like a spreadsheet)
d <- data.frame(letters=x,numbers=y)

# This is how to extract by name
d$letters
d$numbers
d$letters[2:3]
d[['letters']]

# This is how to load a library
library(SPARQL)

# This is how to find out what it does
?'SPARQL-package'
?SPARQL
?paste

# This is how to find functions
??split

