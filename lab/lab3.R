
matrix.100 <- matrix(data = 1:100, nrow = 10, ncol = 10, byrow = FALSE,
       dimnames = NULL)

numbers <- c(4,5,9,10,12,15,16,17)

mat1 <- matrix(numbers, nrow = 4, ncol = 2, byrow= FALSE)
mat2 <- matrix(numbers, nrow = 4, ncol = 2, byrow = TRUE)

a <- matrix.100[c(1,3,5),]


mat3 <- mat1[,c(1:2)]

data <- read.csv("wcgs.csv")
print(data)
print(data[c(1:6),])

print(data.frame[,c(3,4)])

print(matrix.100)
print(mat1)
print(mat2)
print(mat3)
print(a)
