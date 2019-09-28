# 1.1

#Defining our matrix. When you just type the letter, it prints the matrix
M = matrix(c(1/4,sqrt(3)/4,sqrt(3)/4,3/4),nrow=2,ncol=2)
M
t(M)
t(M)-M

#Defining the diagonal and solving for the Trace (sum of diagonal)
diag(M)
sum(diag(M))

#To check for idempotence 
M%*%M

#Finding the eigenvalue of matrix. If we deal with a square matrix, 
#the sum of eigenvalues of "A" is equal to the trace of A. Also,
#The product of eigenvalues gives you the determinant of A.
eigen(M)
prod(eigen(M)$values)
sum(eigen(M)$values)

det(M)

# 1.2 - see also slides 45 & 46. Creating big X
X = matrix(seq(1:6),nrow=3,ncol=2)
X
t(X)%*%X

#Creating the matrix from scratch
#The rows (descibes x1 transposed really)
x1 = c(1,4)
x2 = c(2,5)
x3 = c(3,6)

#Binding rows together to make a matrix
rbind(x1,x2,x3)

#Proving that the summation of xx'for each x is equal to X'X
x1%*%t(x1)
x2%*%t(x2)
x3%*%t(x3)

x1%*%t(x1) + x2%*%t(x2) + x3%*%t(x3)
t(X)%*%X
