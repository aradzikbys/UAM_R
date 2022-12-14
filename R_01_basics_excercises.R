###################################################
### BASICS - 01
###################################################

# Print 'Hello World'
print('Hello world!')
cat('Hello World!')


###################################################
### BASICS - 02
###################################################

# Create vectors:
### 1, 2, 3, ..., 99, 100, 99, ..., 3, 2, 1
x1 <- seq(1,100,1)
x2 <- seq(99,1,-1)
a <- c(x1,x2)
a

a1 <- c(seq(1,100,1),seq(99,1,-1))
a1

a2 <- c(1:100,99:1)
a2

### 1, 2, 3, 1, 2, 3, ...   (x10 times)
b <- rep(1:3,10)
b

b1 <- rep(1:3, times=10)
b1

#repeat the sequence until lenght of the vector == 30:
b2 <- rep(1:3, length.out = 30) 
b2

### 1, 1, 1, ... , 3, 3, 3, ...   (10 x 1, 10 x 2, 10 x 3)
c <- rep(1:3, each=10)
c

### 1, 4, 7, 10, ..., 100
d <- seq(1,100,3)
d

d1=c(0:33 * 3 + 1)
d1

### 1, 4, 9, 16, ..., 81, 100
e <- (1:10)*(1:10)
e

e1 <- (1:10)^2
e1

### 1, 2, 2, 3, 3, 3, ..., 10, 10, 10, 10
### (1 x 1, 2 x 2, 3 x 3, ..., 10 x 10)
f <- rep(1:10, times=(1:10))
f

### a1, b10, c100, d1000, ..., j1000000000 (9 zer)
g.letters <-  letters[1:10]
g.numbers <- rep(10,10)^(0:9)

g <- paste0(g.letters,as.integer(g.numbers))
g

g1 <- paste(g.letters, as.integer(g.numbers), sep='')
g1



###################################################
### BASICS - 03
###################################################

### Create 3D array with dimensions 3 x 3 x 10:
x1 <- array(rep(1:3,10), c(3,3,10))
x1

x2 <- array(1:3, c(3,3,10)) # repeat the vector, so it will fit dimensions
x2

### Check dimensions of array above:
dim(x1)
dim(x2)

### Create a second array that is a subarray of the first containing elements
# of this array that are 'ones'1' (elements in position 1 in the first dimension).
# What is the dimension of new array?
x1.sub <- x1[1,,]
x1.sub
dim(x1.sub)




###################################################
### BASICS - 04
###################################################

# Create list containing: 
### vector of any numbers, lenght 120:
l4a <- c(0:119)
l4a

### vector containing at least 2 elements: letters/numbers
l4b <- c(1,'c')
l4b


### list containing 2 elements: matrix (4,4) built from a vector of negative numbers
# and as a 2nd element: same matrix, but transposed. 
l4c.m <- matrix(-16:-1, c(4,4))
l4c.m

l4c.t <- t(l4c.m)

l4c <- list(l4c.m,l4c.t)

ex4.list <- list(l4a, l4b, l4c)
ex4.list



