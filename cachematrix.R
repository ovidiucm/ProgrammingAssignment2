
#Disclaimer: The script is addapted from a programming assignment from a Coursera Course called R programming

#---------------------------------PURPOSE OF THE SCRIPT----------------------------------------------------------#
#The script is aiming  to cache potentially time-consuming computations.If the computation time is to long
#and the process should be repeated under the same conditions (input, evaluation type) it might be more convenient
#to proceeed with calculation once, store the calculated value in memory (cache) and then recall it when needed.

#Applications: in repeating loops, simulations,when working modularly on a more complex script and need outputs 
#from previous modules involving heavy computational time.

#The script is providing an exemplification for a simple case of computation of the inverse of a matrix, under the 
#assumption that the matrix is invertible.
#The 2nd function can be addapted for other purposes. 
#

#---------------------------------COMPONENTS----------------------------------------------------------#
#The script contains 2 functions:
#makeCacheMatrix - used to generate 4 functions to handle the cached data 
#initialisation and the transfer from memory to workspace/function environment

#cacheSolve - used to perform initial calculation and retrieval ofcached data in sub-sequent calls

#---------------------------------RUNNING THE SCRIPT----------------------------------------------------------#
#open a script file and run the below block post un-commenting

## 1-Load the function in the woking space

## 2-Generate a in-scope object
# set.seed(12345)
# x<-matrix(rnorm(100,0,1),10,10)
# 
## 3-Initialize
# m1<-makeCacheMatrix(x)
# cacheSolve(m1)
## end of this step will be printed theinverse of the matrix
# 
## 4-Sub-sequent run
# cacheSolve(m1)
## end if the step will be printed a message re retrieval of cached data and these will be printed at screen

#--------------------------------------------------------------------------------------------------------------#

#makeCacheMatrix - used to generate 4 functions to handle the cached data 
#initialisation and the transfer from memory to workspace/function environment
makeCacheMatrix <- function(x = matrix()) { 
        mat_inverse <- NULL                     
        set <- function(y) {                      
                x <<- y
                mat_inverse <<- NULL              
        }
       
        get <- function() x                           
        
        setinverse <- function(solve) mat_inverse <<- solve 
          
        getinverse <- function() mat_inverse        
        
        #creating the output: list of the 4 functions to handle cached data 
        #initialisation and transfer to function workspace    
        list(set = set, get = get,                    
             setinverse = setinverse,
             getinverse = getinverse)
}

#cacheSolve - used to perform initial calculation and retrieval ofcached data in sub-sequent calls
cacheSolve<- function(x, ...) {                 
        mat_inverse <- x$getinverse()
      
        #checking for an existing computation/cached data
        if(!is.null(mat_inverse)) {                 
                message("getting cached data - Inverse of the matrix")
                return(mat_inverse)
        }
       
        #performing initial computation if not cached data available
        data <- x$get()                               
        mat_inverse <- solve(data, ...)
        x$setinverse(mat_inverse)
        mat_inverse
}





