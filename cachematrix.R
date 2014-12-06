## cachematrix.R

## Justin Britt
## R Programming - Johns Hopkins University via Coursera
## Programming Assignment 2
## December 2014

## This file contains two functions that store a matrix and cache its inverse.
## These functions assume that the given matrix is square invertible.

## makeCacheMatrix accepts a regular matrix and then returns 
## a matrix object, which is in the form of a list, that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve checks to see if the inverse of the matrix from makeCacheMatrix
## has already been cached. If it has been cached, then it returns the inverse.
## Otherwise, it calculates the inverse, caches it, and then returns it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
