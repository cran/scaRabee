
#Copyright (c) 2009, 2010 Sebastien Bihorel
#All rights reserved.
#
#This file is part of scaRabee.
#
#    scaRabee is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    scaRabee is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with scaRabee.  If not, see <http://www.gnu.org/licenses/>.
#

get.cov.matrix <- function(problem=NULL,Fit=NULL){

  # Initialization of the matrix
  M <- matrix(0,nrow=length(Fit$estimations),ncol=length(Fit$estimations))

  # Reorder param list
  ordered <- order.param.list(x=problem$init)

  # Filter the param and ordered data structures to get only the estimated
  # parameters
  estparam <- problem$init[which(problem$init$isfix==0),]
  fixparam <- problem$init[which(problem$init$isfix==1),]
  estorder <- ordered[which(ordered$isfix==0),]
  fixorder <- ordered[which(ordered$isfix==1),]

  # Calculates the number of estimated model and variance parameters
    # p = nb of model parametres
  p <- length(get.param.data(x=estparam,which='type',type='P')) +
       length(get.param.data(x=estparam,which='type',type='L')) +
       length(get.param.data(x=estparam,which='type',type='IC'))
    # q = nb of variance parametres
  q <- length(get.param.data(x=estparam,which='type',type='V')) 

  # Determines in estparam the corresponding parameter indices from estorder
  indices <- match(estorder$names,estparam$names)

  # Computes model predictions and partial derivatives using original
  # parameter order (x and param)
  w <- c() ; mpder <- c() ; wpder <- c()

  for (i in 1:length(problem$data$ids[,1])){
    # Creates subproblem
    subproblem            <- problem
    subproblem$data$xdata <- problem$data$xdata[problem$data$ids[i,2]:
                                                problem$data$ids[i,3]]
    subproblem$data$ydata <- problem$data$ydata[problem$data$ids[i,2]:
                                                problem$data$ids[i,3],]
    subproblem$dosing$history <- problem$dosing$history[problem$dosing$ids[i,2]:
                                                        problem$dosing$ids[i,3],]
    if (size(problem$cov$data,1)!=0){
      subproblem$cov$data <- problem$cov$data[problem$cov$ids[i,2]:
                                              problem$cov$ids[i,3],]
    } else {
      subproblem$cov$data <- list(NULL)
    }

    # Obtains the model prediction and weighting based on final estimates
    tmp <- problem.eval(subproblem=subproblem,x=Fit$estimations)
      subf <- as.vector(tmp$f)
      subw <- as.vector(tmp$weight)
    rm(tmp)

    ydata <- as.vector(subproblem$data$ydata)

    # Remove instances of Weight corresponding to NaN values in ydata
    subw <- subw[which(!is.na(ydata))]

    # Obtains the matrix or partial derivatives with respect to final estimates
    tmp <- pder(subproblem=subproblem,x=Fit$estimations)
      submpder <- tmp$mpder
      subwpder <- tmp$wpder
    rm(tmp)

    # Reorders submpder and subwpder to compute and output the matrix properly (P,L,IC,V)
    submpder <- submpder[indices,]
    subwpder <- subwpder[indices,]

    # Appends w, submpder, subwpder
    w <- c(w,subw)
    mpder <- cbind(mpder,submpder)
    wpder <- cbind(wpder,subwpder)
  }

  # Computes the matrix to be inversed to get the covariance matrix
    # covariance of the model parameters
  
  M <- matrix(NA,nrow=p+q,ncol=p+q)
  for (j in 1:p){
    for (k in 1:p){
      M[j,k] <- 0.5*sum((1/w^2)*wpder[j,]*wpder[k,])+sum((1/w)*mpder[j,]*mpder[k,])
    }
  }
    # covariance of variance parameters
  for (j in (p+1):(p+q)){
    for (k in (p+1):(p+q)){
      M[j,k] <- 0.5*sum((1/w^2)*wpder[j,]*wpder[k,])
    }
  }
    # covariance of the model parameters with the variance parameters
  for (j in 1:p){
    for (k in (p+1):(p+q)){
      M[j,k] <- 0.5*sum((1/w^2)*wpder[j,]*wpder[k,])
    }
  }
    # covariance of the variance parameters with the model parameters
  for (j in (p+1):(p+q)){
    for (k in 1:p){
      M[j,k] <- M[k,j]
    }
  }

  # Computes the covariance matrix
  covmatrix <- solve(qr(M,LAPACK=TRUE))

  # Gets estimated parameter reordered values, names, type
  estimordered <- data.frame(names=estparam$names[indices],
                             value=Fit$estimations[indices,1],
                             type =estparam$type[indices],
                             stringsAsFactors=FALSE)

  varargout <- list(covmatrix=covmatrix,estimordered=estimordered)

  return(varargout)

}

