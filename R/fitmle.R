
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

fitmle <- function(problem=NULL,
                   estim.options=NULL,
                   files=NULL){

# Check inputs
  if (is.null(problem) | is.null(estim.options) | is.null(files)){
    stop('One or more input argument of fitmle is null. Please, check your code.')
  }

# Create output function
  myOutputFcn <- function(x=NULL,
                          optimValues=NULL,
                          state=NULL,
                          ...){
    #
    # AIM: save on a log file the results of each iteration
    #
    # Note: With fminsearch, optimValues is the cell containing the following
    #       fields: funccount, fval, iteration, and procedure
    #
    
    # Transform the estimates to reflect the constrains operated in the
    # objective function
    estparam <- problem$init[which(problem$init$isfix==0),]
    fixparam <- problem$init[which(problem$init$isfix==1),]
    x <- bound.parameters(x=x,lb=estparam$lb,ub=estparam$ub)

    # Build the output string
    # - get the current parameter values
    for (i in 1:length(x)){
      if (i==1){
        mystr <- sprintf('%0.6g',x[i])
      } else {
        mystr <- paste(mystr,sprintf('%0.6g',x[i]),sep=',')
      }
    }

    # - build a time stamp
    timestamp <- Sys.time()

    # - build the final string for output
    mystr <- paste(sprintf('%d,%0.6g,%d',optimValues$iteration,
                           optimValues$fval,optimValues$funccount),
                   optimValues$procedure,mystr,timestamp,sep=',')

    # Output only the iterations (not at initialization and completion)
    #if (!any(state==c('init','done'))){
    if (state!='done'){
      write(mystr,file=files$iter,sep='\n',append=TRUE)
    }

  }

# Create objection function minimization routine
  obj.fun <- function(x=NULL,...){
    # Constrain all estimates to the defined intervals
    estparam <- problem$init[which(problem$init$isfix==0),]
    x <- bound.parameters(x=x,lb=estparam$lb,ub=estparam$ub)

    # Get model predictions and calculates objective function (ML)
    ML=0
    
    for (i in 1:length(problem$data$ids[,1])){
      # Create subproblem
      subproblem             <- problem
      subproblem$data$xdata  <- problem$data$xdata[problem$data$ids[i,2]:
                                                   problem$data$ids[i,3]]
      subproblem$data$ydata  <- problem$data$ydata[problem$data$ids[i,2]:
                                                   problem$data$ids[i,3],]
      subproblem$dosing$history <- problem$dosing$history[problem$dosing$ids[i,2]:
                                                          problem$dosing$ids[i,3],]
      if (size(problem$cov$data,1)!=0){
        subproblem$cov$data <- problem$cov$data[problem$cov$ids[i,2]:
                                                problem$cov$ids[i,3],]
      } else {
        subproblem$cov$data <- list(NULL)
      }

      # Calculate ML
      if (!problem$debugmode){

        tmp <- try(
          {
          # Get the model predictions and corresponding weights
          pred <- problem.eval(subproblem=subproblem,x=x)
          ydata <- as.vector(subproblem$data$ydata)
          fpred <- as.vector(pred$f)
          weight <- as.vector(pred$weight)
          # Remove instances of fpred, weight, ydata corresponding to NA values in ydata
          fpred <- fpred[which(!is.na(ydata))]
          weight <- weight[which(!is.na(ydata))]
          ydata <- ydata[which(!is.na(ydata))]
          # Calculate minus twice the log likelihood function
          ML <- ML+sum(0.5*((((fpred-ydata)^2)/(weight))+log(weight)+log(2*pi)))
          },
          silent=TRUE)

        if (class(tmp)=="try-error"){
          cat(paste('An error occured during the computation of the model',
                    'predictions or the negative log likelihood.\n'))
          ML <- Inf
        } else {
          ML <- tmp
        }
        
      } else {
        # Get the model predictions and corresponding weights
        pred <- problem.eval(subproblem,x)
        ydata <- as.vector(subproblem$data$ydata)
        fpred <- as.vector(pred$f)
        weight <- as.vector(pred$weight)
        # Remove instances of fpred, weight, ydata corresponding to NA values in ydata
        fpred <- fpred[which(!is.na(ydata))]
        weight <- weight[which(!is.na(ydata))]
        ydata <- ydata[which(!is.na(ydata))]
        # Calculate minus twice the log likelihood function
        ML=ML+sum(0.5*((((fpred-ydata)^2)/(weight))+log(weight)+log(2*pi)))
      }
    }

    if (is.null(ML) | is.na(ML))
      stop('obj.fun: likelihood function cannot be NA or NULL. Check your models functions.',
           call.=FALSE)
    
    return(ML)
    
  }

# Set adjustable options for the minimization function
  options <- optimset(Display='iter',
                      OutputFcn=myOutputFcn,
                      MaxIter=estim.options$maxiter,
                      MaxFunEvals=estim.options$maxfunc,
                      TolFun=1e-3,
                      TolX=1e-3)

# Default residual variability model: additive
  if (is.null(problem$varfun))
     problem$varfun <- 'weighting.additive'

# Definition of parameter variables
  # Determine which parameters are fixed or not
  estparam <- problem$init[which(problem$init$isfix==0),]
  fixparam <- problem$init[which(problem$init$isfix==1),]

  # initial guess for parameters to be fitted
  x0 <- estparam$value

  # Minization of ML objective function
  minimum <- fminsearch(fun=obj.fun,x=x0,options=options)

  # Rename and transpose minimum$x
  minimum$estimations <- transpose(minimum$x)
  minimum$x <- NULL
  
  # Write output message in report file
  write(paste('\nAlgorithm used:', minimum$output$algorithm),
        file=files$report,append=TRUE,sep='\n')
  write(sprintf('Estimation terminated at: %s\n', Sys.time()),
        file=files$report,append=TRUE,sep='\n')
  write(sprintf('Termination message - %s', minimum$output$message),
        file=files$report,append=TRUE,sep='\n')
  write(sprintf('Number of iterations: %d', minimum$output$iterations),
        file=files$report,append=TRUE,sep='\n')
  write(sprintf('Number of function evaluations: %d', minimum$output$funcCount),
        file=files$report,append=TRUE,sep='\n')
  write(sprintf('Negative log likelood: %f', minimum$fval),
        file=files$report,append=TRUE,sep='\n')
  
  # Model parameter estimation constrained to the defined intervals
  estimations <- bound.parameters(x=minimum$estimations,lb=estparam$lb,estparam$ub)

  # Final value of the objective function
  fval <- minimum$fval

  varargout <- list(estimations=estimations,fval=fval)

  return(varargout)
  
}

