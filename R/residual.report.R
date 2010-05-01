
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

residual.report <- function(problem=NULL,Fit=NULL,files=NULL){
  
  # Definition of some local variables by transfer from Fit and states variables
    # Optimized model and variance parameters
  x      <- Fit$estimations
  nstate <- length(problem$states)
  
  # Copies param in newparam and replaces the new estimates in newparam
  newparam <- problem$init
  npar     <- length(newparam$names)
  estindex <- 1
  for (i in 1:npar){
    if (newparam$isfix[i]==0){
      newparam$value[i] <- x[estindex]
      estindex <- estindex + 1
    }
  }

  write(paste('Dose ID,Obs No,Output,Time,Observation,Prediction,Residual,',
              'Variance,Weighted residual',sep=''),
        file=files$pred,append=FALSE,sep='\n')
  
  # Consider each dose level
  for (i in 1:length(problem$data$ids[,1])){
    # Creates subproblem
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
  
    # Gets the model predictions
    pred <- do.call(eval(parse(text=subproblem$modfun)),
                    list(x=newparam,
                         dosing=subproblem$dosing$history,
                         xdata=subproblem$data$xdata,
                         covdata=subproblem$cov$data))
    # Convert pred to matrix if is a vector
    if (size(pred,1)==1)
      pred <- matrix(pred,nrow=1)
    
    # Gets the weights and weighted residuals
    varpar <- newparam[which(newparam$type=='V'),]

    if (is.null(varpar)){
      weight <- do.call(eval(parse(text=subproblem$varfun)),
                        list(x=NULL,f=pred,xdata=subproblem$data$xdata))
    } else {
      weight <- do.call(eval(parse(text=subproblem$varfun)),
                        list(x=varpar,f=pred,xdata=subproblem$data$xdata))
    }

    # Convert weight to matrix if is a vector
    if (size(weight,1)==1)
      weight <- matrix(weight,nrow=1)
      
    # Convert ydata to matrix if is a vector
    ydata <- subproblem$data$ydata
    if (size(ydata,1)==1)
      ydata <- matrix(ydata,nrow=1)

    # Calculates the residuals
    res   <- ydata - pred
    wres  <- res/weight

    # Print the residuals to file
    tmpform <- paste(c(rep('%d,',3),rep('%0.5g,',5),'%0.5g'),collapse='')
    for (j in 1:nstate){
      for (k in 1:length(subproblem$data$xdata)){
        tmp <- c(i,k,j,subproblem$data$xdata[k],ydata[j,k],
                 pred[j,k],res[j,k],weight[j,k],wres[j,k])
        write(do.call(sprintf,c(list(tmpform),as.list(tmp))),
              file=files$pred,append=TRUE,sep='\n')
      }
    }
  }
}

