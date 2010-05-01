
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

problem.eval <- function(subproblem=NULL,x=NULL){

  # Copie subproblem$init in newparam and replaces the new estimates in newparam
  newparam <- subproblem$init
  npar     <- length(newparam$names)
  estindex <- 1
  for (i in 1:npar){
    if (newparam$isfix[i]==0){
      newparam$value[i] <- x[estindex]
      estindex <- estindex + 1
    }
  }

  # Evaluate the full model function modfun
  fpred <- do.call(eval(parse(text=subproblem$modfun)),
                   list(x=newparam,
                        dosing=subproblem$dosing$history,
                        xdata=subproblem$data$xdata,
                        covdata=subproblem$cov$data))

  # Convert fpred to matrix if is a vector
  if (size(fpred,1)==1)
    fpred <- matrix(fpred,nrow=1)

  # Check if fpred contains imaginary numbers
  if (any(is.complex(fpred)))
    cat(' Imaginary number(s) predicted.\n')

  # Evaluate the variance function varfun
#  varpar <- get.param.data(x=newparam,which='value',type='V')
  varpar <- newparam[which(newparam$type=='V'),]
  
  if (is.null(varpar)){
    weight <- do.call(eval(parse(text=subproblem$varfun)),
                      list(x=NULL,f=fpred,xdata=subproblem$data$xdata))
  } else {
    weight <- do.call(eval(parse(text=subproblem$varfun)),
                      list(x=varpar,f=fpred,xdata=subproblem$data$xdata))
  }

  varargout <- list(f=fpred,weight=weight)

  return(varargout)

}

