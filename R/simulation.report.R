
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

simulation.report <- function(problem=problem,files=files){

  issim <- 1

  # Evaluate model at each dose level
  doselevel <- problem$data$ids[,1]
  ndose <- length(doselevel)
  fsim <- list()

  for (i in 1:ndose){
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

    # Evaluate the model given the parameter estimates
    fsim[[i]] <- do.call(eval(parse(text=subproblem$modfun)),
                         list(x=subproblem$init,
                              dosing=subproblem$dosing$history,
                              xdata=subproblem$data$xdata,
                              covdata=subproblem$cov$data,
                              issim=issim))
  }

  # Create the matrix to be save to file
  nstate <- size(fsim[[1]],1)-1

  simmatrix <- data.frame(doseID=numeric(0),
                          output=numeric(0),
                          time=numeric(0),
                          value=numeric(0))
  for (i in 1:ndose){
    times <- fsim[[i]][1,]
    ntime <- length(times)
    for (j in 1:nstate){
       tmpmatrix <- data.frame(doseID=rep(i,ntime),
                               output=rep(j,ntime),
                               time=times,
                               value=fsim[[i]][j+1,])
       simmatrix <- rbind(simmatrix,
                          tmpmatrix)
    } 
  }

  # Create or overwrite a simulation csv file
  write.table(simmatrix,
              file=files$sim,
              sep=',',
              na='NA',
              row.names=FALSE,
              quote=FALSE)

  # Add observations to Fsim
  simmatrix$type <- 'pred'

  for (i in 1:ndose){
    times <- problem$data$xdata[problem$data$ids[i,2]:
                                problem$data$ids[i,3]]
    ntime <- length(times)
    for (j in 1:nstate){
      tmpmatrix <- data.frame(doseID=rep(i,ntime),
                              output=rep(j,ntime),
                              time=times,
                              value=problem$data$ydata[problem$data$ids[i,2]:
                                                       problem$data$ids[i,3],j],
                              type=rep('obs',ntime))
      simmatrix <- rbind(simmatrix,
                         tmpmatrix)
    }
  }

  return(simmatrix)
  
}

