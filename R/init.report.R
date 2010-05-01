
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

init.report <- function(param=NULL,
                        files=NULL){

  # Determines which parameters will be estimated
  x <- param[which(param$isfix==0),]
  fixparam <- param[which(param$isfix==1),]

  # Creates and populates the iteration log
  mystr <- paste('Iteration,Objective function,Number of evaluations,Procedure',
                 paste(x$names,collapse=','),
                 'Time',
                 sep=',')
  
  write(mystr,file=files$iter)
  
  # Creates and populates the report file
  tmp <- 'Maximum likelihood estimation report\n'
  tmp <- c(tmp,paste('Estimation started at: ',
                     Sys.time(),
                     '\n',
                     sep=''))
  tmp <- c(tmp,paste('Model defined in:',
                     paste(rep(' ',24),collapse=''),
                     files$model,'.R',sep=''))
  tmp <- c(tmp,paste('Residual variability model defined in:',
                     paste(rep(' ',3),collapse=''),
                     files$var,'.R',
                     sep=''))
  tmp <- c(tmp,paste('Secondary parameter(s) defined in:',
                     paste(rep(' ',7),collapse=''),
                     files$sec,'.R',
                     sep=''))
  tmp <- c(tmp,paste('Data imported from:',
                     paste(rep(' ',22),collapse=''),
                     files$data,
                     sep=''))
  tmp <- c(tmp,paste('Covariate(s) data imported from:',
                     paste(rep(' ',9),collapse=''),
                     files$cov,
                     sep=''))
  tmp <- c(tmp,paste('Parameter characteristics imported from: ',
                     files$param,
                     sep=''))
  tmp <- c(tmp,paste('Dosing history imported from:',
                     paste(rep(' ',12),collapse=''),
                     files$dose,
                     sep=''))

  write(tmp,file=files$report,sep='\n')
}

