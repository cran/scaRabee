
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

scarabee.directory <- function (curwd=getwd(),
                                files=NULL,
                                runtype=NULL,
                                analysis=NULL){

  # Check inputs
  if (!is.null(curwd)){
    if (!file.exists(curwd)){
      stop('scarabee.directory: curwd is expected to be an existing path.',
           call.=FALSE)
    }
  } else {
    stop('scarabee.directory: curwd is expected to be an existing path, but is NULL.',
         call.=FALSE)
  }
  
  # Interactive call
  if (interactive()){
    repeat{
      wd <- readline(sprintf(paste('Change the current working directory: %s?\n',
                                   '(Enter a new path if you want to use a new ',
                                   'directory)\n',
                                   sep=''),
                             curwd))
      if (wd!=''){
        if (file.exists(wd)){
          setwd(wd)
          break
        }
      } else {
        break
      }
    }
  }

  # Create new directory and set working directory
  dirCreated <- 0 ; i <- 1

  while (dirCreated==0){
    if (i<10){
      dirName <- sprintf('%s.%s.0%d',
                         analysis,
                         substring(runtype,1,3),
                         i)
    } else {
      dirName <- sprintf('%s.%s.%d',
                         analysis,
                         substring(runtype,1,3),
                         i)
    }
    if (file.exists(dirName)){
      i <- i+1
    } else {
      dir.create(dirName)
      dirCreated <- 1
    }
  }

  newwd <- paste(getwd(),'/',dirName,'/',sep='')
  setwd(newwd)

  # Create sub-directories
  dir.create(paste(newwd,'model.definition/',sep=''))
  dir.create(paste(newwd,'run.config.files/',sep=''))
  dir.create(paste(newwd,'run.config.files/model.definition/',sep=''))
 
  # Copy configuration files to working directory
  file.copy(from=paste('../',files$data,sep=''),
            to=newwd)
  file.copy(from=paste('../',files$param,sep=''),
            to=newwd)
  file.copy(from=paste('../',files$dose,sep=''),
            to=newwd)
  file.copy(from=paste('../',files$cov,sep=''),
            to=newwd)
  file.copy(from=paste('../model.definition/',files$model,'.R',sep=''),
            to=paste(newwd,'model.definition/',sep=''))
  file.copy(from=paste('../model.definition/',files$var,'.R',sep=''),
            to=paste(newwd,'model.definition/',sep=''))
  file.copy(from=paste('../model.definition/',files$sec,'.R',sep=''),
            to=paste(newwd,'model.definition/',sep=''))

  # Copy configuration files to backup directory
  file.copy(from=paste('../',files$data,sep=''),
            to=paste(newwd,'run.config.files/',sep=''))
  file.copy(from=paste('../',files$param,sep=''),
            to=paste(newwd,'run.config.files/',sep=''))
  file.copy(from=paste('../',files$dose,sep=''),
            to=paste(newwd,'run.config.files/',sep=''))
  file.copy(from=paste('../',files$cov,sep=''),
            to=paste(newwd,'run.config.files/',sep=''))
  file.copy(from=paste('../model.definition/',files$model,'.R',sep=''),
            to=paste(newwd,'run.config.files/model.definition/',sep=''))
  file.copy(from=paste('../model.definition/',files$var,'.R',sep=''),
            to=paste(newwd,'run.config.files/model.definition/',sep=''))
  file.copy(from=paste('../model.definition/',files$sec,'.R',sep=''),
            to=paste(newwd,'run.config.files/model.definition/',sep=''))
            
  # Copy original R script if possible
  script <- paste('../',analysis,'.R',sep='')

  if (file.exists(script)){
    file.copy(from=script,to=newwd)
    file.copy(from=script,to=paste(newwd,'run.config.files/',sep=''))
  }
  
}

