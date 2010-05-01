
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

scarabee.new <- function(name = 'myanalysis',
                         path = getwd(),
                         type = 'simulation',
                         template = 'ode',
                         with.inputs = TRUE){

  # Check inputs
  if (!is.null(path)){
    if (!file.exists(path)){
      warning(paste('scarabee.skeleton: path did not exist and was coerced to the ',
                    'working directory',sep=''),
              call.=FALSE)
      path <- getwd()
    }
  }

  if (!is.null(name)){
    if (!is.vector(name,'character')){
      warning(paste('scarabee.skeleton: name was not a character vector and was ',
                    'coerced to \'myanalysis\'',sep=''),
              call.=FALSE)
      name <- 'myanalysis'
    }
    
    if (length(name)!=1){
      warning(paste('scarabee.skeleton: name contained more than one element, it was ',
                    'coerced to its first element',sep=''),
              call.=FALSE)
      name <- name[1]
    }

    name <- as.character(name)
    
  }

  if (!(type=='estimation' | type=='simulation')){
    warning(paste('scarabee.skeleton: type was neither \'estimation\' nor ',
                  '\'simulation\', it was coerced to \'simulation\'',sep=''),
            call.=FALSE)
    type <- 'simulation'
  }
  
  if (!is.null(template)){
    if (!is.vector(template,'character')){
      warning(paste('scarabee.skeleton: template was not a character vector and was ',
                    'coerced to \'ode\'',sep=''),
              call.=FALSE)
      template <- 'ode'
    }

    if (length(template)!=1){
      warning(paste('scarabee.skeleton: template contained more than one element and was ',
                    'coerced to its first element',sep=''),
              call.=FALSE)
      template <- template[1]
    }

    if (!is.element(template,c('explicit','ode','dde'))){
      warning(paste('scarabee.skeleton: template was not a valid selection and was ',
                    'coerced to \'ode\'',sep=''),
              call.=FALSE)
      template <- 'ode'
    }

  }

  if (!is.logical(with.inputs)){
    with.inputs <- TRUE
    warning('scarabee.skeleton: with.inputs coerced to TRUE.',
            call.=FALSE)
  }

  # Files names
  if (substring(path,nchar(path),nchar(path))!='/'){
    path <- paste(path,'/',name,'/',sep='')
  } else {
    path <- paste(path,name,'/',sep='')
  }
  
  ana.file <- paste(path,name,'.R',sep='')
  data.file <- paste(path,'data.csv',sep='')
  dosing.file <- paste(path,'dosing.csv',sep='')
  param.file <- paste(path,'initials.csv',sep='')
  cov.file <- paste(path,'covariates.csv',sep='')
  model.file <- paste(path,'model.definition/model.R',sep='')
  var.file <- paste(path,'model.definition/weighting.R',sep='')
  sec.file <- paste(path,'model.definition/secondary.R',sep='')

  # Get template text
  data('scarabee.template',
       'explicit.template',
       'ode.template',
       'dde.template',
       'secondary.template',
       'weighting.template')

  # Create main and model.definition directory
  if (!file.exists(path)){
    dir.create(path)
    dir.create(paste(path,'model.definition/',sep=''))
  } else {
    stop(sprintf(paste('scarabee.skeleton: The name argument should be different from \'%s\',\n',
                       'because the following directory already exists:\n%s\n',sep=''),
                       name,path),
         call.=FALSE)
  }

  # Create main analysis script
  tmp <- scarabee.template
  tmp[,1] <- as.character(tmp[,1])
  tmp <- sapply(tmp, function(x) gsub('@type@',paste('\'',type,'\'',sep=''),x))
  tmp <- sapply(tmp, function(x) gsub('@newline@','',x))
  write.table(tmp,
              file=ana.file,
              sep='\n',
              quote=FALSE,
              row.names=FALSE,
              col.names=FALSE)

  # Create input files, if requested
  if (with.inputs) {
    # Create data file
    write('Dose ID,Time,Y(1)',
          file=data.file,
          sep='\n')

    # Create dosing file
    write('Dose ID,Time,State,Bolus,Infusion Rate',
          file=dosing.file,
          sep='\n')

    # Create initials file
    write('Parameter,Type,Value,Fixed,Lower bound,Upper bound',
          file=param.file,
          sep='\n')

    # Create covariates file
    write('Dose ID,Time,Cov(1)',
          file=cov.file,
          sep='\n')
  }

  # Create variance/weighting file
  tmp <- weighting.template
  tmp[,1] <- as.character(tmp[,1])
  tmp <- sapply(tmp, function(x) gsub('@newline@','',x))
  write.table(tmp,
              file=var.file,
              sep='\n',
              quote=FALSE,
              row.names=FALSE,
              col.names=FALSE)

  # Create secondary file
  tmp <- secondary.template
  tmp[,1] <- as.character(tmp[,1])
  tmp <- sapply(tmp, function(x) gsub('@newline@','',x))
  write.table(tmp,
              file=sec.file,
              sep='\n',
              quote=FALSE,
              row.names=FALSE,
              col.names=FALSE)

  # Create model file
  if (template == 'explicit'){
    tmp <- explicit.template
  } else if (template == 'ode'){
    tmp <- ode.template
  } else if (template == 'dde'){
    tmp <- dde.template
  }
  tmp[,1] <- as.character(tmp[,1])
  tmp <- sapply(tmp, function(x) gsub('@newline@','',x))
  write.table(tmp,
              file=model.file,
              sep='\n',
              quote=FALSE,
              row.names=FALSE,
              col.names=FALSE)
              
  # Display message
  cat(sprintf('\nA new scaRabee working directory has been created at:\n%s\n\n',path))
  
}