require(scaRabee)

# User-prompt: define target directory
if (interactive()){
  cat('\nExample 1 - Simulation of a model defined with closed form solution\n\n')

  repeat{
    wd <- readline('Enter a path to store the demo files:\n>')
    if (wd!=''){
      if (!file.exists(wd)){
        action <- readline(sprintf(paste('\nDirectory \'%s\' does not exist:\n',
                                          '  [c] Continue with current working directory: %s\n',
                                          '  [r] Retry\n',
                                          '  [a] Abort\n>',sep=''),wd,getwd()))
        if (action == 'a') {
          stop(call.=FALSE)
        } else if (action == 'c') {
          wd <- getwd()
          if (substring(wd,nchar(wd),nchar(wd))!='/'){
            wd <- paste(wd,'/',sep='')
          }
          options(warn=-1)
          nd <- try(file.create(paste(wd,'test.R',sep='')))
          options(warn=0)

          if (nd) { # User has permission on directory
            file.remove(paste(wd,'test.R',sep=''))
            break
          }
          cat('\nYou don\'t have permissions on this directory.\n')
        }
        
      } else {
        if (substring(wd,nchar(wd),nchar(wd))!='/'){
          wd <- paste(wd,'/',sep='')
        }
        options(warn=-1)
        nd <- try(file.create(paste(wd,'test.R',sep='')))
        options(warn=0)

        if (nd) { # User has permission on directory
          file.remove(paste(wd,'test.R',sep=''))
          break
        }
        cat('\nYou don\'t have permissions on this directory.\n')
      }
      
    } else {
    
      wd <- getwd()
      if (substring(wd,nchar(wd),nchar(wd))!='/'){
        wd <- paste(wd,'/',sep='')
      }
      options(warn=-1)
      if (file.exists(wd))
        nd <- try(file.create(paste(wd,'test.R',sep='')))
      options(warn=0)

      if (nd) { # User has permission on directory
        file.remove(paste(wd,'test.R',sep=''))
        break
      }

      cat('\nYou don\'t have permissions on this directory.\n')
      
    }
  }
} else {
  return(NULL)
}

# Set file files
old.wd <- getwd()
wd <- paste(wd,'example.1/',sep='')
ana.file <- paste(wd,'example.1.R',sep='')
data.file <- paste(wd,'data.csv',sep='')
dosing.file <- paste(wd,'dosing.csv',sep='')
param.file <- paste(wd,'initials.csv',sep='')
cov.file <- paste(wd,'covariates.csv',sep='')
model.file <- paste(wd,'model.definition/model.R',sep='')
var.file <- paste(wd,'model.definition/weighting.R',sep='')
sec.file <- paste(wd,'model.definition/secondary.R',sep='')

# Copy files
data(example1.covariates,
     example1.data,
     example1.dosing,
     example1.initials,
     example1.model,
     example1,
     example1.secondary,
     example1.weighting)

# Create main and model.definition directory
if (file.exists(wd)) {
  stop(sprintf(paste('\nDirectory \'%s\' already exists.\nDemo aborted. ',
                     'Please retry using a different target directory.\n',sep=''),wd),
       call.=FALSE)
}

dir.create(wd)
dir.create(paste(wd,'model.definition/',sep=''))
setwd(wd)

# Create main analysis script
tmp <- example1
tmp[,1] <- as.character(tmp[,1])
tmp <- sapply(tmp, function(x) gsub('@newline@','',x))
write.table(tmp,
            file=ana.file,
            sep='\n',
            quote=FALSE,
            row.names=FALSE,
            col.names=FALSE)

# Create input files
  # Create data file
  tmp <- example1.data
  tmp[,1] <- as.character(tmp[,1])
  tmp <- sapply(tmp, function(x) gsub('@newline@','',x))
  write.table(tmp,
              file=data.file,
              sep='\n',
              quote=FALSE,
              row.names=FALSE,
              col.names=FALSE)

  # Create dosing file
  tmp <- example1.dosing
  tmp[,1] <- as.character(tmp[,1])
  tmp <- sapply(tmp, function(x) gsub('@newline@','',x))
  write.table(tmp,
              file=dosing.file,
              sep='\n',
              quote=FALSE,
              row.names=FALSE,
              col.names=FALSE)

  # Create initials file
  tmp <- example1.initials
  tmp[,1] <- as.character(tmp[,1])
  tmp <- sapply(tmp, function(x) gsub('@newline@','',x))
  write.table(tmp,
              file=param.file,
              sep='\n',
              quote=FALSE,
              row.names=FALSE,
              col.names=FALSE)

  # Create covariates file
  tmp <- example1.covariates
  tmp[,1] <- as.character(tmp[,1])
  tmp <- sapply(tmp, function(x) gsub('@newline@','',x))
  write.table(tmp,
              file=cov.file,
              sep='\n',
              quote=FALSE,
              row.names=FALSE,
              col.names=FALSE)

# Create variance/weighting file
tmp <- example1.weighting
tmp[,1] <- as.character(tmp[,1])
tmp <- sapply(tmp, function(x) gsub('@newline@','',x))
write.table(tmp,
            file=var.file,
            sep='\n',
            quote=FALSE,
            row.names=FALSE,
            col.names=FALSE)

# Create secondary file
tmp <- example1.secondary
tmp[,1] <- as.character(tmp[,1])
tmp <- sapply(tmp, function(x) gsub('@newline@','',x))
write.table(tmp,
            file=sec.file,
            sep='\n',
            quote=FALSE,
            row.names=FALSE,
            col.names=FALSE)

# Create model file
tmp <- example1.model
tmp[,1] <- as.character(tmp[,1])
tmp <- sapply(tmp, function(x) gsub('@newline@','',x))
write.table(tmp,
            file=model.file,
            sep='\n',
            quote=FALSE,
            row.names=FALSE,
            col.names=FALSE)

# Run example 1
source(ana.file)
setwd(old.wd)