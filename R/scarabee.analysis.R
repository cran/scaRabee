
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

scarabee.analysis <- function(files=NULL,
                              states=NULL,
                              runtype=NULL,
                              debugmode=FALSE,
                              estim.options=NULL,
                              analysis='myanalysis') {

  oldwd <- getwd()

  anatry <- try({

    # Check files
    if (!is.null(files)){
      file.names <- c('data','param','dose','cov','model','var','sec')
      if (is.list(files)){
        if (any(is.na(match(names(files),file.names))))
          stop(sprintf(paste('scarabee.analysis: files must be a list with',
                            'the following items:\n%s %s %s %s %s %s %s',sep=''),
                      'data','param','dose','cov','model','var','sec'),
              call.=FALSE)
        if (any(sapply(file,is.null))){
          stop('scarabee.analysis: one or more element of files are NULL.',
              call.=FALSE)
        }
      } else {
        stop('scarabee.analysis: files must be a list.',
            call.=FALSE)
      }
    } else {
      stop('scarabee.analysis: files cannot be NULL.',
          call.=FALSE)
    }

    # Check states
    if (!is.numeric(states)) {
      stop('scarabee.analysis: states should be a vector of numerical values.',
          call.=FALSE)
    } else {
      if (any(states<=0)){
        stop('scarabee.analysis: states should be a vector of positive numerical values.',
            call.=FALSE)
      }
    }

    # Check runtype
    if (runtype=='estimation') {
      issim <- 0
    } else {
      if (runtype=='simulation') {
        issim = 1
      } else {
        stop('scarabee.analysis: runtype must either be estimation or simulation.',
            call.=FALSE)
      }
    }

    # Check debugmode
    if (!is.logical(debugmode)){
      stop('scarabee.analysis: debugmode must either be logical variable.',
          call.=FALSE)
    }

    # Check estim values
    if (is.numeric(estim.options$maxiter)!=1 | is.numeric(estim.options$maxfunc)!=1)
      stop('scarabee.analysis: estim.options$maxiter and estim.options$maxfunc should be numerical values.',
          call.=FALSE)
    if (length(estim.options$maxiter)!=1 | length(estim.options$maxfunc)!=1)
      stop('scarabee.analysis: estim.options$maxiter and estim.options$maxfunc should only contain one value.',
          call.=FALSE)

    # Check analysis
    if (is.na(analysis))
      analysis <- 'myanalysis'

  ################################################################################
    # trim the .m extension in modelfile, varfile, and secfile
    files$model <- gsub('[.]R','',files$model)
    files$var   <- gsub('[.]R','',files$var)
    files$sec   <- gsub('[.]R','',files$sec)

    files$iter   <- paste(analysis,'.iterations.csv',sep='')
    files$report <- paste(analysis,'.report.txt',sep='')
    files$pred   <- paste(analysis,'.predictions.csv',sep='')
    files$sim    <- paste(analysis,'.simulations.csv',sep='')

    # Create working/backup directory
    scarabee.directory(curwd=getwd(),
                      files=files,
                      runtype=runtype,
                      analysis=analysis)

  ################################################################################
    # Model input importation step
    nstate <- length(states)

    # 0- parse the model definition files
    for (file in list.files('model.definition/', pattern='\\.[Rr]$')){
      source(paste('model.definition/',file,sep=''))
    }

    # 1- imports data to be fitted
    data <- as.matrix(read.csv(file=files$data,
                              header=TRUE,
                              as.is=TRUE))

    # 2- imports parameter file (names + initial guess)
    param <- read.csv(file=files$param,
                      header=TRUE,
                      as.is=TRUE,
                      col.names=c('names','type','value','isfix','lb','ub'))

    if (dim(param)[1]==0) {
      stop(sprintf('myanalysis: The matrix of parameters created from %s is empty. Please, check the content of your file.',
                  files$par),
          call.=FALSE)
    }

    if (any(param[,3]< 0))
      stop('scarabee.analysis: Negative values for model parameters are not allowed.',
          call.=FALSE)

    # 3- imports dosing history
    dosing <- read.csv(file=files$dose,
                      header=TRUE,
                      as.is=TRUE)

    # 4- imports covariate data
    covariates <- read.csv(file=files$cov,
                          header=TRUE,
                          as.is=TRUE)

    #########################################################################
    # Checks if the IDs used in the dosing, data, covariates are the same
    dataIDs   <- find.id(data[,1])
    dosingIDs <- find.id(dosing[,1])
    covIDs    <- find.id(covariates[,1])

    if (dim(covariates)[1]!=0){
      if (sum(dataIDs[,1]!=dosingIDs[,1])!=0 | sum(dataIDs[,1]!=covIDs[,1])!=0){
        stop(paste('scarabee.analysis: The levels of IDs used in the data, dosing, and covariate',
                  'files should be the same.'),
            call.=FALSE)
      }
    } else {
      if (sum(dataIDs[,1]!=dosingIDs[,1])!=0){
        stop(paste('scarabee.analysis: The levels of IDs used in the data and dosing',
                  'files should be the same.'),
            call.=FALSE)
      }
    }

    #########################################################################
    # Checks if ID set of data contain at least 2 time points
    for (id in dataIDs[,1]){
      if (length(which(data[,1]==id))<2){
        stop(sprintf(paste('scarabee.analysis: The matrix of data created from %s ',
                          'should contain at least two time points per Dose ID.\n',
                          'Please, check the content of your file.',sep=''),
                    files$data),
            call.=FALSE)
      }
    }

    #########################################################################
    # Definition of some variables to pass to model and estimation functions
    problem <- list()
    problem$data$xdata     <- transpose(data[,2])
    problem$data$ydata     <- transpose(data[,3:(nstate+2)])
    problem$data$ids       <- dataIDs
    problem$dosing$history <- dosing[,2:5]
    problem$dosing$ids     <- dosingIDs
    problem$cov$data       <- covariates[,-1]
    problem$cov$ids        <- covIDs
    problem$states         <- states
    problem$init           <- param
    problem$debugmode      <- debugmode
    problem$modfun         <- files$model
    problem$varfun         <- files$var
    problem$secfun         <- files$sec


    # Divert the output if necessary
    if (!interactive())
      sink(file=paste(analysis,'.log',sep=''))

    if (issim == 0) {
      # Estimation run
      cat('Optimization started\n')

      # Initializing and preparing iteration log and report files
      init.report(param=param,files=files)

      # Fittings;
        # parameter optimization
        Fit <- fitmle(problem=problem,estim.options=estim.options,files=files)

        # covariance step
        Fit <- fitmle.cov(problem=problem,Fit=Fit)

      # Edition of the report file
      Fit <- finalize.report(problem=problem,Fit=Fit,files=files)

      # Creation of a file containing final model predictions, residuals and
      # weighted residual for all states
      residual.report(problem=problem,Fit=Fit,files=files)

      # Creation of some diagnostic plots
      estimation.plot(problem=problem,Fit=Fit,files=files)

      cat('\nOptimization terminated. Please, see report.txt file for more details.\n')

    } else {
      # Simulation run
      cat(paste('Simulation started at: ', Sys.time(),
                '. This operation can take a few momemt.\n',sep=''))

      # Evaluates the model given the parameter estimates and saves to file
      Fsim <- simulation.report(problem=problem,files=files)

      # Plot simulations
      simulation.plot(problem=problem,Fsim=Fsim,files=files)

      cat(paste('\nSimulation complete at:',Sys.time(),'\n',sep=''))
      cat(paste('Model predictions were saved into ',files$sim,'.\n',sep=''))
    }

    # Wrap-up things
    scarabee.clean(files=files,analysis=analysis)

    if (!interactive())
      sink()
  })

  if (class(anatry)=="try-error") setwd(oldwd)
  
}

