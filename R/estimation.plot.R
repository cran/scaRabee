
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

estimation.plot <- function(problem=NULL,Fit=NULL,files=NULL){

# Sets some default variable value
  nestparam <- length(Fit$estimations)
  
# Plots ... vs iterations

  # Determines the most esthetical arrangement of plots based on nestparam
  tmplayout <- get.layout(nplot=nestparam + 1)
  
  # imports and reshape data to be plotted
  data <- read.table(file=files$iter,
                     header=TRUE,
                     as.is=TRUE,
                     sep=',',
                     check.names=FALSE)
  est.names <- problem$init$names[which(problem$init$isfix==0)]
  data <- reshape(data[,c('Objective function',est.names)],
                  idvar='iteration',
                  times=c('Objective function',est.names),
                  timevar='variable',
                  varying=list(c('Objective function',est.names)),
                  direction='long')
  data$variable <- factor(data$variable,levels=unique(data$variable))
  data$iteration <- data$iteration-1
  names(data)[2] <- 'value'

  # Create storage list for plots
  plotlist <- list()
  
  # Objective function and parameters vs iteration plot
  pdf(file='01.Iterations.pdf',
      width=8.5,
      height=11)

  #trellis.par.set()
  myplot <- xyplot(value~iteration|variable,
                   data=data,
                   as.table=TRUE,
                   type='l',
                   col=4,
                   scales=list(x=list(relation='free'),
                               y=list(relation='free')),
                   strip=strip.custom(style=1,bg=0),
                   xlab='Iteration',
                   ylab='Value / Estimate',
                   main='Gradient monitoring',
                   layout=tmplayout)
   print(myplot)
   
   dev.off()

   plotlist[[1]] <- list(myplot)

# Plot diagnostic plots
  # imports data to be plotted
  data <- read.table(file=files$pred,
                     header=TRUE,
                     as.is=TRUE,
                     sep=',',
                     check.names=FALSE)

  names(data)[length(names(data))] <- 'WRES'

  # Find pred/residuals dose level indices
  resids <- find.id(data[,1])
  
  # Creates one sets of plot for each dose level
  doselevel <- problem$data$ids[,1] 

  for (i in 1:length(doselevel)){
    # Create placeholders in plotlist
    plotlist[[i+1]] <- list(NA,NA,NA,NA,NA)
    
    # Creates subset of pred/residuals datasets
    subdata <- data[which(data[,1]==i),]
    subdata$Output <- factor(subdata$Output,levels=unique(subdata$Output))
    
    outputs   <- unique(subdata$Output)
    noutput   <- length(outputs)
    tmplayout <- get.layout(nplot=noutput)

    # Model predictions vs independent variable
    if (i<10){
      figName <- sprintf('0%d.Dose.0%d.predictions.pdf',i,i)
    } else {
      figName <- sprintf('%d.Dose.%d.predictions.pdf',i,i)
    }
    
    pdf(file=figName,
        width=8.5,
        height=11)
        
    myplot <- xyplot(Observation+Prediction~Time|Output,
                      data=subdata,
                      as.table=TRUE,
                      type=c('p','l'),
                      pch=c(3,NULL),
                      col=c(1,4),
                      distribute.type=TRUE,
                      scales=list(x=list(relation='free'),
                                  y=list(relation='free')),
                      strip=strip.custom(var.name='Output',
                                         style=1,
                                         bg=0,
                                         sep=' ',
                                         strip.name=c(TRUE,TRUE)),
                      xlab='Time',
                      ylab='Observations / Predictions',
                      main=sprintf('Dose level %d',doselevel[i]),
                      layout=tmplayout)
    print(myplot)

    dev.off()

    plotlist[[i+1]][[1]] <- myplot
    
    #Model residuals
    if (i<10){
      figName <- sprintf('0%d.Dose.0%d.residuals.pdf',i,i)
    } else {
      figName <- sprintf('%d.Dose.%d.residuals.pdf',i,i)
    }
    
    pdf(file=figName,
        paper='letter')

    for (j in 1:noutput){
      rplot1 <- xyplot(Prediction~Observation,
                       data=subdata,
                       panel=function(x,y,...){
                         panel.xyplot(x,y,type='p',pch=3,col=4)
                         panel.abline(c(0,1),type='l',col=2)
                       },
                       strip=NULL,
                       xlab=sprintf('Observations - Output %d',outputs[j]),
                       ylab=sprintf('Predictions - Output %d',outputs[j]))
      rplot2 <- xyplot(WRES~Time,
                       data=subdata,
                       panel=function(x,y,...){
                         panel.xyplot(x,y,type='p',pch=3,col=4)
                         panel.abline(c(0,0),type='l',col=2)
                       },
                       strip=NULL,
                       xlab=sprintf('Time - Output %d',outputs[j]),
                       ylab=sprintf('Weighted residuals - Output %d',outputs[j]))
      rplot3 <- xyplot(WRES~Observation,
                       data=subdata,
                       panel=function(x,y,...){
                         panel.xyplot(x,y,type='p',pch=3,col=4)
                         panel.abline(c(0,0),type='l',col=2)
                       },
                       strip=NULL,
                       xlab=sprintf('Observations - Output %d',outputs[j]),
                       ylab=sprintf('Weighted residuals - Output %d',outputs[j]))
      rplot4 <- xyplot(WRES~Prediction,
                       data=subdata,
                       panel=function(x,y,...){
                         panel.xyplot(x,y,type='p',pch=3,col=4)
                         panel.abline(c(0,0),type='l',col=2)
                       },
                       strip=NULL,
                       xlab=sprintf('Predictions - Output %d',outputs[j]),
                       ylab=sprintf('Weighted residuals - Output %d',outputs[j]))
      print(rplot1,split=c(1,1,2,2),more=TRUE)
      print(rplot2,split=c(1,2,2,2),more=TRUE)
      print(rplot3,split=c(2,1,2,2),more=TRUE)
      print(rplot4,split=c(2,2,2,2),more=FALSE)

      plotlist[[i+1]][[2]] <- rplot1
      plotlist[[i+1]][[3]] <- rplot2
      plotlist[[i+1]][[4]] <- rplot3
      plotlist[[i+1]][[5]] <- rplot4
    }

    dev.off()
    
  }

  # Display plots in iteractive mode
  if (interactive()){
    par(ask=TRUE)
      print(plotlist[[1]][[1]])
      for (i in (1:length(doselevel))+1){
        for (j in 1:5){
          print(plotlist[[i]][[j]])
        }
      }
    par(ask=FALSE)
  }

  # Print a message to screen
  cat(sprintf('\n%s%s\n%s\n','Diagnostic figures have been created and saved in: ',
              getwd(),'You may open and edit them at your convenience.'))
                            
}
