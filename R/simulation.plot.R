
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

simulation.plot <- function(problem=NULL,Fsim=Fsim,files=files){

  # Get the number of different dose levels and states
  doselevel <- problem$data$ids[,1]
  ndose <- length(doselevel)
  nstate <- length(unique(Fsim$output))

  # Order Fsim output
  Fsim$output <- factor(Fsim$output,levels=unique(Fsim$output)) 

  # Create storage list for plots
  plotlist <- list()
  
  # Plotting loop
  for (i in 1:ndose){
    if (i<10){
      figName <- sprintf('0%d.sim.Dose.%d.pdf',i,i)
    } else {
      figName <- sprintf('%d.sim.Dose.%d.pdf',i,i)
    }

    # Open device
    if (nstate==1){
      pdf(file=figName,
          paper="letter")
    } else {
      pdf(file=figName,
          width=8.5,
          height=11)
    }

    # Extracts data from dose level
    tmp <- Fsim[Fsim$doseID==i,]

    # Create plots
    simplot <- xyplot(value~time|output,
                      data=tmp,
                      group=tmp$type,
                      as.table=TRUE,
                      panel = function(x,y,subscripts,...){
                        panel.xyplot(x,y,...,
                                     type=c('p','l'),
                                     pch=c(3,NULL),
                                     col=c(1,4),
                                     distribute.type=TRUE,
                                     subscripts=subscripts)
                      },

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
                      layout=get.layout(nstate))

    print(simplot)

    dev.off()

    plotlist[[i]] <- simplot
  }

  # Display plots in iteractive mode
  if (interactive()){
    par(ask=TRUE)
      for (i in 1:length(ndose)){
        print(plotlist[[i]])
      }
    par(ask=FALSE)
  }
  
  # Print a message to screen
  cat(sprintf('\n%s%s\n%s\n','Diagnostic figures have been created and saved in the ',
              'working directory.','You may open and edit them at your convenience.'))

}
