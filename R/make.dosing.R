
#Copyright (c) 2009-2011 Sebastien Bihorel
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

make.dosing <- function(derparms=NULL,
                        bolus=NULL,
                        infusion=NULL,
                        check=FALSE){
  
  # Input validation
  if (check){
    if (is.null(bolus) | is.null(infusion))
      stop('bolus or infusion argument is NULL.')
    
    if (size(infusion,1)>=1){ 
      if (any(c(-1,-2)%in%infusion$RATE) &
          length(derparms)==0)
        stop(paste('derparms argument (derived parameters) cannot be NULL when',
                   'the rate or the\n  duration of infusion(s) are estimated.'))
    }
    
    # Check infusion and bolus
    if (any(infusion$RATE==-1)){
      # Check for expected parameters
      cmts <- unique(infusion[which(infusion$RATE==-1),'CMT'])
      expected <- paste('R',cmts,sep='')
      if (!all(exists(expected))){
        missings <- expected[which(!exists(expected))]
        stop(paste('variable(s) expected from dosing history not defined:\n  ',
                   paste(missings,collapse=', '),sep=''))
      }
      for (cmt in cmts){
        rcmt <- paste('R',cmt,sep='')
        # Check dimension of expected parameter
        if (size(eval(parse(text=rcmt)),1)>1){
          stop(paste(rcmt,' variable has invalid dimensions.',sep=''))
        }
        if (length(eval(parse(text=rcmt)))%in%c(1,size(infusion,2))){
          stop(paste(rcmt,' variable has invalid dimensions.',sep=''))
        }
      }
    } else if (any(infusion$rate==-2)) {
      # Check for expected parameters
      cmts <- unique(infusion[which(infusion$RATE==-2),'CMT'])
      expected <- paste('D',cmts,sep='')
      if (!all(exists(expected))){
        missings <- expected[which(!exists(expected))]
        stop(paste('variable(s) expected from dosing history not defined:\n  ',
                   paste(missings,collapse=', '),sep=''))
      }
      
      for (cmt in cmts){
        dcmt <- paste('D',cmt,sep='')
        # Check dimension of expected parameter
        if (size(eval(parse(text=dcmt)),1)>1){
          stop(paste(dcmt,' variable has invalid dimensions.',sep=''))
        }
        if (!(length(eval(parse(text=dcmt)))%in%c(1,size(infusion,2)))){
          stop(paste(dcmt,' variable has invalid dimensions.',sep=''))
        }
      }
    }
  }
  
  # Get estimated rate
  if (any(infusion$RATE==-1)){
    cmts <- unique(infusion[which(infusion$RATE==-1),'CMT'])
    
    for (cmt in cmts){
      rcmt <- paste('derparms$R',cmt,sep='')
      index <- which(infusion$CMT==cmt)
      if (length(eval(parse(text=rcmt)))==1){
        infusion$RATE[index] <- eval(parse(text=rcmt))
      } else {
        infusion$RATE[index] <- eval(parse(text=rcmt))[index]
      }
    }
  } else if (any(infusion$RATE==-2)) {
    cmts <- unique(infusion[which(infusion$RATE==-2),'CMT'])
    
    for (cmt in cmts){
      dcmt <- paste('derparms$D',cmt,sep='')
      index <- which(infusion$CMT==cmt)
      if (length(eval(parse(text=dcmt)))==1){
        infusion$RATE[index] <- infusion$AMT[index]/eval(parse(text=dcmt))
      } else {
        infusion$RATE[index] <- 
          infusion$AMT[index]/eval(parse(text=dcmt))[index]
      }
    }
  }
  
  # Subset infusion to first 4 columns (i.e, remove covariates)
  infusion <- infusion[,1:4]
  
  # Create dosing
  dosing <- data.frame(TIME=numeric(0),
                       CMT=numeric(0),
                       AMT=numeric(0),
                       RATE=numeric(0),
                       TYPE=numeric(0))
    
  cmts <- unique(c(unique(bolus$CMT),unique(infusion$CMT)))
  
  for (cmt in cmts){
    if (size(infusion,1) > 0){  # There is infusion data
      tmp.data <- convert.infusion(infusion)
      tmp.data$TYPE <- 0
      if(size(bolus,1) > 0){     # There is bolus data (need to infer rate)
        bolus$RATE <- approx(x=tmp.data$TIME,
                             y=tmp.data$RATE,
                             xout=bolus$TIME,
                             method='constant',
                             yleft=0,
                             rule=2,
                             f=0,
                             ties='ordered')$y
        bolus$TYPE <- 1
        tmp.data <- rbind(tmp.data, bolus)
        tmp.data <- tmp.data[order(tmp.data$TIME,
                                   tmp.data$CMT,
                                   tmp.data$TYPE),]
      }
      tmp.data <- tmp.data[,c('TIME','CMT','AMT','RATE','TYPE')]
    } else {                    # There is no infusion data
      tmp.data <- bolus[,c('TIME','CMT','AMT','RATE')]
      tmp.data$TYPE <- 1
    }
    dosing <- rbind(dosing,tmp.data)
  }
  dosing <- dosing[order(dosing$TIME,
                         dosing$CMT,
                         dosing$TYPE),]
  
  return(as.matrix(dosing))
}
