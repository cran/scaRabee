
get.events <- function(bolus=NULL, scale=NULL){
  
  if (is.null(bolus) || dim(bolus)[1]==0)
    return(data.frame(var=character(0),
                      time=numeric(0),
                      value=numeric(0),
                      method=character(0)))
  
  events <- data.frame(var=paste('a',bolus$CMT,sep=''),
                       time=bolus$TIME,
                       value=bolus$AMT,
                       method=rep('add',dim(bolus)[1]))
                     
  
  if (!is.null(scale)){
    events$value <- events$value/scale[bolus$CMT]
  }
  
  return(events)
  
}