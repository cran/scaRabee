## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo = FALSE, message = FALSE-------------------------------------------
require(scaRabee)

## ----eval = FALSE-------------------------------------------------------------
#  require(scaRabee)
#  scarabee.new(name='myanalysis',
#               path = 'some/target/directory/',
#               type = 'simulation',
#               method = 'population',
#               template = 'ode')

## ----eval=FALSE---------------------------------------------------------------
#  v <- rbind(ones(1,ntime))

## ----eval=FALSE---------------------------------------------------------------
#  v <- rbind((SD^2)*ones(1,ntime))

## ----eval=FALSE---------------------------------------------------------------
#  v <- rbind((CV^2)*(y[1,]^2))

## ----eval=FALSE---------------------------------------------------------------
#  v <- rbind((SD^2)*ones(1,ntime) + (CV^2)*(y[1,]^2))

## ----eval=FALSE---------------------------------------------------------------
#  demo(ex, package = 'scaRabee', echo = FALSE)

