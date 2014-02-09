
\name{ode.utils}
\alias{create.intervals}
\alias{de.output}
\alias{derived.parms}
\alias{init.cond}
\alias{init.update}
\alias{input.scaling}
\alias{make.dosing}
\alias{ode.syst}
\title{
  Utility Functions for Ordinary Differential Equation Systems
}
\description{
  This is a collection of utility functions called by \code{ode.model} (and for 
  some by \code{dde.model} when a model defined by ordinary (or delay)
  differential equations is evaluated. None of these functions is typically 
  called directly by users.
}
\usage{
  ode.syst(t = NULL,
           y = NULL,
           parms = NULL,
           derparms = NULL,
           codeode = NULL,
           dosing = NULL,
           has.dosing = NULL,
           dose.states = NULL,
           covdata = NULL,
           scale = NULL,
           check = FALSE)
  create.intervals(xdata = NULL,
                   bolus = NULL,
                   infusion = NULL)
  de.output(f = NULL,
            parms = NULL,
            derparms = NULL,
            codeoutput = NULL,
            dosing = NULL,
            xdata = NULL,
            check = FALSE)
  derived.parms(parms = NULL,
                covdata,
                codederiv = NULL,
                check = FALSE)
  init.cond(parms = NULL,
            derparms = NULL,
            codeic = NULL,
            dosing = NULL,
            check = FALSE)
  input.scaling(parms = NULL,
                derparms = NULL,
                codescale = NULL,
                ic = NULL,
                check = FALSE)
  make.dosing(allparms = NULL,
              bolus = NULL,
              infusion = NULL,
              check = FALSE)
  init.update(a = NULL,
              t = NULL,
              dosing = NULL,
              scale = NULL)
}
\arguments{
  \item{t}{A scalar or a vector of numerical time values.}
  \item{y}{A vector of system state values.}
  \item{parms}{A vector of primary parameters.}
  \item{derparms}{A list of derived parameters, specified in the $DERIVED block
    of code.}
  \item{codeode}{The content of the R code specified within the $ODE block 
    in the model file.}
  \item{dosing}{A data.frame of dosing information created by \code{make.dosing}
    from instantaneous and zero-order inputs into the system and containing the 
    following columns: \describe{
      \item{TIME}{Dosing event times.}
      \item{CMT}{State where the input should be assigned to.}
      \item{AMT}{Amount that should be assigned to system \code{state} at the
        corresponding \code{TIME}.}
      \item{RATE}{Rate of input that should be assigned to system \code{CMT} at 
        the corresponding \code{TIME}. See 
        \code{vignette('scaRabee',package='scaRabee')} for more details about
        the interpolation of the input rate at time not specified in
        \code{dosing}.}
      \item{TYPE}{An indicator of the type of input. \code{TYPE} is set to 1 if
        the record in \code{dosing} correspond an original bolus input; it is 
        set to 0 otherwise.}
    }
  }
  \item{has.dosing}{A logical variable, indicating whether any input has to
    be assigned to a system state.}
  \item{dose.states}{A vector of integers, indicating in which system state one
    or more doses have to be assigned to.}
  \item{covdata}{A matrix of covariate data extracted from the dataset.}
  \item{scale}{A vector of system scale, typically returned by 
    \code{input.scaling}}
  \item{check}{An indicator whether checks should be performed to validate 
     function inputs}
  \item{xdata}{A vector of times at which the system is being evaluated.}
  \item{bolus}{bij x 4 data.frame providing the instantaneous inputs for a 
    treatment and individual.}
  \item{infusion}{fij x (4+c) data.frame providing the zero-order inputs for a
    treatment and individual.}
  \item{f}{A matrix of time (first row) and system predictions. In the 
    \code{de.output} function, the first row is deleted so that \code{f} has
    the same number of rows as in \code{dadt} defined in the $ODE or $DDE block.}
  \item{codeoutput}{The content of the R code specified within the $OUTPUT block 
    in the model file.}
  \item{codederiv}{The content of the R code specified within the $DERIVED block 
    in the model file.}
  \item{codeic}{The content of the R code specified within the $IC block in the 
    model file.}
  \item{codescale}{The content of the R code specified within the $SCALE block 
    in the model file.}
  \item{ic}{A vector of initial conditions, typically returned by \code{init.cond}}
  \item{allparms}{A vector of parameters (primary+derived).}
  \item{a}{A vector of system state values, similar to \code{y}.}
}
\details{
  \code{ode.syst} is the function which actually evaluates the system of
  ordinary differential equations specified in the $ODE block.
  
  \code{create.intervals} is a function that allows the overall integration 
  interval to be split into sub-intervals based upon dosing history.
  This allows for the exact implementation of bolus inputs into the system. It
  determines the number of unique bolus dosing events there is by system state 
  in \code{dosing}. It then creates the sub-intervals using these unique event 
  times. If the first dosing events occurs after the first observation time, an 
  initial sub-interval is added.
  
  \code{de.output} is the function which evaluates the code specified in the 
  $OUTPUT block and, thus, defines the output of the model.

  \code{derived.parms} is the function which evaluates the code provided in the
  $DERIVED block and calculate derived parameters. It prevents primary 
  parameters and covariates from being edited.

  \code{init.cond} is the function which evaluates the code provided in the $IC
  block, and, thus, defines the initial conditions of the system.
  
  \code{input.scaling} is the function which evaluates the code provided in the
  $SCALE block, and, thus, defines how system inputs are scaled.
  
  \code{make.dosing} is the function which processes the instantaneous and 
  zero-order inputs provided in the dataset and creates the \code{dosing} 
  object. This function also implements absorption lags if the user included
  ALAGx parameters in \code{parms} or \code{derpamrs}.
  
  \code{init.update} is a function that updates the system states at the 
  beginning of each integration interval created by \code{create.intervals} to 
  provide an exact implementation of bolus inputs into the system.
}
\seealso{
  \code{\link[scaRabee]{dde.model}}
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }