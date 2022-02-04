%Copyright (c) 2009-2022 Sebastien Bihorel
%All rights reserved.
%
%This file is part of scaRabee.
%
%    scaRabee is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    scaRabee is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with scaRabee.  If not, see <http://www.gnu.org/licenses/>.
%

\name{fitmle}
\alias{fitmle}
\title{
  Maximum Likelihood Estimator
}
\description{
  \code{fitmle} is a secondary function called during estimation runs. It 
  performs the optimization of the model parameters by the method of the maximum
  likelihood, i.e. the minimization of an objective function defined as the 
  exact negative log likelihood of the observed data, given the structural 
  model, the model of residual variability, and the parameter estimates. This 
  minimization is performed by the Nelder-Mead simplex algorithm implemented in 
  \code{fminsearch} from the \pkg{neldermead} package. \code{fitmle} is 
  typically not called directly by users.
}
\usage{
  fitmle(problem = NULL,
         estim.options = NULL,
         files = NULL)
}
\arguments{
  \item{problem}{A list containing the following levels:\describe{
    \item{data}{A list containing as many levels as there are treatment levels 
      for the subject (or population) being evaluated, plus the \code{trts} 
      level listing all treatments for this subject (or population), and the 
      \code{id} level giving the identification number of the subject (or set to
      1 if the analysis was run at the level of the population.
      
      Each treatment-specific level is a list containing the following levels: 
      \describe{
        \item{cov}{mij x 3 data.frame containing the times of observations of the
          dependent variables (extracted from the TIME variable), the indicators
          of the type of dependent variables (extracted from the CMT variable),
          and the actual dependent variable observations (extracted from the 
          DV variable) for this particular treatment.}
        \item{cov}{mij x c data.frame containing the times of observations of 
          the dependent variables (extracted from the TIME variable) and all the
          covariates identified for this particular treatment.} 
        \item{bolus}{bij x 4 data.frame providing the instantaneous inputs 
          for a treatment and individual.}
        \item{infusion}{fij x (4+c) data.frame providing the zero-order inputs for
          a treatment and individual.}
        \item{trt}{the particular treatment identifier.}}
    }
    \item{method}{A character string, indicating the scale of the analysis. Should
       be 'population' or 'subject'.}
    \item{init}{A data.frame of parameter data with the following columns:
      'names', 'type', 'value', 'isfix', 'lb', and 'ub'.}
    \item{debugmode}{Logical indicator of debugging mode.}
    \item{modfun}{Model function.}
    }
  }
  \item{estim.options}{A list of estimation options containing two elements
    \code{maxiter} (the maximum number of iterations) and \code{maxeval} (the
    maximum number of function evaluations).
  }
  \item{files}{A list of input used for the analysis. The following elements are
    expected and none of them could be null: \describe{
      \item{data}{A .csv file located in the working directory, which contains
        the dosing information, the observations of the dependent variable(s)
        to be modeled, and possibly covariate information. The expected format 
        of this file is described in details in \code{vignette('scaRabee',
        package='scaRabee')}.}
      \item{param}{A .csv file located in the working directory, which contains
        the initial guess(es) for the model parameter(s) to be optimized or used
        for model simulation. The expected format of this file is described in
        details in \code{vignette('scaRabee',package='scaRabee')}.}
      \item{model}{A text file located in the working directory, which defines 
        the model. Models specified with explicit, ordinary or delay 
        differential equations are expected to respect a certain syntax and 
        organization detailed in \code{vignette('scaRabee',package='scaRabee')}.}
      \item{iter}{A .csv file reporting the values of the objective function
        and estimates of model parameters at each iteration.}
      \item{report}{A text file reporting for each individual in the dataset the
        final parameter estimates for structural model parameters, residual 
        variability and secondary parameters as well as the related statistics 
        (coefficients of variation, confidence intervals, covariance and 
        correlation matrix).}
      \item{pred}{A .csv file reporting the predictions and calculated residuals
        for each individual in the dataset.}
      \item{est}{A .csv file reporting the final parameter estimates for each
        individual in the dataset.}
      \item{sim}{A .csv file reporting the simulated model predictions for each 
        individual in the dataset. (Not used for estimation runs).}
    }
  }
}
\value{
  Return a list with two elements: \code{estimations} which contains the vector
  of final parameter estimates and \code{fval} the minimal value of the
  objective function.
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
  
  Pawel Wiczling
}
\seealso{
  \code{\link[neldermead]{fminsearch}}
}

\keyword{ method }
