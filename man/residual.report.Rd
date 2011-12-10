%Copyright (c) 2009-2011 Sebastien Bihorel
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

\name{residual.report}
\alias{residual.report}
\title{
  Creation of Prediction & Residual Report
}
\description{
  \code{residual.report} is a secondary function called at the end of the 
  estimations runs. It creates a file containing the predictions, residuals and
  weighted residuals at all observation time points. \code{residual.report} is 
  typically not called directly by users.
}
\usage{
  residual.report(problem = NULL,
                  Fit = NULL,
                  files = NULL)
}
\arguments{
  \item{problem}{A list containing the following levels:\describe{
    \item{code}{A list of R code extracted from the model file. Depending on 
      content of the model file, the levels of this list could be: template,
      derived, lags, ode, dde, output, variance, and/or secondary.}
    \item{method}{A character string, indicating the scale of the analysis. Should
      be 'population' or 'subject'.}
    \item{init}{A data.frame of parameter data with the following columns:
      'names', 'type', 'value', 'isfix', 'lb', and 'ub'.}
    \item{debugmode}{Logical indicator of debugging mode.}
    \item{modfun}{Model function.}
    \item{data}{A list which content depends on the scope of the analysis. If 
      the analysis was run at the level of the subject, \code{data} contains as 
      many levels as the number of subjects in the dataset, plus the \code{ids}
      level containing the vector of identification numbers of all subjects 
      included in the analysis population. If the analysis was run at the level 
      of the population, \code{data} contains only one level of data and 
      \code{ids} is set to 1.
      
      Each subject-specific level contains as many levels as there are treatment
      levels for this subject, plus the \code{trts} level listing all treatments
      for this subject, and the \code{id} level giving the identification number
      of the subject. 
      
      Each treatment-specific levels is a list containing the following levels: 
      \describe{
        \item{cov}{mij x 3 data.frame containing the times of observations of the
          dependent variables (extracted from the TIME variable), the indicators
          of the type of dependent variables (extracted from the CMT variable),
          and the actual dependent variable observations (extracted from the 
          DV variable) for this particular treatment and this particular 
          subject.}
        \item{cov}{mij x c data.frame containing the times of observations of 
          the dependent variables (extracted from the TIME variable) and all the
          covariates identified for this particular treatment and this 
          particular subject.} 
        \item{bolus}{bij x 4 data.frame providing the instantaneous inputs for
          a treatment and individual.}
        \item{infusion}{fij x (4+c) data.frame providing the zero-order inputs for
          a treatment and individual.}
        \item{trt}{the particular treatment identifier.}}
      }
    }
  }
  \item{Fit}{A list containing the following elements:\describe{
    \item{estimations}{The vector of final parameter estimates.}
    \item{fval}{The minimal value of the objective function.}
    \item{cov}{The matrix of covariance for the parameter estimates.}
    \item{orderedestimations}{A data.frame with the same structure as
      \code{problem$init} but only containing the sorted estimated estimates.
      The sorting is performed by \code{order.param.list}.}
    \item{cor}{The upper triangle of the correlation matrix for the parameter
      estimates.}
    \item{cv}{The coefficients of variations for the parameter estimates.}
    \item{ci}{The confidence interval for the parameter estimates.}
    \item{AIC}{The Akaike Information Criterion.}
    \item{sec}{A list of data related to the secondary parameters, containing
      the following elements:\describe{
        \item{estimates}{The vector of secondary parameter estimates calculated
          using the initial estimates of the primary model parameters.}
        \item{estimates}{The vector of secondary parameter estimates calculated
          using the final estimates of the primary model parameters.}
        \item{names}{The vector of names of the secondary parameter estimates.}
        \item{pder}{The matrix of partial derivatives for the secondary
          parameter estimates.}
        \item{cov}{The matrix of covariance for the secondary parameter
          estimates.}
        \item{cv}{The coefficients of variations for the secondary parameter
          estimates.}
        \item{ci}{The confidence interval for the secondary parameter
          estimates.}
        }
      }
    \item{orderedfixed}{A data.frame with the same structure as
      \code{problem$init} but only containing the sorted fixed estimates.
      The sorting is performed by \code{order.param.list}.}
    \item{orderedinitial}{A data.frame with the same content as
      \code{problem$init} but sorted by \code{order.param.list}.}
    }
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
  Creates the prediction and residual report in the run directory.
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }
