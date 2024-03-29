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

\name{scarabee.read.data}
\alias{scarabee.read.data}
\title{
  Read scaRabee Data File
}
\description{
  \code{scarabee.read.data} is a secondary function called at each 
  \pkg{scaRabee} run. It reads and processes the data contained in the specified
  data file.\code{scarabee.read.data} is typically not called directly by users.
}
\usage{
  scarabee.read.data(files = NULL,
                     method = NULL)
}
\arguments{
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
        and estimates of model parameters at each iteration.  (Not used for 
        simulation runs).}
      \item{report}{A text file reporting for each individual in the dataset the
        final parameter estimates for structural model parameters, residual 
        variability and secondary parameters as well as the related statistics 
        (coefficients of variation, confidence intervals, covariance and 
        correlation matrix). (Not used for simulation runs).}
      \item{pred}{A .csv file reporting the predictions and calculated residuals
        for each individual in the dataset. (Not used for simulation runs).}
      \item{est}{A .csv file reporting the final parameter estimates for each
        individual in the dataset. (Not used for simulation runs).}
      \item{sim}{A .csv file reporting the simulated model predictions for each 
        individual in the dataset. (Not used for estimation runs).}
    }
  }
  \item{method}{A character string, indicating the scale of the analysis. Should
    be 'population' or 'subject'.}
}
\value{
  Return a list with 2 levels: \describe{
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
        \item{trt}{the particular treatment identifier.}
      }
    }
    \item{new}{A logical indicator defining whether or not a modified data file
      has been created based upon the original file. This is the case if and 
      if only the time of first data record for one or more individuals in the 
      original data file is not 0. The new data file is created such as the
      TIME variable is modified so that time of the first data record for all 
      individuals is 0; the time of later records is modified accordingly.
    }
  }
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[scaRabee]{scarabee.analysis}}
}
\keyword{ method }
