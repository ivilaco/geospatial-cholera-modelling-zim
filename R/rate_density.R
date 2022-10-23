#' The rate_density function
#'
#' This function builds off the ppmify function from Nick Golding's ppmify package, 
#' converting your case data into a data.frame suitable for applying Poisson regression models.
#' @name rate_density
#' @param points data.frame object with at least `lng` and `lat` of cases
#' An optional `timeslice` integer field can be included denoting the time period in which the point was observed
#' @param exposure URL to rasterLayer of exposure (to be used as offset). Required. 
#' Raster representing the population over which points arose. Currently 
#' only accepts a single raster which is used across all time periods.
#' @param covariates Optional URL to rasterLayer or rasterStack of additional covariates to include. 
#' Should be at the same resolution/extent as `exposure`. If not, will be resampled to the same 
#' resolution and extent as `exposure`.
#' @param covariates_dynamic Optional named list of rasterStack objects corresponding to timeslice-specific covariates to include 
#' in the model. 
#' @param approx_num_int_points Approximate number of integration points to use. Defaults to 5,000.
#' @k k parameter to be used in `mgcv::gam`
#' @k_covar k parameter to be used for covariates
library(sf)
library(raster)
source('R/space_time_ppmify.R')

rate_density <- function(points,
                         exposure,
                         covariates = NULL,
                         covariates_dynamic = NULL,
                         space_time = FALSE,
                         prediction_periods = "last",
                         prediction_exposure = exposure,
                         fixed_static_covariates = NULL,
                         fixed_dynamic_covariates = NULL,
                         k = 150,
                         k_covar = 4,
                         approx_num_int_points = 5000,
                         smooth_effects = TRUE,
                         discrete = TRUE,
                         discrete_prediction = discrete,
                         debug = FALSE,
                         additional_formula_terms=NULL){

  # Define exposure raster
  exposure <- stack(exposure)
  
  if(!is.null(covariates)){
  covariates <- stack(covariates)
  }
  
  # Convert to SF object
  if(space_time){
  points$period <- points$timeslice
  }else{
    points$period <- 1
  }
  points_sf <- st_as_sf(points, coords = c('lng', 'lat'))

  # Get ppm df
  ppm_df <- space_time_ppmify(points = points_sf,
                              covariates = covariates,
                              covariates_dynamic = covariates_dynamic,
                              exposure = exposure,
                              #prediction_exposure = prediction_exposure,
                              space_time = space_time,
                              fixed_static_covariates = fixed_static_covariates,
                              approx_num_int_points = approx_num_int_points,
                              prediction_stack=TRUE)

  # Fit model
  periods <- sort(unique(points$period))
  time_knots <- length(periods) - 1
  if(time_knots>10){
    time_knots <- 10
  }
  if(space_time){
    k <- c(k, time_knots)
    form <- paste('outcome ~ te(x, y, period, k=k, bs = c("gp", "cr"), d=c(2,1))')
  }else{
    form <- paste('outcome ~ s(x, y, k=k, bs="gp")')
  }

  # Add covariates if provided
  if(!is.null(covariates) | !is.null(covariates_dynamic)){
    covs <- c(names(covariates), names(covariates_dynamic))   
    
    if(smooth_effects){
      form <- paste(form, '+', paste('s(', covs,
                                              ', bs="gp", k=k_covar)',
                                              collapse = '+'))
    }else{
      form <- paste(form, '+', paste(covs,
                                     collapse = '+')) 
    }
  }
  
  if(!is.null(additional_formula_terms)){
    form <- paste0(form, additional_formula_terms)
  }

  gam_mod <- mgcv::bam(as.formula(form),
                       offset=log(exposure),
                       weights = regression_weights,
                       data = ppm_df$ppm_df,
                       discrete = discrete,
                       nthreads = 8,
                       #cluster = 8,
                       select = TRUE,
                       family = "poisson")  
  
  # Predict
  #beginCluster()
  if(space_time == FALSE){
  predicted_log_rate <- raster::predict(ppm_df$prediction_stack, gam_mod,
                                        discrete = discrete_prediction)
  }else{

    if(prediction_periods == 'each'){
        predicted_log_rate <- stack()
        period_raster <- exposure
        for(i in 1:length(periods)){
          period_raster[] <- periods[i]
          names(period_raster) <- "period"
          pred_stack_period <- stack(ppm_df$prediction_stack, period_raster)
          
          # Add dyanmic covariates if they are defined
          if(!is.null(covariates_dynamic)){
            dynamic_covariate <- stack(sapply(covariates_dynamic, FUN = function(x){x[[i]]}))
            
            # If any are fixed, then fix here
            if(!is.null(fixed_dynamic_covariates)){
              for(covar in names(fixed_dynamic_covariates)){
                dynamic_covariate[[covar]][] <- fixed_dynamic_covariates[[covar]]
              }
            }
            pred_stack_period <- stack(pred_stack_period, dynamic_covariate)
          }
          predicted_log_rate <- stack(predicted_log_rate,
                                      raster::predict(pred_stack_period, gam_mod,
                                                      discrete = discrete_prediction))
        }
    }
    
    if(prediction_periods == 'last'){
      period_raster <- exposure
        period_raster[] <- max(periods)
        names(period_raster) <- "period"
        pred_stack_period <- stack(ppm_df$prediction_stack, period_raster)
        predicted_log_rate <- raster::predict(pred_stack_period, gam_mod)
    }
  }
  #endCluster()
  pred_raster_inc_per_1000 <- exp(predicted_log_rate)*1000
  
  # Return
  if(debug){
    return(list(pred_raster = pred_raster_inc_per_1000,
                model = gam_mod))
  }else{
  return(pred_raster_inc_per_1000)
  }
}
  