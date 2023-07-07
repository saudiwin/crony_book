# Helper functions for combining MI estimates for conditional means for ggplot etc.

to_use <- c('Less than 5',
            'From 5 to 9',
            'From 10 to 19',
            'From 20 to 50',
            'From 51 to 100',
            'From 101 to 250',
            'From 251 to 500',
            'From 501 to 1000',
            '1001 and over')
perf_labels <- c('Less than 5',
                   'From 5\n to 9',
                   'From 10\n to 19',
                   'From 20\n to 50',
                   'From 51\n to 100',
                   'From 101\n to 250',
                   'From 251\n to 500',
                   'From 501\n to 1000',
                   '1001 and\n over')

# function that will recode a lot of variables to sensible means
recode_vars <- function(this_data) {
  this_data <- mutate(this_data,
                      firm_size=factor(firm_size,levels=to_use),
                      perf_year1=factor(perf_year1,
                                        c('More than 20% loss',
                                          'Between 10 and 20% loss',
                                          'Between 10 and 5% loss',
                                          'Between 5 and 0% loss',
                                          'Between 0 and 5% profit margin',
                                          'Between 5 and 10% profit margin',
                                          'Between 10 and 20% profit margin')),
                      perf_year2=factor(perf_year2,
                                        c('More than 20% loss',
                                          'Between 10 and 20% loss',
                                          'Between 10 and 5% loss',
                                          'Between 5 and 0% loss',
                                          'Between 0 and 5% profit margin',
                                          'Between 5 and 10% profit margin',
                                          'Between 10 and 20% profit margin')),
                      registered=factor(registered,
                                        levels=c("Registered as a domestic company",
                                                 "100% foreign-owned enterprise",
                                                 "Joint-venture with an ${e://Field/country_adj}  private enterprise",
                                                 "Joint-venture with an ${e://Field/country_adj} state-owned enterprise",
                                                 "Other"),
                                        labels=c('Domestic Firm',
                                                  'Foreign Firm',
                                                 'JV with Domestic Firm',
                                                 'JV with State-owned Firm',
                                                 'Other')),
                      education=factor(education,levels=c('High School or lower',
                                                          'University (2-year)',
                                                          'University (4-year)',
                                                          'M.A./M.B.A.',
                                                          'PhD')))
  
  return(this_data)
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

mi_mean <- function(est_var,imputed) {
  
  out_data <- data_frame(est_var=est_var,
                         imputed=imputed) %>% 
    group_by(imputed) %>% summarize(mean_est=mean(est_var)) %>% 
    ungroup %>% 
    summarize(mean_est=mean(mean_est))
  return(out_data$mean_est)  
}

#' Each row should be one imputed value
#' @param within_var The within variation, i.e., the variance of the estimate within each imputation
#' @param between_var The estimate to be imputed
mi_se <- function(within_var,between_var) {
  
  out_data <- data_frame(within_var=within_var,
                         between_var=between_var) %>% 
    mutate(between_est=(between_var-mean(between_var))^2) %>% 
    summarize(within_est=mean(within_var),
              between_est=sum(between_est)/(n()-1),
              n=n()) %>% 
    mutate(total_var=within_est + (1 + 1/n)*between_est)
  return(sqrt(out_data$total_var)) 
}


rbind_matrix <- function(matrix_list) {
  all_dims <- sapply(matrix_list,function(m) {
    m <- t(m)
    return(m)
  })
  do.call(rbind,all_dims) %>% return
}

impute_amce <- function(datasets,formula=NULL,
                        keep_vars=NULL,
                        num_cores=4,
                        ...) {

  if(!is.null(keep_vars)) {
    datasets <- lapply(datasets,dplyr::select,one_of(keep_vars))
  }
  # set up object to keep results from
  first_run <- amce(formula=formula,
                    data=datasets[[1]],...)

  all_runs <- mclapply(datasets,function(d,...){
    out_result <- amce(formula=formula,
                       data=d,
                       ...)
    return(out_result)
    },...,
    mc.cores=num_cores)
  if(!is.null(first_run$vcov.resp)) {
    resp_est <- combine_amces(all_runs,uncond=F)
    first_run$cond.estimates <- resp_est$comb.est
    first_run$vcov.resp <- resp_est$comb.cov
  } 
    resp_est <- combine_amces(all_runs,uncond=T)
    first_run$estimates <- resp_est$comb.est
    first_run$vcov.prof <- resp_est$comb.cov
  
  
  return(first_run)
}
 
combine_amces <- function(models,uncond=T) {
  # code modified from 
  # https://gist.github.com/carlislerainey/9963264
  # Carlisle Rainey's Github
  
  # Arguments
  #   models:  a list of models, one estimated on each of m MI data sets.
  
  # Note: I borrow the notation below mostly from Rubin's *Multiple Imputation 
  # for Nonresponse in Surveys*.
  
  # count the number of imputations
  n.imp <- length(models)
  
  # reformat the AMCE object

  if(uncond==T) {
    n.vars <- length(models[[1]]$estimates)
    name_vars <- names(models[[1]]$estimates)
    length_vars <- sapply(models[[1]]$estimates,ncol)
    each_names <- lapply(models[[1]]$estimates,colnames)
    names(each_names) <- name_vars
    models <- lapply(models,function(m) {
      ses <- lapply(m$estimates,function(e) e[1,])
      m$coef <- unlist(ses)
      return(m)
    })
  } else {
    n.vars <- length(models[[1]]$cond.estimates)
    name_vars <- names(models[[1]]$cond.estimates)
    length_vars <- sapply(models[[1]]$cond.estimates,ncol)
    each_names <- lapply(models[[1]]$cond.estimates,colnames)
    names(each_names) <- name_vars
    models <- lapply(models,function(m) {
      ses <- lapply(m$cond.estimates,function(e) e[1,])
      m$coef <- unlist(ses)
      return(m)
    })
  }
  
  # calculate the average of the coefficients (Eqn 3.1.2, p. 76)
  mi.est <- NULL
  for (i in 1:n.imp) {
    mi.est <- rbind(mi.est, models[[i]]$coef)
  }
  
  
  Q.bar <- apply(mi.est, 2, mean)
  # store estimates
  comb.est <- Q.bar
  
  # calculate the average of the covariances (Eqn 3.1.3, p. 76)
  U.bar <- NULL
  if(uncond==T) {
    U.bar <- models[[1]]$vcov.prof
    for (i in 2:n.imp) {
      U.bar <- U.bar + models[[i]]$vcov.prof
    }
    U.bar <- U.bar/n.imp
  } else {
    U.bar <- models[[1]]$vcov.resp
    for (i in 2:n.imp) {
      U.bar <- U.bar + models[[i]]$vcov.resp
    }
    U.bar <- U.bar/n.imp
  }
  
  # calculate the variance *between* the MI data sets (Eqn 3.1.4, p. 76)
  B <- NULL
  
  B <- (models[[1]]$coef - Q.bar)%*%t(models[[1]]$coef - Q.bar)
  for (i in 2:n.imp) {
    B <- B + (models[[i]]$coef - Q.bar)%*%t(models[[i]]$coef - Q.bar)
  }
  
  
  B <- B/(n.imp - 1)
  # add fake data for intercept
  B <- cbind(rep(0,nrow(B)),B)
  B <- rbind(rep(0,ncol(B)),B)
  # calculate the *total* covariance (Eqn 3.1.5, p. 76)
  comb.cov <- U.bar + (1 + 1/(n.imp))*B # T iin Rubin's notation
  
  # calculate the standard errors
  # drop the intercept
  comb.se <- sqrt(diag(comb.cov))[-1]
  
  # recreate AMCE format for SES/coefs
  names(comb.est) <- unlist(sapply(1:length(name_vars), function(i) rep(name_vars[i],each=length_vars[i])))
  names(comb.se) <- unlist(sapply(1:length(name_vars), function(i) rep(name_vars[i],each=length_vars[i])))
  comb.amce <-  lapply(name_vars,function(n) {
    
    out_m <- rbind(comb.est[names(comb.est)==n],
                   comb.se[names(comb.est)==n])
    colnames(out_m) <- each_names[[n]]
    if(uncond==T) {
      row.names(out_m) <- c('AMCE',
                            'Std. Error')
    } else {
      row.names(out_m) <- c('Conditional Estimate',
                            'Std. Error')
    }
    return(out_m)
    
  })
  names(comb.amce) <- name_vars
  # add the relevant quantities to a list and return it
  res <- list(comb.est = comb.amce, comb.cov = comb.cov)
  return(res)
}


coeftest_amce <- function(data_est,vcov. = NULL, df = NULL, ...) {

  {
    coef0 <- if ("stats4" %in% loadedNamespaces()) 
      stats4::coef
    else coef
    vcov0 <- if ("stats4" %in% loadedNamespaces()) 
      stats4::vcov
    else vcov
    est <- data_est$coefficients
    if (is.null(vcov.)) 
      se <- data_est$variance
    else {
      if (is.function(vcov.)) 
        se <- vcov.(x, ...)
      else se <- vcov.
    }
    se <- sqrt(diag(se))
    # don't need to re-arrange names
    
    # if (!is.null(names(est)) && !is.null(names(se))) {
    #   if (length(unique(names(est))) == length(names(est)) && 
    #       length(unique(names(se))) == length(names(se))) {
    #     anames <- names(est)[names(est) %in% names(se)]
    #     est <- est[anames]
    #     se <- se[anames]
    #   }
    # }
    tval <- as.vector(est)/se
    if (is.null(df)) {
      df <- data_est$df
      if (inherits(df, "try-error")) 
        df <- NULL
    }
    if (is.null(df)) 
      df <- 0
    if (is.finite(df) && df > 0) {
      pval <- 2 * pt(abs(tval), df = df, lower.tail = FALSE)
      cnames <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      mthd <- "t"
    }
    else {
      pval <- 2 * pnorm(abs(tval), lower.tail = FALSE)
      cnames <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
      mthd <- "z"
    }
    rval <- cbind(est, se, tval, pval)
    colnames(rval) <- cnames
    class(rval) <- "coeftest"
    attr(rval, "method") <- paste(mthd, "test of coefficients")
    return(rval)
  }
}


# simple helper function to drop overly white values for diverging/sequential scales
create_pal <- function(measure_var,palette_name,drop_middle=F) {
  length_fac <- length(unique(measure_var))
  
  if(drop_middle==F) {
    this_pal <- RColorBrewer::brewer.pal(name=palette_name,n=length_fac+2)[3:(length_fac+2)]
  } else {
    this_pal <- RColorBrewer::brewer.pal(name=palette_name,n=length_fac)
    this_pal[ceiling(length(this_pal)/2)] <- "#DFDFDF"
  }
  return(this_pal)
} 

# helper to create latex tables
create_table <- function(out_data,caption,test_name,drop=F) {

  out_data <- dplyr::select(out_data,1:8)
  if(drop==F) {
    out_data <- dplyr::select(out_data,-3)
    names(out_data)[6] <- '$z$ Value'
    names(out_data)[7] <- '$Pr(>|z|)$'
    names(out_data)[2] <- 'Interaction'
  } else {
    out_data <- dplyr::select(out_data,-2)
    names(out_data)[6] <- '$z$ Value'
    names(out_data)[7] <- '$Pr(>|z|)$'
    names(out_data)[2] <- 'Attribute'
  }

  out_table <- xtable::xtable(out_data,auto=T,caption=paste0('Coefficients for ',caption),label=test_name)
  
  if(!file.exists('coef_tables.tex')) {
    print(out_table,
          sanitize.text.function = function(x) {
            out <- gsub('\\$\\{e://Field/country_adj\\}',"Country-",x)
            out <- gsub('%',"\\%",out)
            return(out)},
          latex.environments='center',
          include.rownames=F,
          booktabs=T,print.results=F, 
          tabular.environment = "longtable") %>% 
      cat(file=paste0('coef_tables.tex'),append=F)
  } else {
    print(out_table,
          sanitize.text.function = function(x) {
            out <- gsub('\\$\\{e://Field/country_adj\\}',"Country-",x)
            out <- gsub('%',"\\%",out)
            return(out)},
          latex.environments='center',
          include.rownames=F,
          booktabs=T,print.results=F,
          hline.after=c(-1, 0), 
          tabular.environment = "longtable") %>% 
      cat(file=paste0('coef_tables.tex'),append=T)
  }

  
}
