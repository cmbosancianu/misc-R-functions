# The first argument should be a list of models.
# The second argument should be a numeric, describing the precision
#   of the coefficients in the table.

# For now, the function only uses information on estimates and 
# standard errors, with CI functionality to be added later.

# Requires "broom", "dplyr", "stringr", and "xtable"

texStan <- function(l, numdigits, conf.level) {
  require(broom)
  require(dplyr)
  require(stringr)
  require(xtable)
  
  # How many columns should the data have?
  lenlis <- length(l)
  
  # Store the various outputs into a list of data frames
  reslist <- list()
  for (i in 1:lenlis) {
    reslist[[i]] <- tidy(l[[i]], intervals = TRUE, prob = conf.level)
    reslist[[i]] <- reslist[[i]] %>%
      select(-c(lower, upper))
    reslist[[i]]$term <- as.character(reslist[[i]]$term)
  }
  
  # Find the number of unique column names; "bind_rows()" is
  #   found in the "dplyr" package
  tempDF <- as.data.frame(bind_rows(reslist))
  varVec <- unique(tempDF$term); rm(tempDF)
  varVec <- as.data.frame(varVec)
  colnames(varVec) <- c("term")
  varVec$term <- as.character(varVec$term)
  
  # Merge all estimates into one dataframe
  for (i in 1:lenlis) {
    varVec <- left_join(varVec, reslist[[i]], by = c("term"))
    varVec <- varVec %>%
      select(-c(std.error))
    colnames(varVec)[i+1] <- paste("Model", i, sep = " ")
  }
  estimDF <- varVec; rm(varVec)
  
  # Follow up with standard errors
  tempDF <- as.data.frame(bind_rows(reslist))
  varVec <- unique(tempDF$term); rm(tempDF)
  varVec <- as.data.frame(varVec)
  colnames(varVec) <- c("term")
  varVec$term <- as.character(varVec$term)
  
  # Merge all standard errors into one data frame
  for (i in 1:lenlis) {
    varVec <- left_join(varVec, reslist[[i]], by = c("term"))
    varVec <- varVec %>%
      select(-c(estimate))
    colnames(varVec)[i+1] <- paste("Model", i, sep = " ")
  }
  stderrDF <- varVec; rm(varVec)
  
  # Create the final data frame
  finalDF <- bind_rows(estimDF, setNames(stderrDF, names(estimDF))) %>% 
    arrange(term)
  rm(estimDF, stderrDF, i)
  
  # Start cleaning variable names as much as possible
  # This would be relevant for models with fixed effects
  finalDF$term <- str_remove_all(finalDF$term,
                                 pattern = c("b_"))
  finalDF$term <- str_remove_all(finalDF$term,
                                 pattern = c("as.factor"))
  
  # Identify even row names in the data frame and delete variable names
  evenrw <- as.numeric(rownames(finalDF[c(FALSE,TRUE), ]))
  finalDF$term[evenrw] <- NA; rm(evenrw)
  
  # Round digits before printing
  finalDF[ ,2:(lenlis+1)] <- round(finalDF[ ,2:(lenlis+1)],
                                   digits=numdigits)
  
  finalDF[ ,2:(lenlis+1)] <- as.character(finalDF[ ,2:(lenlis+1)])
  
  # Put brackets around even numbers
  for (i in 2:(lenlis+1)) {
    finalDF[ ,i] <- with(finalDF,
                         ifelse(is.na(term), paste0("(", finalDF[ ,i], ")"),
                                finalDF[ ,i]))
  }
  
  # Print final table
  print(xtable(finalDF), type = "latex",
        include.rownames = FALSE,
        floating = TRUE,
        floating.environment = "table",
        table.placement = "!ht",
        caption.placement = "top")
}
