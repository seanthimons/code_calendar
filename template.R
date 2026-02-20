# packages ---------------------------------------------------------------

{
  list.of.packages <- c(
    'here',
    'rio',
    'tidyverse',
    'janitor',
    'esquisse',
    'skimr',
    'timetk',
    'sf', #if plotting
    'mapview', # if plotting
    'mapgl',
    'toxpiR'
  )

  new.packages <- list.of.packages[
    !(list.of.packages %in% installed.packages()[, "Package"])
  ]
  if (length(new.packages)) install.packages(new.packages)

  lapply(list.of.packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      #install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  })

  rm(list.of.packages, new.packages)
}

# custom functions -------------------------------------------------------
{
  `%ni%` <- Negate(`%in%`)

  ## for printing of colnames for selection
  prettylist <- function(x) {
    paste0("'", x, "',", "\n") %>% cat()
  }

  skim_count <- skim_with(
    numeric = sfl(n = length, median = ~ median(.x, na.rm = T))
  )
}

# raw --------------------------------------------------------------------
