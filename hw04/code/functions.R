# -------------------------------------------------------
# Title: functions
# Description: script to produce functions used in hw04
# Author: Doug Koerber
# -------------------------------------------------------
library(stringr)

# remove missing function
remove_missing <- function(x) {
  return(x[!is.na(x)])
}

# get minimum function
get_minimum <- function(x, na.rm=FALSE) {
  if (na.rm) {
    x <- remove_missing(x)
  } else if (NA %in% x) {
    return(NA)
  }
  x <- sort(x, na.last = TRUE)
  return(x[1])
}

# get maximum function
get_maximum <- function(x, na.rm=FALSE) {
  if (na.rm) {
    x <- remove_missing(x)
  } else if (NA %in% x) {
    return(NA)
  }
  x <- sort(x, decreasing = TRUE, na.last = TRUE)
  return(x[1])
}

# get range function
get_range <- function(x, na.rm=FALSE) {
  if (na.rm) {
    x <- remove_missing(x)
  }
  range_min = get_minimum(x)
  range_max = get_maximum(x)
  return(range_max - range_min)
}

# get median function
get_median <- function(x, na.rm=FALSE) {
  if (na.rm) {
    x <- remove_missing(x)
  } else if (NA %in% x) {
    return(NA)
  }
  x <- sort(x, na.last = TRUE)
  n = length(x)
  if ((n %% 2) != 0) {
    return(x[ceiling(n / 2)])
  } else {
    lower <- x[(n / 2)]
    upper <- x[(n / 2) + 1]
    return((lower + upper) / 2)
  }
}

# get average function
get_average <- function(x, na.rm=FALSE) {
  if (na.rm) {
    x <- remove_missing(x)
  } else if (NA %in% x) {
    return(NA)
  }
  n = length(x)
  total = 0
  for (i in 1:n) {
    total = total + x[i]
  }
  return(total / n)
}

# get standard deviation function
get_stdev <- function(x, na.rm=FALSE) {
  if (na.rm) {
    x <- remove_missing(x)
  } else if (NA %in% x) {
    return(NA)
  }
  x_bar = get_average(x, na.rm = TRUE)
  n = length(x)
  total = 0
  for (i in 1:n) {
    total = total + ((x[i] - x_bar) ^ 2)
  }
  return(sqrt(total / (n - 1)))
}

# get 10th percentile function
get_percentile10 <- function(x, na.rm=FALSE) {
  if (na.rm) {
    x <- remove_missing(x)
  } else if (NA %in% x) {
    return(NA)
  }
  return(quantile(x, probs = 0.1)[[1]])
}

# get 90th percentile function
get_percentile90 <- function(x, na.rm=FALSE) {
  if (na.rm) {
    x <- remove_missing(x)
  } else if (NA %in% x) {
    return(NA)
  }
  return(quantile(x, probs = 0.9)[[1]])
}

# get first quartile function
get_quartile1 <- function(x, na.rm=FALSE) {
  if (na.rm) {
    x <- remove_missing(x)
  } else if (NA %in% x) {
    return(NA)
  }
  return(quantile(x, probs = 0.25)[[1]])
}

# get third quartile function
get_quartile3 <- function(x, na.rm=FALSE) {
  if (na.rm) {
    x <- remove_missing(x)
  } else if (NA %in% x) {
    return(NA)
  }
  return(quantile(x, probs = 0.75)[[1]])
}

# count missing function
count_missing <- function(x) {
  n <- length(x)
  total = 0
  for (i in 1:n) {
    if (is.na(x[i])) {
      total = total + 1
    }
  }
  return(total)
}

# summary statistics function
summary_stats <- function(x) {
  list_minimum   = get_minimum(x, na.rm = TRUE)
  list_percent10 = get_percentile10(x, na.rm = TRUE)
  list_quartile1 = get_quartile1(x, na.rm = TRUE)
  list_median    = get_median(x, na.rm = TRUE)
  list_mean      = get_average(x, na.rm = TRUE)
  list_quartile3 = get_quartile3(x, na.rm = TRUE)
  list_percent90 = get_percentile90(x, na.rm = TRUE)
  list_maximum   = get_maximum(x, na.rm = TRUE)
  list_range     = get_range(x, na.rm = TRUE)
  list_stdev     = get_stdev(x, na.rm = TRUE)
  list_missing   = count_missing(x)
  return(list(minimum = list_minimum,
              percent10 = list_percent10,
              quartile1 = list_quartile1,
              median = list_median,
              mean = list_mean,
              quartile3 = list_quartile3,
              percent90 = list_percent90,
              maximum = list_maximum,
              range = list_range,
              stdev = list_stdev,
              missing = list_missing
              )
         )
}

# print statistics function
print_stats <- function(x) {
  n <- length(x)
  for (i in 1:n) {
    element_name <- str_pad(names(x)[i], width = 9, side = 'right')
    element <- format(x[[i]], digits = 4, nsmall = 4)
    cat(paste0(element_name, ': ', element, '\n'))
  }
}

# rescale function
rescale100 <- function(x, xmin, xmax) {
  z = 100 * ((x - xmin) / (xmax - xmin))
  return(z)
}

# drop lowest function
drop_lowest <- function(x) {
  x_min = get_minimum(x, na.rm = TRUE)
  pos = match(x_min, x)
  return(x[-pos])
}

# score homework function
score_homework <- function(x, drop=FALSE) {
  if (drop) {
    x <- drop_lowest(x)
  }
  return(get_average(x))
}

# score quiz function
score_quiz <- function(x, drop=FALSE) {
  if (drop) {
    x <- drop_lowest(x)
  }
  return(get_average(x))
}

# score lab function
score_lab <- function(x) {
  if (x >= 11) {
    return(100)
  } else if (x >= 10) {
    return(80)
  } else if (x >= 9) {
    return(60)
  } else if (x >= 8) {
    return(40)
  } else if (x >= 7) {
    return(20)
  } else {
    return(0)
  }
}