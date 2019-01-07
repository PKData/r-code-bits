
##Read in data
dat<-read.csv(_______)


##remember to take out columns that you know you don't want from a practical perspective
##ID column, I'm looking at you!




#Making NA a factor value.  This is helpful in cases where NA actually means something to you.  It seems that much of the time it does.


NA_preproc <- function (dat) {
  for (j in 1:ncol(dat)) {
    x <- dat[[j]]
    if (is.factor(x) && anyNA(x)) dat[[j]] <- base::addNA(x)
    if (is.character(x)) dat[[j]] <- factor(x, exclude = NULL)
  }
  dat
}

#making your dat df house the na preprocessed dat
dat<-NA_preproc(dat)


#getting the actual number of levels for factors in practice.  Simpler methods I've tried have been close, but missed the mark.

debug_contr_error <- function (dat, subset_vec = NULL) {
  if (!is.null(subset_vec)) {
    ## step 0
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
      }
      subset_log_vec <- subset_vec
    } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
      } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
      } 
    } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
    }
    dat <- base::subset(dat, subset = subset_log_vec)
  } else {
    ## step 1
    dat <- stats::na.omit(dat)
  }
  if (nrow(dat) == 0L) warning("no complete cases")
  ## step 2
  var_mode <- sapply(dat, mode)
  if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
  var_class <- sapply(dat, class)
  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
    stop("matrix variables with 'AsIs' class must be 'numeric'")
  }
  ind1 <- which(var_mode %in% c("logical", "character"))
  dat[ind1] <- lapply(dat[ind1], as.factor)
  ## step 3
  fctr <- which(sapply(dat, is.factor))
  if (length(fctr) == 0L) warning("no factor variables to summary")
  ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
  dat[ind2] <- lapply(dat[ind2], base::droplevels.factor)
  ## step 4
  lev <- lapply(dat[fctr], base::levels.default)
  nl <- lengths(lev)
  ## return
  list(nlevels = nl, levels = lev)
}

#transforming the number of levels into a list of columns to remove
levelers<-data.frame(debug_contr_error(dat)$nlevels)
removelist<-row.names(levelers)

#making your new df that is ready for glm or other models.
dat2<-dat[,-which(names(dat) %in% removelist)]

#Model Away!!

simple_glm_model<-glm(Target~.,data=dat2)

