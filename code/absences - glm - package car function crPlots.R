crPlots <- function (model, terms = ~., layout = NULL, ask, main, ...) {
  
  ######################################
  # check if user wants specific terms #
  ######################################
  terms <- if (is.character(terms)) 
    paste("~", terms)
  
  else terms
  
  #################################
  # refit model w/ specifed terms #
  #################################
  vform <- update(formula(model), terms)
  
  if (any(is.na(match(all.vars(vform), all.vars(formula(model)))))) 
    stop("Only predictors in the formula can be plotted.")
  
  mf <- attr(model.frame(model), "terms")
  
  terms <- attr(mf, "term.labels")
  
  vterms <- attr(terms(vform), "term.labels")
  
  ##################################
  # warn if there are interactions #
  ##################################
  if (any(attr(terms(model), "order") > 1)) {
    stop("C+R plots not available for models with interactions.")
  }
  
  ###############################
  # the number of terms to plot # 
  nt <- length(vterms)
  ###############################
    if (nt == 0) 
    stop("No plots specified")
  
  ##########################
  # default title for plot # 
  ########################## 
  if (missing(main)) 
    main <- if (nt == 1) 
      "Component + Residual Plot"
  
  else "Component + Residual Plots"
  
  ##################################
  # if mult. terms, arrange layout #
  ##################################
  if (nt > 1 & (is.null(layout) || is.numeric(layout))) {
    
    if (is.null(layout)) {
      layout <- switch(min(nt, 9), c(1, 1), c(1, 2), c(2, 2), c(2, 2), c(3, 2), c(3, 2), c(3, 3), c(3, 3), c(3, 3))
    }
    
    ask <- if (missing(ask) || is.null(ask)) 
      prod(layout) < nt
    
    else ask
    
    op <- par(mfrow = layout, ask = ask, no.readonly = TRUE, oma = c(0, 0, 1.5, 0), mar = c(5, 4, 1, 2) + 0.1)
    
    on.exit(par(op))
    
  }
  
  ###############################
  # deal with missing variables #
  ###############################
  if (!is.null(class(model$na.action)) && class(model$na.action) == "exclude") 
    class(model$na.action) <- "omit"
    
  #####################################
  # WHAT IS THIS FUNCTION crPlot ???? # 
  #####################################
  # it looks like this function crPlots just arranges plots for multiple variables 
  # then uses "crPlot" to actual plot the things
  # BUT, there is documentation for this function 
  for (term in vterms) crPlot(model, term, ...)
  
  mtext(side = 3, outer = TRUE, main, cex = 1.2)
  
  invisible(0)
}