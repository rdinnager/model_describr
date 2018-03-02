modeldescribr.lmerMod <- function(model, fixed_labels, random_labels) {
  fixeds <- lme4::nobars(formula(lme4mod))
  fixed_terms <- terms(fixeds)
  
  betas <- attr(fixed_terms, "term.labels")
  if(attr(fixed_terms, "intercept") == 1) {
    betas <- c("intercept", betas)
  }
  beta_interact <- grepl("[:]", betas)
  
  randoms <- lme4::findbars(formula(model))
  sub_terms <- list()
  for(i in seq_along(randoms)) {
    form_char <- as.character(randoms[[i]])
    sub_term <- terms(as.formula(paste0("~", form_char[2])))
    s_terms <- attr(sub_term, "term.labels")
    if(attr(sub_term, "intercept") == 1) {
      s_terms <- c("intercept", s_terms)
    }
    sub_terms[[i]] <- list(betas = s_terms, group = form_char[3]) 
  }
  
  eq_terms <- list()
}