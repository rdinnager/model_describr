#' Function to extract equations representing the underlying model of an `lmerMod` object
extract_model.lmerMod <- function(model, fixed_labels, random_labels) {
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
  
  term2eq <- function(term, symbol = "\\beta") {
    
    if(term == "intercept"){
      
      eq <- "\\alpha"
      
    } else {
    
      if(grepl("[:]", term)) {
        two_terms <- strsplit(term, ":", fixed = TRUE)
        term_1 <- two_terms[[1]][1]
        term_2 <- two_terms[[1]][2]
        eq <- glue::glue("{<symbol>}_{\\text{<term>}} x_{\\text{<term_1>}}_i x_{\\text{<term_2>}}_i", .open = "<", .close = ">")
      } else {
        eq <- glue::glue("{<symbol>}_{\\text{<term>}} x_{\\text{<term>}}_i", .open = "<", .close = ">")
      }
      
    }
    eq
  }
  
  beta_md <- sapply(betas, term2eq)
  beta_eq <- paste(unlist(beta_md), collapse = " + ")
  
  lapply(lapply(sub_terms, function(y) y$betas), function(x) sapply(x, term2eq, "\\gamma"))
  
  html <- Pandoc.convert(text = paste0("$$", beta_eq, "$$"), open = FALSE, format = "html4")
  file.rename(html, gsub("html4", "html", html))
  viewer <- getOption("viewer")
  viewer(gsub("html4", "html", html))

  
  eq_head <- "$$\\begin{aligned}"
  
  test <- "\\beta+i\\sim\\text{Normal}\\left(0,\\sigma^2\\right)\\\\\n\\beta^i\\sim\\text{Normal}\\left(0,\\sigma^2\\right)"
  
  eq_tail <- "\\end{aligned}$$"
  
  Pandoc.convert(text = "$$\\begin{aligned}\n\\beta_i \\sim & \\text{Normal}\\left(0,\\sigma^2\\right)\\\\
\\gamma_i \\sim & \\text{Normal}\\left(0,\\sigma^2\\right)\n\\end{aligned}$$")
  
  f <- paste0(tempfile(), ".tex")
  con <- file(f, "w", encoding = "UTF-8")
  cat(paste(eq_head, test, eq_tail, sep = "\n"), file = con)
  close(con)
  f.out <- paste0(tempfile(), ".html")
  system(paste(panderOptions("pandoc.binary"), f, "-f latex -t html -o", f.out))
  
  system(paste(panderOptions("pandoc.binary"), f, "-t html -o", f.out))
  
  Pandoc.convert(text = paste(eq_head, test, eq_tail, sep = "\n"))
  eq_terms <- list()
}