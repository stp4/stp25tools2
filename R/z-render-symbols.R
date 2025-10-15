# Symbols -----------------------------------------------------------------

symbol_nbsp <-   function(output = which_output()) {
  switch(
    output,
    html = "&nbsp;",
    " ")

}

symbol_chi2 <- function(output = which_output()) {
  switch(
    output,
    text =  "X2",
    markdown =  "X2",
    html =  "&chi;<sup>2</sup>",
    word =   "X2",
    "X2"
  )
}

symbol_kleiner_gleich <- function(output = which_output()) {
  if (output == "html")
    "&le;"
  else
    "<="
}

symbol_groesser_gleich <-  function(output = which_output()) {
  switch(
    output,
    html = "&ge;",
    ">=")
}

symbol_alpha	<- function(output = which_output()) {
  switch(
    output,
    html = "&alpha;",
    "alpha")

}

symbol_beta	<- function(output = which_output()) {
  switch(
    output,
    html = "&beta;",
    "beta")
}

symbol_eta	<-  function(output = which_output()) {
  switch(
    output,
    html = "&eta;",
    "eta")
}

symbol_kappa	<- function(output = which_output()) {
  switch(
    output,
    text =  "kappa",
    markdown =  "kappa",
    html =  "&kappa;",
    word =   "kappa",
    "kappa"
  )
}



#sep <- get_opt("sep_element")

sub_tiefgestellt <- function(output = which_output()){
  switch(
    output,
    text =  c("", ""),
    markdown =   c("", ""),
    html =  c("<sub>", "</sub>"),
    word =  c("", ""),
    c("", "")
  )

}





