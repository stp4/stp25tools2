# Ausgangslage:
#
# In der Medizin sind reale und interessante labor Messwerte sehr selten normal verteilt.
# Eine eindeutig normal verteilte Messreihe deutet eher auf eine Datenmanipulation oder das Rauschen eines Messinstruments hin.
# Zu erwarten sind logarithmische, Poisson- oder S-förmige (Sigmoid) Verteilungen.
#
# Ich postuliere, dass es bei Labor-Messwerten ein plausibles inhaltliches Minimum und Maximum gibt, das Wissenschaftler kennen müssen.
# Außerdem muss die Verteilung entweder bekannt sein oder sich mittels QQ-Plot bestimmen lassen.
# 
# 

#' CI-Sigmoid-Funktion
#'
#'
#' @param x numeric
#' @param conf_level 	the confidence leve
#'
#' @returns vector (n, mean, sd, ci_lower, ci_upper)
#' @export
#'
#' @examples
#'
#' calculate_sigmoid_ci(airquality$Ozone)
#'
calculate_sigmoid_ci <- function(x,
                                 conf_level = 0.95,
                                 minimum = NULL,
                                 maximum = NULL,
                                 eps = 0.01) {
 #  mean_raw <- mean(x, na.rm = TRUE)
 #  sd_raw <- sd(x, na.rm = TRUE)
 # print( t.test(na.omit(x)) )
  # Scale the values from 0 to 100 and ensure that there are no negative values.
  # Logit-Transform
  x <-  scale_sigmoid(x,
                minimum = minimum,
                maximum = maximum,
                eps = eps)
  
  transformed <- na.omit(x)
  
  n_t <- length(transformed)
  mean_t <- mean(transformed)
  sd_t <- sd(transformed)
  
  # CI auf logit-Skala alternative t.test()
  margin <- qt((1 + conf_level) / 2, n - 1) * sd_t / sqrt(n)
  ci_lower <- mean_t - margin
  ci_upper <- mean_t + margin  
  
  # Back-transform
  rslt<- re_scale_sigmoid(
      c(
        mean = mean_t,
        sd = sd_t,
        ci_lower = ci_lower,
        ci_upper = ci_upper
      ),
      minimum = attr(x, "min") ,
      maximum = attr(x, "max"),
      eps = attr(x, "eps")
    )
 
  # Correktion
  rslt["ci_lower"] <-  max(attr(x, "min"), rslt["ci_lower"] )
  rslt["ci_upper"] <-   min(attr(x, "max"), rslt["ci_upper"])
  
  c(n = n_t, rslt)

}




#' @title Scaling  and Re-Scaling
#' @param x numeric vector
#' @param minimum numeric or min(x) Theoretical minimum of the measurement values
#' @param maximum numeric or max(x) Theoretical maximum of the measurement values
#'
#' @returns numeric vector
#' @export
#' @name scale_rescale
#'
#' @examples
#'
#' #'scale_sigmoid (airquality$Ozone) #|> hist()
#' scale_0_100(1:100) |> re_scale_0_100()
#' summary(scale_0_100(1:100))
#' scale_sigmoid(1:10) |> re_scale_sigmoid()
#' scale_log(1:10) |> re_scale_log()
#'
scale_guess <- re_scale_guess<- function(x){
  
  cat("Das ist nur ein Platzhalter - Ich arbeite noch daran.")
  x 
}




#' @rdname scale_rescale
#' @description
#' scale_0_100(): Scaling 0 to 100
#'
#' @param shift_range Small buffer for boundary
#' @export
scale_0_100 <- function(x,
                        minimum = NULL,
                        maximum = NULL,
                        shift_range = 0.001) {
  if (is.null(minimum)) {
    minimum <- min(x, na.rm = TRUE) * (1 - shift_range)
  }
  else{
    if (min(x, na.rm = TRUE)  < minimum) {
      x[x < minimum] <- NA
      warning("Es gibt Werte kleiner als das Minimum -> diese wurden entfernt")
    }
  }
  if (is.null(maximum)) {
    maximum <- max(x, na.rm = TRUE) * (1 + shift_range)
    
  }
  else {
    if (max(x, na.rm = TRUE)  > maximum) {
      x[x > maximum] <- NA
      warning("Es gibt Werte kleiner groeser das Maximum -> diese wurden entfernt")
    }
  }
  
  x <-  (x - minimum)  / (maximum - minimum) * 100
  attr(x, "min") <- minimum
  attr(x, "max") <- maximum
  x
}
#' @rdname scale_rescale
#' @export
re_scale_0_100 <- function(x,
                           minimum = attr(x, "min"),
                           maximum = attr(x, "max")) {
  x * (maximum - minimum) / 100  + minimum
}

#' scale_sigmoid
#' @rdname scale_rescale
#' @description
#' scale_sigmoid():  
#' Zunächst wird auf den Bereich 0 bis 100 transformiert, dann erfolgt die Logit-Transformation.
#'
#'
#' @param eps Small buffer for boundary
#' @export
scale_sigmoid <- function(x,
                          minimum = NULL,
                          maximum = NULL,
                          eps = 0.001) {
  if (any(x <= eps, na.rm = TRUE)) {
    stop(
      "Es gibt Werte kleiner als",
      eps,
      "daher kann kein log() berechnet werden. Mit eps die Grenze einstellen"
    )
  }
  
  transformed <-
    scale_0_100(x, minimum, maximum)
  m_min <- attr(transformed, "min")
  m_max <- attr(transformed, "max")
  
  transformed <- log((transformed + eps) / (100 - transformed + eps))
  attr(transformed, "min") <- m_min
  attr(transformed, "max") <- m_max
  attr(transformed, "eps") <- eps
  transformed
}

#' @rdname scale_rescale
#' @export
re_scale_sigmoid <- function(x,
                             minimum = attr(x, "min") ,
                             maximum = attr(x, "max"),
                             eps = attr(x, "eps")) {
  re_scale_0_100(100 / (1 + exp(-x)) - eps, minimum, maximum)
  
}


#' scale_sigmoid
#' @rdname scale_rescale
#' @description
#' scale_log():

#' @export
scale_log <- function(x,
                      minimum = NULL,
                      maximum = NULL,
                      eps = 0.001) {
  if (any(x <= eps, na.rm = TRUE)) {
    stop(
      "Es gibt Werte kleiner als",
      eps,
      "daher kann kein log() berechnet werden. Mit eps die Grenze einstellen"
    )
  }
  
  transformed <-
    scale_0_100(x, minimum, maximum)
  m_min <- attr(transformed, "min")
  m_max <- attr(transformed, "max")
  
  transformed <- log((transformed + eps))
  attr(transformed, "min") <- m_min
  attr(transformed, "max") <- m_max
  attr(transformed, "eps") <- eps
  transformed
}

#' @rdname scale_rescale
#' @export
re_scale_log <- function(x,
                         minimum = attr(x, "min") ,
                         maximum = attr(x, "max"),
                         eps = attr(x, "eps")) {
  re_scale_0_100(exp(x) - eps, minimum, maximum)
  
}

 
