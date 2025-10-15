#
# # data --------------------------------------------------------------------
#
#
# set.seed(123)
# n <- 2000
# n_clinics <- 5
#
#
# # Basis-Variablen
# sex <- sample(c("male", "female"), size = n, replace = TRUE, prob = c(0.5, 0.5))
# age <- round(rgamma(n, shape = 10, scale = 3))
# age <- age   - min(age) + 18
# age[age > 90] <- 90 # Maximalalter begrenzen
#
# bmi_group <- numeric(n)
# for(i in 1:n) {
#   if(age[i] < 30) {
#     bmi_group[i] <- sample(c(c("normal", "overweight", "obese")),
#                      size = 1, prob = c(0.60, 0.20, 0.10))
#   } else {
#     bmi_group[i] <- sample(c(c("normal", "overweight", "obese")),
#                      size = 1, prob = c(0.15, 0.35, 0.30))
#   }
# }
#
# bmi_group <- factor(bmi_group, c("normal", "overweight", "obese"))
# # Harnsäure abhängig vom Geschlecht erzeugen
# harnsaeure <- numeric(n)
# for(i in 1:n) {
#   if(sex[i] == "female") {
#     harnsaeure[i] <- round(runif(1, 2.0, 6.9), 1)
#   } else {
#     harnsaeure[i] <- round(runif(1, 2.2, 8), 1)
#   }
# }
#
#
# smoking <- factor(sample(
#   c("never", "former", "current"),
#   n,
#   replace = TRUE,
#   prob = c(0.5, 0.3, 0.2)
# ))
# cholesterol <- round(rnorm(n, mean = 200, sd = 30))  # mg/dl Werte
#
# # Hierarchie (Kliniken)
#
# clinic <- factor(sample(1:n_clinics, size = n, replace = TRUE))
# clinic_re <- rnorm(n_clinics, mean = 0, sd = 0.3)  # zufälliger Klinik-Effekt
# clinic_effect <- clinic_re[as.numeric(clinic)]
#
#
# # Lineares Prädiktorenmodell (log-link)
#
# linpred <- 0.01 * age +
#   0.5  * (smoking == "current") +
#   0.2  * (smoking == "former") +
#   0.3  * (bmi_group == "obese") +
#   0.001  * (cholesterol / 50) +                 # Skaliert
#   0.2 * age * (smoking == "current") / 100 + # Interaktion
#   clinic_effect
#
# lambda <- exp(linpred)
#
#
# # Outcome simulieren (0–12)
#
# hospital_visits <- pmin(rpois(n, lambda), 12)
#
# # Binäre Variante
# any_visit <- (hospital_visits > 0)
#
#
# # Datensatz zusammenstellen
#
# hsptl <- data.frame(
#   id =1:n,
#   clinic = factor( clinic, seq_len(n_clinics), LETTERS[seq_len(n_clinics)]),
#   sex = factor(sex),
#   age = age,
#   bmi = bmi_group,
#   smoking = smoking,
#   cholesterol = cholesterol,
#   homocystein = round(runif(n, 5, 12),1),
#   glucose = round(runif(n, 50, 120)),
#   tbg = round(runif(n, 12, 35), 1),
#   harnsaeure =harnsaeure,
#   visits = hospital_visits,
#   any_visit = any_visit
# )
# # |>
# #   Label(
# #     sex = "Sex",
# #     age = "Age",
# #     bmi = "BMI group",
# #     smoking = "Smoking",
# #     cholesterol = "Cholesterol [mg/dl]",
# #     homocystein = "Homocystein [µmol/l",
# #     glucose = "Glucose [mg/dl]",
# #     harnsaeure = "Harnsäure [mg/dl]",
# #     tbg =  "TBG [mg/l",
# #     visits = "Hospital visits",
# #     any_visit = "any visit",
# #     clinic = "Clinic"
# #   )
#
# rm(
#   age,
#   sex,
#   bmi_group,
#   smoking,
#   cholesterol,
#   hospital_visits,
#   any_visit,
#   clinic,
#   clinic_effect,
#   linpred,
#   clinic_re,
#   n_clinics,
#   n,
#   lambda
# )
#
#
# dat_no_visit <- sample( which(!hsptl$any_visit) )
# dat_visit <-sample(which(hsptl$any_visit), sum(!hsptl$any_visit))
# hsptl <- hsptl[c(dat_no_visit, dat_visit ),  ]
# hsptl <- hsptl[order(hsptl$id), ]
# # xtabs( ~sex  + any_visit, hsptl)
# #
# #
# #
# # summary(hsptl)
# # hist(hsptl$visits)
#
#
# #head(hsptl, 10)
#
#
# # prepare -----------------------------------------------------------------
#
#
#
#
# #
# # hsptl |>
# #   prepare_data(
# #     sex,
# #     age[median,0],
# #     bmi,
# #     smoking,
# #     cholesterol,
# #     homocystein,
# #     glucose,
# #     tbg,
# #     harnsaeure,
# #     visits[median],
# #     any_visit
# #   )
#
# #
#
#
#
#
# # describe ----------------------------------------------------------------
#
#
#
# #
# # hsptl |>
# #   Tbll_desc(
# #     sex,
# #     age[median,0],
# #     bmi,
# #     smoking,
# #     cholesterol,
# #     homocystein,
# #     glucose,
# #     tbg,
# #     harnsaeure,
# #     visits[median],
# #     any_visit
# #   )
# #
# #
# # hsptl |>
# #   Tbll_desc(
# #     ~sex +
# #     age[median,0] +
# #     bmi +
# #     smoking +
# #     cholesterol +
# #     homocystein +
# #     glucose +
# #     tbg +
# #     harnsaeure +
# #     visits[median] +
# #     any_visit
# #   )





#' require(stp25tools2)
#'
#' # --------------------------------------------------
#' Tbll_desc(~ age +smoking+ bmi_group+cholesterol+clinic,
#'           data = hsptl)
#'
#' Tbll_desc(~hospital_visits +any_visit+age +smoking+ bmi_group+cholesterol+clinic,
#'           data = hsptl)|> data.frame()
#'
#' hsptl |>
#'   Tbll_desc(
#'     hospital_visits ,any_visit,age ,
#'     smoking, bmi_group,cholesterol,clinic)|>
#'   data.frame()
#'
#'
#' lm_fit <- lm(hospital_visits ~ age  + smoking + bmi_group + cholesterol,
#'              data = hsptl)
#' #plot((allEffects(lm_fit)))
#' # Poisson-Modell
#' glm_pois <- glm(hospital_visits ~ age + smoking + bmi_group + cholesterol,
#'                 family = poisson(link = "log"),
#'                 data = hsptl)
#'
#'
#' #oldc
#' #options(contrasts = contrasts)
#' regression_to_df_flexible(ols=lm_fit,
#'                           pois=glm_pois,
#'                           include.ci =TRUE)
#'
#'
#' # Pakete
#' library(MASS)
#' require(effects)
#'
#' # Lineares Modell (ungeeignet für Counts)
#' lm_fit <- lm(hospital_visits ~ age * smoking + bmi_group + cholesterol,
#'              data = hsptl)
#' #plot((allEffects(lm_fit)))
#' # Poisson-Modell
#' glm_pois <- glm(hospital_visits ~ age * smoking + bmi_group + cholesterol,
#'                 family = poisson(link = "log"),
#'                 data = hsptl)
#'
#' # Negativ-Binomial-Modell
#' glm_nb <- glm.nb(hospital_visits ~ age * smoking + bmi_group + cholesterol,
#'                  data = hsptl)
#'
#'
#' # Logistisches Regressionsmodell (binomial)
#' glm_binom <- glm(any_visit ~ age * smoking + bmi_group + cholesterol,
#'                  family = binomial(link = "logit"),
#'                  data = hsptl)
#' require(performance)
#' # check_model(lm_fit)       # Residuen, Normalität etc.
#' # check_model(glm_pois)     # Dispersion, Residuen
#' # check_model(glm_nb)       # NB passt meist besser bei Overdispersion
#' # check_model(glm_binom)    # Klassifikationsdiagnostik
#'
#' check_overdispersion(glm_pois)
#' check_overdispersion(glm_nb)
#'
#' compare_performance(lm_fit, glm_pois, glm_nb,#glm_binom,
#'                     rank = TRUE, verbose = TRUE)
#'
#'
#' # Zusammenfassung der Modelle
#' regression_to_df_flexible(ols=lm_fit,
#'                           pois=glm_pois,
#'                           nb= glm_nb,
#'                           binom= glm_binom)
#'
#'
#' library(MASS)      # glm.nb
#' library(lme4)      # glmer.nb
#' library(performance)
#'
#' # Negativ-Binomial ohne Random Effects
#' glm_nb <- glm.nb(hospital_visits ~ age * smoking + bmi_group + cholesterol,
#'                  data = hsptl)
#'
#' # Negativ-Binomial Mixed Effects Modell
#' glmer_nb <- glmer.nb(hospital_visits ~ age * smoking + bmi_group + cholesterol +
#'                        (1 | clinic),
#'                      data = hsptl)
#'
#' #check_model(glm_nb)
#' #check_model(glmer_nb)
#'
#' check_overdispersion(glm_nb)
#' check_overdispersion(glmer_nb)
#' icc(glmer_nb)
#' #ranef(glmer_nb)
#' compare_performance(glm_nb, glmer_nb,
#'                     rank = TRUE, verbose = TRUE)
#'
#' stop()
#'
#' library(lme4)
#'
#'
#' # Lineares Modell (ungeeignet für Counts)
#' lmer_fit<- lmer(hospital_visits ~ age * smoking + bmi_group + cholesterol +
#'                   (1 | clinic),
#'                 data = hsptl)
#'
#' # Poisson Mixed Model
#' glmer_pois <- glmer(hospital_visits ~ age * smoking + bmi_group + cholesterol +
#'                       (1 | clinic),
#'                     family = poisson(link = "log"),
#'                     data = hsptl)
#'
#' # Negativ-Binomial Mixed Model (glmer.nb)
#' glmer_nb <- glmer.nb(hospital_visits ~ age * smoking + bmi_group + cholesterol +
#'                        (1 | clinic),
#'                      data = hsptl)
#'
#' # Logistisches Mixed Model (binär: any_visit)
#' glmer_binom <- glmer(any_visit ~ age * smoking + bmi_group + cholesterol +
#'                        (1 | clinic),
#'                      family = binomial(link = "logit"),
#'                      data = hsptl)
#'
#'
#'
#'
#'
#' regression_to_df_flexible(
#'   ols = lmer_fit,
#'   pois = glmer_pois,
#'   nb = glmer_nb,
#'   binom = glmer_binom)
#'
#'
#'
#' # Modellgüte: AIC
#' #AIC(lm_fit, glm_pois, glm_nb,glm_binom)
#'
#' # Dispersion im Poisson-Modell prüfen
#' dispersion <- sum(resid(glm_pois, type="pearson")^2) / glm_pois$df.residual
#' dispersion
#' #' Wenn dispersion ≈ 1, reicht Poisson.
#' #' Wenn dispersion >> 1, ist Negativ-Binomial deutlich besser.
#' #'
#' # Residualdiagnostik
#' par(mfrow = c(1,3))
#' plot(lm_fit$fitted.values, resid(lm_fit),
#'      main="LM Residuen vs Fitted", xlab="Fitted", ylab="Residuals")
#' abline(h=0, col="red")
#'
#' plot(glm_pois$fitted.values, resid(glm_pois, type="pearson"),
#'      main="Poisson Residuen vs Fitted", xlab="Fitted", ylab="Pearson Residuals")
#' abline(h=0, col="red")
#'
#' plot(glm_nb$fitted.values, resid(glm_nb, type="pearson"),
#'      main="NegBin Residuen vs Fitted", xlab="Fitted", ylab="Pearson Residuals")
#' abline(h=0, col="red")
#'
#'
#'
#' # ROC-Kurve und AUC
#' library(pROC)
#' roc_obj <- roc(hsptl$any_visit, fitted(glm_binom))
#' plot(roc_obj, col="blue", lwd=2, main="ROC-Kurve: Logistic Regression")
#' auc(roc_obj)

