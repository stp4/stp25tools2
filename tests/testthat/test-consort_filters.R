# library(testthat)
# library(dplyr)


  # Testdatensatz erstellen
  set.seed(0815)
  n <- 100
  
  test_data <- data.frame(
    docu = rbinom(n, 1, .1) == 1,
    group = sample(c("HM", "M/UM", "10K"), n, replace = TRUE),
    sex = sample(c("male", "female"), n, replace = TRUE),
    age = round(runif(n, 16, 80)),
    outcome = rnorm(n),
    measure = rnorm(n)
  )
  
  # Fehlende Werte hinzufügen für realistische Tests
  test_data$group[sample.int(n, 3)] <- NA
  test_data$sex[sample.int(n, 2)] <- NA
  test_data$outcome[sample.int(n, 5)] <- NA
  

  
  # Test 1: Einfacher Filter mit Exclusion()
  test_that("Exclusion() filtert korrekt", {
    result <- Exclusion(
      test_data,
      docu ~ "missing documentation",
      age < 18 ~ "age <18",
      is.na(group) ~ "missing group",
      is.na(sex) ~ "missing sex"
    )
    
    # Manuelles Filter mit dplyr::filter für Vergleich
    manual_filtered <- test_data %>%
      dplyr::filter(
        !docu,
        age >= 18,
        !is.na(group),
        !is.na(sex)
      )
    
    # Anzahl der verbleibenden Fälle sollte gleich sein
    expect_equal(
      sum(result$FILTER),
      nrow(manual_filtered)
    )
    
    # Die gefilterten Daten sollten identisch sein
    result_filtered <- result %>% dplyr::filter(FILTER)
    expect_equal(
      nrow(result_filtered),
      nrow(manual_filtered)
    )
    
    # trialno sollte fortlaufend sein
    expect_equal(
      result$trialno,
      1:nrow(test_data)
    )
  })
  

  
  # Test 3: Levels werden in korrekter Reihenfolge beibehalten
  test_that("Levels Reihenfolge ist korrekt", {
    result <- Exclusion(
      test_data,
      docu ~ "missing documentation",
      age < 18 ~ "age under 18", 
      is.na(group) ~ "missing group data"
    )
    
    # Levels sollten in der Übergabe-Reihenfolge sein
    expected_levels <- c("missing documentation", "age under 18", "missing group data")
    expect_equal(levels(result$exclusion), expected_levels)
    
    # Faktor sollte korrekt kodiert sein
    expect_s3_class(result$exclusion, "factor")
  })
  



test_that("Konsistenz mit consort::consort_plot Anforderungen", {
  # Testdatensatz für CONSORT-spezifische Tests
  consort_data <- data.frame(
    docu = c(TRUE, FALSE, FALSE, FALSE),
    group = c("A", "B", NA, "A"),
    sex = c("male", "female", "male", NA),
    age = c(16, 25, 30, 40),
    outcome = c(1, 2, NA, 3),
    measure = c(1, NA, 2, 3)
  )
  
  result <- consort_data %>%
    Exclusion(
      docu ~ "Documentation missing",
      age < 18 ~ "Underage",
      is.na(group) ~ "Group missing",
      is.na(sex) ~ "Sex missing"
    ) %>%
    Followup(
      is.na(outcome) ~ "Outcome missing",
      is.na(measure) ~ "Measure missing"
    )
  
  # Test für consort_plot Kompatibilität
  expect_true("trialno" %in% names(result))
  expect_true("exclusion" %in% names(result)) 
  expect_true("lost_followup" %in% names(result))
  expect_true("followup" %in% names(result))
  expect_true("FILTER" %in% names(result))
  
  # trialno sollte eindeutig sein
  expect_equal(length(unique(result$trialno)), nrow(result))
  
  # exclusion und lost_followup sollten Factors sein
  expect_s3_class(result$exclusion, "factor")
  expect_s3_class(result$lost_followup, "factor")
})
