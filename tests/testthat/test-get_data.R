test_that("read_text_table works", {

  expect_equal(read_text_table("
  a b c
  1 2 3
  4 5 6
  7 8 NA
  "),
               data.frame(
                 a = c(1, 4, 7),
                 b = c(2, 5, 8),
                 c = c(3, 6, NA)
               ))



})


# test_that("read files works", {
#
#
#   df_csv <- get_data(
#     "../../data/dummy.csv",
#     dec = ",",
#     na.strings = "-",
#     skip = 1,
#     label = 1
#   )
#
#
#   expect_equal(
#     get_label(df_csv),
#     c(
#       id =   "id"  ,
#       group.student = "group student" ,
#       X1.score =  "1 Score" ,
#       y.pct = "y %"  ,
#       z.mol = "z mol"
#     )
#   )
#
#   df_xls <-  get_data("../../data/dummy.xlsx", na.strings = "-")
#   expect_equal(
#     get_label(df_xls),
#     c(
#       id = "id" ,
#       group.student = "group student" ,
#       X1.score = "1 Score" ,
#       y.pct = "y %" ,
#       z.ntl.nt.dn.rcrsvn.dn.prg = "z Anteil nit den recrusiven  + den Progresiven"
#     )
#   )
#
#
#   df_spss  <- get_data("../../data/dummy2.sav")
#   expect_equal(get_label(df_spss),
#                c(
#                  dummy = "dummy" ,
#                  dummy.at.1 = "dummy@1",
#                  osl  = "Ösl"
#                ))
#
# })


test_that("fix_names works", {

  df <- data.frame(
    Öse = c(1, 2, 3, 1, 2, 3),
    Löre = gl(2, 3, labels = c('gro\u00df', 'klein')),
    Fürn = c(9, 7, 6, 8, 6, 9),
    Mäße = c(6, 7, 8, 5, 6, 7),
    hüne=c(1, 2, 3, 1, 2, 3)
  )
  df_fix <- fix_names(df)


  expect_equal(
    names(df_fix),
    c("ose",   "lore",  "furn",  "masse" ,"hune")
 )



})

