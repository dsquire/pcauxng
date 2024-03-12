df <- read.csv("C:/Users/csquire/OneDrive/Documents/Projects/test-datasetx/mills_allen.csv",
               header = TRUE,
               sep = ",",
               na.strings = "NA")

df$Ethnicity_Elab <- NULL
df$Gender <- NULL
df$Ethnicity <- NULL
df$EnglishNativeLang <- NULL
df$VG_Frequency <- NULL

bad_cols <- check_data(df)

car::vif(model, data = df)

df$AC1_Fail
df$WeekdayPlaytime_1
table(df$WeekdayPlaytime_1)

model <- lm(BPNSF_CF3 ~ BPNSF_AS1 + BPNSF_AF1 + BPNSF_RS1 + BPNSF_RF1 + BPNSF_CS1 + BPNSF_CF1, df)

olsrr::ols_coll_diag(model)
olsrr::ols_vif_tol(model)
olsrr::ols_plot_cooksd_bar(model)
olsrr::ols_plot_resid_stud(model)
olsrr::ols_eigen_cindex(model)
olsrr::ols_step_all_possible(model)
results <- olsrr::ols_step_best_subset(model)
datawizard::data_codebook(df)

qp$l
other_cols <- c("CompSat", "AutoSat", "RelSat", "AutoFrust", "RelFrust", "NeedSat", "Intrinsic", "Introjected", "Amotivation", "IGD", "SC_P2", "IGD_P1", "NeedFrust", "AvgWeekendPlaytime", "SelfControl", "SC_P1")
bad_cols <- c(bad_cols, other_cols)
qp <- mice::quickpred(df, mincor = 0.1, exclude = bad_cols)

# make output a named list
# excluding zero or no variance as predictors is good
# We should drop no variance columns from being predicted
# We should impute near zero variance columns
# read up on pmm

dfcomplete <- mice::mice(df, m = 1, predictorMatrix = qp)
dfcomplete$loggedEvents
table(df$Introjected)

mice::complete(dfcomplete)

ampdf <- mice::ampute(mice::complete(dfcomplete), mech = "MAR")


mycorrs <- correlation::correlation(df, method = "auto", select = other_cols)

datawizard::data_codebook(df)
