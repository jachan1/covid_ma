#' ---
#' title: "COVID-19 Tests by Town"
#' author: ""
#' ---


#+ echo=F, warning=F, message=F

require(ggplot2)
require(dplyr)
require(tidyr)
require(readxl)

covid_hist_fldr <- "/Users/jamesbridget/Documents/covid_ma/"
covid_hist_fl <- file.path(covid_hist_fldr, "saved_data", "covid_%s.csv")

saved_fls <- list.files(file.path(covid_hist_fldr, "saved_data"))
start_dt <- max(as.Date(gsub(".csv|^covid_", "", saved_fls), format="%b-%d-%Y"))
days_since = as.numeric(Sys.Date() - start_dt)

for(i in 1:days_since){
  dt_dt <- start_dt + i
  dt <- tolower(sprintf("%s-%g-%s", format(dt_dt, "%B"), as.numeric(format(dt_dt, "%d")), format(dt_dt, "%Y")))
  tf <- sprintf("https://www.mass.gov/doc/weekly-public-health-report-raw-data-%s/download", dt)
  sv_fl <- "/Users/jamesbridget/Downloads/today_file.xlsx"
  err_catch <- tryCatch(download.file(tf, destfile = sv_fl), error=function(e) "err", warning=function(w) "err")
  if(class(err_catch) == "character") {
    
  } else {
    shts <- excel_sheets(sv_fl)
    county_data <- read_excel(sv_fl, sheet=grep("city_town", shts, value=T, ignore.case = T))
    cols <- if("Percent Positivity (Last 14 Days)" %in% names(county_data)){
      c(town = "City/Town",
        tests_total = "Total Tested (Last 14 days)",
        tests_pos = "Total Positive Tests (Last 14 days)")
    } else if("Percent Positive" %in% names(county_data)){
      c(town = "City/Town",
        tests_total = "Total Persons Tested",
        tests_pos = "Count")
    } else if("Positive Tests Last 14 days" %in% names(county_data)) {
      c(town = "City/Town",
        tests_total = "Total tests last 14 days",
        tests_pos = "Two Week Case Count")
    } else {
      next
    }
    
    county_data %>% 
      select(all_of(cols)) %>% 
      mutate(dt=dt_dt,
             tests_pos = as.character(tests_pos),
             tests_posn = as.numeric(gsub("<5", "2", tests_pos))) %>% 
      write.csv(sprintf(covid_hist_fl, dt), row.names=F)
  }
}

all_fls <- list.files(file.path(covid_hist_fldr, "saved_data"), full.names = T, pattern=".csv")

covid_all_raw <- lapply(all_fls, function(fl) {
  read.csv(fl, stringsAsFactors = F) %>% 
    mutate(tests_pos = as.numeric(gsub("<5", "2", tests_pos)))
}) %>% 
  bind_rows()

covid_j1 <- covid_all_raw %>% 
  filter(difftime("2020-07-10", dt) > 0) %>% 
  select(town, tests_total, tests_posn, dt) %>% 
  pivot_wider(names_from=c(dt), values_from=c(tests_total, tests_posn)) %>% 
  mutate(dt = "2020-07-01",
         tests_total = `tests_total_2020-07-01` - `tests_total_2020-06-24`,
         tests_posn = `tests_posn_2020-07-01` - `tests_posn_2020-06-24`) %>% 
  select(town, tests_total, tests_posn, dt)

towns <- tribble(
  ~town, ~pop, ~student_pop,
  "Boston", 692600, 50480,
  "Revere", 53073, 7532,
  "Everett", 46451, 7057,
  "Somerville", 81360, 4939,
  "Chelsea", 39690, 6255,
  "Winthrop", 18544, 1965,
  "Malden", 60470, 6418
) %>% 
  mutate(keep=T)

covid_all <- bind_rows(covid_all_raw %>% filter(difftime("2020-07-10", dt) < 0), covid_j1) %>% 
  left_join(towns) %>% 
  mutate(pct_pos = tests_posn / tests_total,
         alpha=ifelse(town=="Chelsea", "p", "s"),
         n_covid_students = student_pop  * tests_posn / pop)


#' Percent of tests that returned positive
#' Note: Boston testing protocol likely differs from other towns due to colleges testing

#+ echo=F, warning=F, message=F

covid_all %>% 
  filter(keep %in% T) %>% 
  ggplot(aes(x=dt, y=pct_pos, group=town, color=town, alpha=alpha)) +
  geom_point() + 
  geom_line() +
  geom_hline(yintercept=0.05, linetype=2, color="red") + 
  scale_alpha_manual(breaks=c("p", "s"), values=c(1, 0.5), guide=F) + 
  scale_y_continuous(breaks=c(1:10, seq(20, 100, 10))/100, labels = scales::percent_format(accuracy = 1)) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  ylab("Percent of tests returned positive") + 
  xlab("")

#' Number of students to expect to have COVID
#' Assuming that all cases in the past 2 weeks have been identified and are evening distributed across the population. 

#+ echo=F, warning=F, message=F

covid_all %>% 
  filter(keep %in% T) %>% 
  ggplot(aes(x=dt, y=n_covid_students, group=town, color=town, alpha=alpha)) +
  geom_point() + 
  geom_line() +
  scale_alpha_manual(breaks=c("p", "s"), values=c(1, 0.5), guide=F) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  ylab("Potential Number of Students Currently With COVID") + 
  xlab("")

#' number of positive tests

#+ echo=F, warning=F, message=F

covid_all %>% 
  filter(keep %in% T) %>% 
  ggplot(aes(x=dt, y=tests_posn, group=town, color=town, alpha=alpha)) +
  geom_point() + 
  geom_line() +
  scale_alpha_manual(breaks=c("p", "s"), values=c(1, 0.5), guide=F)  + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  ylab("Total positive tests in past 2 weeks") + 
  xlab("")

#' Number of tests conducted

#+ echo=F, warning=F, message=F

covid_all %>% 
  filter(keep %in% T) %>% 
  ggplot(aes(x=dt, y=tests_total, group=town, color=town, alpha=alpha)) +
  geom_point() + 
  geom_line() +
  scale_alpha_manual(breaks=c("p", "s"), values=c(1, 0.5), guide=F)   + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  ylab("Total tests in past 2 weeks") + 
  xlab("")
