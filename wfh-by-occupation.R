library(here)
library(data.table)
library(magrittr)
library(readxl)

# SOC-Census OCC code crosswalk
dt_occ <- fread('occ.txt')

# BLS 2028 projections
dt_ep <- read_excel('occupation.XLSX',
                    skip = 2,
                    sheet = 'Table 1.7') %>%
  data.table() %>%
  .[`...3` == 'Line item',
    .(soccode = `...2`,
      employment = `2018`)] %>%
  dt_occ[., on = 'soccode']

# Dingel and Neiman teleworkability estimates
dt_wfh <- fread('occupations_workathome.csv') %>%
  .[,
    .(teleworkable = mean(teleworkable)),
    .(soccode = substr(onetsoccode,1,7))] %>%
  .[dt_ep, on = 'soccode'] %>%
  .[,
    .(teleworkable = sum(teleworkable * employment) / sum(employment)),
    .(OCC,
      OCC_Title)]

# IPUMS OCC2010 titles and groups
dt_occ2010 <- fread('occ2010.tsv')

# IPUMS IND1990 titles and groups
dt_ind1990 <- fread('ind1990.tsv')


# ACS data from usa.ipums.org
dt_acs <- fread('usa_00265.csv.gz')

# convert 2020 census OCC codes to 2014-2019 codes
dt_occ20 <- fread('occ20.txt') %>%
  .[, .(OCC19, OCC20)]
dt_acs[, OCC20 := 0L]
dt_acs[, OCC20 := OCC]
dt_acs <- dt_occ20[dt_acs, on = 'OCC20']
dt_acs[!is.na(OCC19), OCC := OCC19]
dt_acs[, OCC19 := NULL]

dt_acs <- dt_wfh[dt_acs, on = 'OCC']

dt_acs[, sex := 'Men']
dt_acs[SEX == 2, sex := 'Women']

dt_acs[, age := 'Under 25']
dt_acs[ AGE >= 25, age := '25 to 34']
dt_acs[ AGE >= 35, age := '35 to 44']
dt_acs[ AGE >= 45, age := '45 to 54']
dt_acs[ AGE >= 55, age := '55 to 64']
dt_acs[ AGE >= 65, age := '65 and over']

dt_acs[, educ := 'High school or less']
dt_acs[EDUCD >= 71, educ := 'Some college or associate degree']
dt_acs[EDUCD >= 101, educ := 'Bachelor\'s degree']
dt_acs[EDUCD >= 114, educ := 'Advanced degree']

dt_acs <- dt_occ2010[dt_acs, on = 'OCC2010']
dt_acs <- dt_ind1990[dt_acs, on = 'IND1990']

# summarize for charts
dt_acs.sex <- dt_acs %>%
  .[EMPSTAT == 1
    & !is.na(teleworkable),
    .(workers = sum(PERWT),
      wfh_baseline = sum(PERWT * (TRANWORK == 80)) / sum(PERWT),
      pct_teleworkable = sum(PERWT * (teleworkable)) / sum(PERWT)),
    .(value = sex)] %>%
  .[, variable := 'Sex']

dt_acs.age <- dt_acs %>%
  .[EMPSTAT == 1
    & !is.na(teleworkable),
    .(workers = sum(PERWT),
      wfh_baseline = sum(PERWT * (TRANWORK == 80)) / sum(PERWT),
      pct_teleworkable = sum(PERWT * (teleworkable)) / sum(PERWT)),
    .(value = age)] %>%
  .[, variable := 'Age']

dt_acs.educ <- dt_acs %>%
  .[EMPSTAT == 1
    & !is.na(teleworkable),
    .(workers = sum(PERWT),
      wfh_baseline = sum(PERWT * (TRANWORK == 80)) / sum(PERWT),
      pct_teleworkable = sum(PERWT * (teleworkable)) / sum(PERWT)),
    .(value = educ)] %>%
  .[, variable := 'College']

dt_acs.occ <- dt_acs %>%
  .[EMPSTAT == 1
    & !is.na(teleworkable),
    .(workers = sum(PERWT),
      wfh_baseline = sum(PERWT * (TRANWORK == 80)) / sum(PERWT),
      pct_teleworkable = sum(PERWT * (teleworkable)) / sum(PERWT)),
    .(value = OccGroup)] %>%
  .[, variable := 'Occupation']

dt_acs.det <- dt_acs %>%
  .[EMPSTAT == 1
    & !is.na(teleworkable),
    .(workers = sum(PERWT),
      wfh_baseline = sum(PERWT * (TRANWORK == 80)) / sum(PERWT),
      pct_teleworkable = sum(PERWT * (teleworkable)) / sum(PERWT)),
    .(value = Occupation)] %>%
  .[, variable := 'Detailed occupation']

dt_acs.ind <- dt_acs %>%
  .[EMPSTAT == 1
    & !is.na(teleworkable),
    .(workers = sum(PERWT),
      wfh_baseline = sum(PERWT * (TRANWORK == 80)) / sum(PERWT),
      pct_teleworkable = sum(PERWT * (teleworkable)) / sum(PERWT)),
    .(value = IndGroup)] %>%
  .[, variable := 'Industry']

dt_acs.grp <- dt_acs.sex %>%
  rbind(dt_acs.age) %>%
  rbind(dt_acs.educ) %>%
  rbind(dt_acs.occ) %>%
  rbind(dt_acs.det) %>%
  rbind(dt_acs.ind)


# CPS data from cps.ipums.org
dt_cps <- fread('cps_00166.csv.gz')

dt_cps <- dt_cps[EMPSTAT %in% 10:12]

dt_cps[, date := as.Date(paste0(YEAR, '-', MONTH, '-01'))]

dt_cps[, sex := 'Men']
dt_cps[SEX == 2, sex := 'Women']

dt_cps[, age := 'Under 25']
dt_cps[ AGE >= 25, age := '25 to 34']
dt_cps[ AGE >= 35, age := '35 to 44']
dt_cps[ AGE >= 45, age := '45 to 54']
dt_cps[ AGE >= 55, age := '55 to 64']
dt_cps[ AGE >= 65, age := '65 and over']

dt_cps[, educ := 'High school or less']
dt_cps[EDUC >= 81, educ := 'Some college or associate degree']
dt_cps[EDUC >= 110, educ := 'Bachelor\'s degree']
dt_cps[EDUC >= 123, educ := 'Advanced degree']

dt_cps <- dt_occ2010[dt_cps, on = 'OCC2010']
dt_cps <- dt_ind1990[dt_cps, on = 'IND1990']

dt_cps %>%
  .[date %in% as.Date(c('2020-05-01', '2022-09-01')),
    .(workers = sum(WTFINL),
      wfh = sum(WTFINL * (COVIDTELEW == 2)) / sum(WTFINL)),
    .(date = format(date, '%b_%Y'), educ)] %>%
  .[, workersPct := workers / sum(workers), date] %>%
  .[]

# summarize for charts
dt_cps.sex <- dt_cps %>%
  .[date %in% as.Date(c('2020-05-01', '2022-09-01')),
    .(wfh = sum(WTFINL * (COVIDTELEW == 2)) / sum(WTFINL)),
    .(date = format(date, '%b_%Y'),
      value = sex)] %>%
  dcast(value ~ date, value.var = 'wfh') %>%
  .[, variable := 'Sex']

dt_cps.age <- dt_cps %>%
  .[date %in% as.Date(c('2020-05-01', '2022-09-01')),
    .(wfh = sum(WTFINL * (COVIDTELEW == 2)) / sum(WTFINL)),
    .(date = format(date, '%b_%Y'),
      value = age)] %>%
  dcast(value ~ date, value.var = 'wfh') %>%
  .[, variable := 'Age']

dt_cps.educ <- dt_cps %>%
  .[date %in% as.Date(c('2020-05-01', '2022-09-01')),
    .(wfh = sum(WTFINL * (COVIDTELEW == 2)) / sum(WTFINL)),
    .(date = format(date, '%b_%Y'),
      value = educ)] %>%
  dcast(value ~ date, value.var = 'wfh') %>%
  .[, variable := 'College']

dt_cps.occ <- dt_cps %>%
  .[date %in% as.Date(c('2020-05-01', '2022-09-01')),
    .(wfh = sum(WTFINL * (COVIDTELEW == 2)) / sum(WTFINL)),
    .(date = format(date, '%b_%Y'),
      value = OccGroup)] %>%
  dcast(value ~ date, value.var = 'wfh') %>%
  .[, variable := 'Occupation']

dt_cps.det <- dt_cps %>%
  .[date %in% as.Date(c('2020-05-01', '2022-09-01')),
    .(wfh = sum(WTFINL * (COVIDTELEW == 2)) / sum(WTFINL)),
    .(date = format(date, '%b_%Y'),
      value = Occupation)] %>%
  dcast(value ~ date, value.var = 'wfh') %>%
  .[, variable := 'Detailed occupation']

dt_cps.ind <- dt_cps %>%
  .[date %in% as.Date(c('2020-05-01', '2022-09-01')),
    .(wfh = sum(WTFINL * (COVIDTELEW == 2)) / sum(WTFINL)),
    .(date = format(date, '%b_%Y'),
      value = IndGroup)] %>%
  dcast(value ~ date, value.var = 'wfh') %>%
  .[, variable := 'Industry']

dt_cps.grp <- dt_cps.sex %>%
  rbind(dt_cps.age) %>%
  rbind(dt_cps.educ) %>%
  rbind(dt_cps.occ) %>%
  rbind(dt_cps.det) %>%
  rbind(dt_cps.ind)


# combine ACS and CPS data and sort for charts
dt_grp <- dt_cps.grp[dt_acs.grp, on = c('variable','value')]

dt_grp <- dt_grp[(variable != 'Detailed occupation' & workers >= 1000000)
                 | (variable == 'Detailed occupation' & workers >= 500000 & May_2020 >= 0.3)]

dt_grp[, value := paste0(substr(value,1,1), tolower(substr(value,2,nchar(value))))]

dt_grp <- dt_grp[order(variable, -(wfh_baseline + May_2020))]

dt_grp[, sort := 1:.N, variable]

dt_grp[value == 'High school or less', sort := 4]
dt_grp[value == 'Some college or associate degree', sort := 3]
dt_grp[value == 'Bachelor\'s degree', sort := 2]
dt_grp[value == 'Advanced degree', sort := 1]

dt_grp[value == 'Under 25', sort := 1]
dt_grp[value == '25 to 34', sort := 2]
dt_grp[value == '35 to 44', sort := 3]
dt_grp[value == '45 to 54', sort := 4]
dt_grp[value == '55 to 64', sort := 5]
dt_grp[value == '65 and over', sort := 6]

dt_grp <- dt_grp[order(variable, sort)]

dt_grp[, groupPct := workers / sum(workers), .(variable)]
dt_grp[, groupPctPrev := shift(cumsum(groupPct)), .(variable)]
dt_grp[is.na(groupPctPrev), groupPctPrev := 0]

fwrite(dt_grp, 'wfh-by-occ.csv')


# time-series chart
dt_acs.ts <- dt_acs %>%
  .[EMPSTAT == 1
    & !is.na(teleworkable),
    .(wfh_baseline = sum(PERWT * (TRANWORK == 80)) / sum(PERWT))]

dt_ts <- dt_cps %>%
  .[,
    .(telework = sum(WTFINL * (COVIDTELEW == 2)) / sum(WTFINL)),
    .(date)] %>%
  .[, wfh_baseline := dt_acs.ts$wfh_baseline]

fwrite(dt_ts, 'wfh-ts.csv')


# occupation comparison
dt_acs.det <- dt_acs %>%
  .[EMPSTAT == 1
    & !is.na(teleworkable),
    .(workers = sum(PERWT),
      wfh_baseline = sum(PERWT * (TRANWORK == 80)) / sum(PERWT),
      pct_teleworkable = sum(PERWT * (teleworkable)) / sum(PERWT)),
    .(OCC, OCC_Title)]

dt_cps.det <- dt_cps %>%
  .[date %in% as.Date(c('2020-05-01', '2022-09-01')),
    .(wfh = sum(WTFINL * (COVIDTELEW == 2)) / sum(WTFINL)),
    .(date = format(date, '%b_%Y'),
      OCC)] %>%
  dcast(OCC ~ date, value.var = 'wfh')

dt_det <- dt_cps.det[dt_acs.det, on = 'OCC']

fwrite(dt_det, 'wfh-occ.csv')

# servr::httd()


bitmap <- rsvg::rsvg('chart1.svg', css = "style.css", width = 1700)
png::writePNG(bitmap, "chart1.png", dpi = 400)

bitmap <- rsvg::rsvg('chart2.svg', css = "style.css", width = 1700)
png::writePNG(bitmap, "chart2.png", dpi = 400)

bitmap <- rsvg::rsvg('chart3.svg', css = "style.css", width = 1700)
png::writePNG(bitmap, "chart3.png", dpi = 400)

bitmap <- rsvg::rsvg('chart4.svg', css = "style.css", width = 1700)
png::writePNG(bitmap, "chart4.png", dpi = 400)


# 2021 telework estimate
dt_grp %>%
  .[,
    .(workers = sum(workers),
      groupPct = sum(groupPct),
      May_2020 = sum(May_2020 * groupPct)),
    .(variable)]

dt_acs[AGE >= 25 & EMPSTAT %in% c(1), .(perwt = sum(PERWT)), .(educ)] %>%
  .[, pct := perwt / sum(perwt)] %>%
  .[]
