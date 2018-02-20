library(httr)
library(tidyverse)
library(jsonlite)
library(meetupr)
library(lubridate)
library(forecast)

# Get your Meetup key by going to
# https://secure.meetup.com/meetup_api/key/
MY_MEETUP_KEY = "MYMEETUPKEY"

########################################
###    .date_helper from rladies     ###
###     meetupr/internals.R          ###
###     converts milliseconds from   ###
###     Meetup API request into      ###
###     date/time a la               ###
###     2018-02-08 08:57:05 EST      ###
########################################
# helper function to convert a vector of milliseconds
# since epoch into POSIXct
.date_helper <- function(time) {
  if (is.character(time)) {
    # if date is character string, try to convert to numeric
    time <- tryCatch(expr = as.numeric(time),
                     error = warning("One or more dates could
                                     not be converted properly"))
  }
  if (is.numeric(time)) {
    # divide milliseconds by 1000 to get seconds; convert to POSIXct
    seconds <- time / 1000
    out <- as.POSIXct(seconds, origin = "1970-01-01")
  } else {
    # if no conversion can be done, then return NA
    warning("One or more dates could not be converted properly")
    out <- rep(NA, length(time))
  }
  return(out)
  }

# Get the Yes RSVPs for a specified event
# id_name is a descriptor of your choice
# and event_id appears at the end of the Meetup.com URL
# e.g., the number in
# https://www.meetup.com/Research-Triangle-Analysts/
# events/246678392/
meetup_yes_RSVPs <- function(id_name, event_id){
  meetup_url  <- paste0('https://api.meetup.com/2/rsvps?key=',
                        MY_MEETUP_KEY,
                        '&event_id=', event_id, '&order=name')
  meetup_resp <- GET(meetup_url)
  # 200 means success (OK)
  meetup_resp_code <- meetup_resp$status_code
  if (meetup_resp_code != 200){
    warning(paste0('Warning: url returned unexpected status code.
                   Expected 200 (OK)
                   and received ', meetup_resp_code,
                   'for event id = ', event_id))
  }
  # Based on Brian Fannin's Research Triangle Analysts
  # "Go Ape for APIs!" talk:
  # https://github.com/PirateGrunt/ape4apis/blob/master/index.Rmd
  # https://www.meetup.com/Research-Triangle-Analysts/events/237582947/

  the_content   <- content(meetup_resp, "text",
                           encoding = "UTF-8")
  # Use flatten = TRUE because data frame is at least sometimes nested,
  # which creates problems with my dplyr use
  df_content     <- data.frame(fromJSON(the_content,
                                        simplifyDataFrame =  TRUE,
                                        flatten = TRUE)) %>%
    filter(results.response=='yes') %>%
    select(results.mtime)
  # Does not count guests (df_content$results.guests)
  df_content$dates_yes <-  lubridate::ymd(stringr::str_sub(
    .date_helper(df_content$results.mtime),
    1 ,10))
  df_content           <-    df_content %>%
    group_by(dates_yes) %>%
    summarise(dates_yes_ct=n()) %>%
    ungroup()
  id_name      <- as.character(id_name)
  dates_yes    <- unique(df_content$dates_yes)
  dates_yes_ct <- df_content$dates_yes_ct
  # stringsAsFactors = FALSE to avoid warnings
  # about factor level differences when I bind_rows later
  df_out       <- data.frame(id_name, dates_yes, dates_yes_ct,
                             stringsAsFactors = FALSE)
}
# manually extracted from URLs like
# https://www.meetup.com/Research-Triangle-Analysts/events/246678392/
AF18_id  <- '246678392'
AF17_id  <- '237118943'
AF16_id  <- '228455037'
AF15_id  <- '219885748'
AF18_frm <- meetup_yes_RSVPs("AF18", AF18_id)
AF17_frm <- meetup_yes_RSVPs("AF17", AF17_id)
AF16_frm <- meetup_yes_RSVPs("AF16", AF16_id)
AF15_frm <- meetup_yes_RSVPs("AF15", AF15_id)

allAF_frm    <- bind_rows(AF15_frm, AF16_frm, AF17_frm, AF18_frm) %>%
                 arrange(id_name, dates_yes) %>%
                 group_by(id_name) %>%
                 mutate(day_counter = row_number()) %>%
                 ungroup()

AF18_ts      <- ts(allAF_frm$dates_yes_ct, start=allAF_frm$day_counter[1], end=allAF_frm$day_counter[10])

# add days and month column as inspired by
# StackOverflow user NicE on
# https://stackoverflow.com/questions/28503262/using-lubridate-and-ggplot2-effectively-for-date-axis
allAF_frm$yes_day   <- lubridate::day(allAF_frm$dates_yes)
allAF_frm$yes_month <- lubridate::month(allAF_frm$dates_yes)
allAF_frm$yes_year  <- lubridate::year(allAF_frm$dates_yes)
allAF_frm$dates_yes_otheryear <- as.Date(
  format(
    # 2019 used in below line because future year
    # denotes false year imposed to homogenize
    # year for ggplot printing on same axes
    allAF_frm$dates_yes,"%d-%m-2019"),
  format="%d-%m-%y")
allAF_frm            <- allAF_frm %>% group_by(id_name) %>%
  mutate(dates_yes_cumsum = cumsum(dates_yes_ct)) %>%
  ungroup()
# Note for future self:
#  This ggplot error may mean parentheses closed after
#  geom lines (e.g., geom_point...) :
#    Error in aes(x = factor(dates_yes),
#      y = dates_yes_ct, colour = id_name) +
#      non-numeric argument to binary operator
ggplot(data = allAF_frm,
       aes(x = dates_yes_otheryear,
           y = dates_yes_cumsum,
           colour = factor(yes_year))) +
  geom_line() +
  scale_x_date(date_labels = "%b") +
  xlab("Month of year") +
  ylab("YES (will attend) RSVPs from Meetup.com API") +
  ggtitle(label = paste0("Research Triangle Analysts 'Analytics>Forward' as of  ",
                         Sys.Date()),
          subtitle = "$10 includes meals. Mara Averick keynoting March 10, 2018.") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_colour_discrete("Year") +
  annotate("path",
           x=ymd('2020-01-28')+1*cos(seq(0,2*pi,length.out=25)),
           y=18+8*sin(seq(0,2*pi,length.out=25))) +
  annotate("text", x = ymd('2020-01-29'), y = 30,
           label = "What happened on 2018-01-29?",
           size = 4)

fit <- auto.arima(WWWusage)
checkresiduals(fit)



WWWusage %>% ets %>% forecast(h=20) -> fc
autoplot(WWWusage, series="Data") +
  autolayer(fc, series="Forecast") +
  autolayer(fitted(fc), series="Fitted")

# RE: annotate("path" t0 make a transparent circle:
# Credit StackOverflow user Luis who mysteriously
# indicates nothing about himself in his profile
# and has asked 0 questions, and provided only 1
# answer, which I used.
# https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2

# I wanted to try ggforce::geom_circle but gave up
# because (on Windows) package install failed with:
# Installation failed: Could not find build tools
# necessary to build ggforce
# and this was after RStudio attempted an auto-install
# of Rtools34.exe, which
# terminated without error after files were extracted.
