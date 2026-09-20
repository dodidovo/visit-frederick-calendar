# =========================================================
# VISIT FREDERICK EVENT SCRAPER
# =========================================================

library(httr2)
library(xml2)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)


# =========================================================
# 1. GET VISIT FREDERICK RSS FEED
# =========================================================

rss_url <- "https://www.visitfrederick.org/event/rss/"

response <- request(rss_url) |>
  req_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/153.0.0.0 Safari/537.36",
    `Accept` = "application/rss+xml, application/xml, text/xml, */*",
    `Accept-Language` = "en-US,en;q=0.9"
  ) |>
  req_perform()

rss_text <- resp_body_string(response)

rss <- read_xml(rss_text)

items <- xml_find_all(rss, ".//item")

cat("RSS events found:", length(items), "\n\n")


# =========================================================
# 2. GET EVENT TITLES AND LINKS
# =========================================================

events <- map_dfr(items, function(item) {
  
  tibble(
    title = xml_text(xml_find_first(item, "./title")),
    url = xml_text(xml_find_first(item, "./link"))
  )
})


# =========================================================
# 3. FUNCTION TO SCRAPE EACH EVENT PAGE
# =========================================================

get_event_details <- function(url) {
  
  tryCatch({
    
    # -----------------------------------------------------
    # Download event page
    # -----------------------------------------------------
    
    page_response <- request(url) |>
      req_headers(
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/153.0.0.0 Safari/537.36",
        `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        `Accept-Language` = "en-US,en;q=0.9"
      ) |>
      req_perform()
    
    page_html <- resp_body_string(page_response)
    
    page <- read_html(page_html)
    
    
    # =====================================================
    # 4. FIND JSON-LD INFORMATION
    # =====================================================
    
    json_ld <- html_elements(
      page,
      'script[type="application/ld+json"]'
    )
    
    json_text <- html_text(json_ld)
    
    
    # Visit Frederick uses different event types,
    # such as Event and EducationEvent.
    #
    # Therefore, look for startDate/endDate instead
    # of requiring @type = Event.
    
    event_json <- json_text[
      str_detect(
        json_text,
        regex(
          '"startDate"\\s*:\\s*"',
          ignore_case = TRUE
        )
      ) &
        str_detect(
          json_text,
          regex(
            '"endDate"\\s*:\\s*"',
            ignore_case = TRUE
          )
        )
    ]
    
    
    # =====================================================
    # 5. INITIALIZE VALUES
    # =====================================================
    
    start_date <- as.Date(NA)
    end_date <- as.Date(NA)
    description <- NA_character_
    
    
    # =====================================================
    # 6. EXTRACT DATES AND DESCRIPTION
    # =====================================================
    
    if (length(event_json) > 0) {
      
      # Start date
      
      start_match <- str_match(
        event_json[1],
        '"startDate"\\s*:\\s*"([^"]+)"'
      )
      
      
      # End date
      
      end_match <- str_match(
        event_json[1],
        '"endDate"\\s*:\\s*"([^"]+)"'
      )
      
      
      # Description
      
      description_match <- str_match(
        event_json[1],
        '"description"\\s*:\\s*"([^"]*)"'
      )
      
      
      # Convert start date
      
      if (!is.na(start_match[1, 2])) {
        
        start_date <- as.Date(
          str_sub(start_match[1, 2], 1, 10)
        )
        
      }
      
      
      # Convert end date
      
      if (!is.na(end_match[1, 2])) {
        
        end_date <- as.Date(
          str_sub(end_match[1, 2], 1, 10)
        )
        
      }
      
      
      # Save description
      
      if (!is.na(description_match[1, 2])) {
        
        description <- description_match[1, 2]
        
      }
      
    }
    
    
    # =====================================================
    # 7. FIND RECURRENCE
    # =====================================================
    
    # We specifically search for "Recurring..."
    #
    # This avoids accidentally capturing the site's
    # interface label:
    #
    # "recurrence":"Recurrence"
    
    recurrence_candidates <- str_extract_all(
      page_html,
      regex(
        "Recurring\\s+[^\"'<]{1,150}",
        ignore_case = TRUE
      )
    )[[1]]
    
    
    # Only keep recurrence text that contains
    # an actual weekday.
    
    recurrence_candidates <- recurrence_candidates[
      str_detect(
        recurrence_candidates,
        regex(
          "Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday",
          ignore_case = TRUE
        )
      )
    ]
    
    
    # Save recurrence
    
    if (length(recurrence_candidates) > 0) {
      
      recurrence <- recurrence_candidates[1] |>
        str_replace_all("\\\\\"", "\"") |>
        str_replace_all("\\\\/", "/") |>
        str_squish()
      
    } else {
      
      recurrence <- NA_character_
      
    }
    
    
    # =====================================================
    # 8. RETURN EVENT DETAILS
    # =====================================================
    
    tibble(
      start_date = start_date,
      end_date = end_date,
      description = description,
      recurrence = recurrence
    )
    
    
  }, error = function(e) {
    
    # If a page fails, return an empty row rather
    # than stopping the entire scraper.
    
    tibble(
      start_date = as.Date(NA),
      end_date = as.Date(NA),
      description = NA_character_,
      recurrence = NA_character_
    )
    
  })
}


# =========================================================
# 9. SCRAPE ALL EVENT PAGES
# =========================================================

cat("Scraping event pages...\n")

details <- map_dfr(
  events$url,
  get_event_details
)


# =========================================================
# 10. COMBINE RSS AND PAGE INFORMATION
# =========================================================

events <- bind_cols(
  events,
  details
)


# =========================================================
# 11. REMOVE EVENTS WITHOUT VALID DATES
# =========================================================

events <- events |>
  filter(
    !is.na(start_date),
    !is.na(end_date)
  )


# =========================================================
# 12. SHOW SCRAPING RESULTS
# =========================================================

cat("\n")
cat("Total events:", nrow(events), "\n")

cat(
  "Events with recurrence:",
  sum(!is.na(events$recurrence)),
  "\n\n"
)


# =========================================================
# 13. DISPLAY ALL SCRAPED EVENTS
# =========================================================

events |>
  select(
    title,
    start_date,
    end_date,
    recurrence
  ) |>
  print(
    n = Inf,
    width = Inf
  )


# =========================================================
# 14. CREATE INDIVIDUAL EVENT OCCURRENCES
# =========================================================

create_occurrences <- function(event) {
  
  
  # -------------------------------------------------------
  # NON-RECURRING EVENT
  # -------------------------------------------------------
  
  if (is.na(event$recurrence)) {
    
    return(
      tibble(
        title = event$title,
        url = event$url,
        description = event$description,
        date = event$start_date,
        original_end_date = event$end_date,
        recurrence = NA_character_
      )
    )
  }
  
  
  # -------------------------------------------------------
  # RECURRING EVENT
  # -------------------------------------------------------
  
  recurrence_part <- str_remove(
    event$recurrence,
    regex(
      "^Recurring weekly on\\s*",
      ignore_case = TRUE
    )
  )
  
  
  # -------------------------------------------------------
  # WEEKDAY NAMES
  # -------------------------------------------------------
  
  days <- c(
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday"
  )
  
  
  # -------------------------------------------------------
  # FIND THE WEEKDAYS IN THE RECURRENCE
  # -------------------------------------------------------
  
  matching_days <- days[
    vapply(
      days,
      function(day) {
        
        str_detect(
          recurrence_part,
          regex(
            paste0("\\b", day, "\\b"),
            ignore_case = TRUE
          )
        )
        
      },
      logical(1)
    )
  ]
  
  
  # -------------------------------------------------------
  # CREATE EVERY DATE IN THE EVENT RANGE
  # -------------------------------------------------------
  
  all_dates <- seq(
    from = event$start_date,
    to = event$end_date,
    by = "day"
  )
  
  
  # -------------------------------------------------------
  # GET WEEKDAY FOR EACH DATE
  # -------------------------------------------------------
  
  date_weekdays <- weekdays(all_dates)
  
  
  # -------------------------------------------------------
  # KEEP ONLY ACTUAL RECURRING WEEKDAYS
  # -------------------------------------------------------
  
  occurrence_dates <- all_dates[
    date_weekdays %in% matching_days
  ]
  
  
  # -------------------------------------------------------
  # RETURN ONE ROW PER OCCURRENCE
  # -------------------------------------------------------
  
  tibble(
    title = event$title,
    url = event$url,
    description = event$description,
    date = occurrence_dates,
    original_end_date = event$end_date,
    recurrence = event$recurrence
  )
}


# =========================================================
# 15. EXPAND ALL EVENTS
# =========================================================

calendar_events <- map_dfr(
  seq_len(nrow(events)),
  function(i) {
    
    create_occurrences(
      events[i, ]
    )
    
  }
)


# =========================================================
# 16. REMOVE PAST OCCURRENCES
# =========================================================

calendar_events <- calendar_events |>
  filter(
    if_else(
      is.na(recurrence),
      original_end_date >= Sys.Date(),
      date >= Sys.Date()
    )
  )

# =========================================================
# 17. SORT EVENTS
# =========================================================

calendar_events <- calendar_events |>
  arrange(
    date,
    title
  )


# =========================================================
# 18. SUMMARY
# =========================================================

cat("\n")
cat(
  "Today's date:",
  as.character(Sys.Date()),
  "\n"
)

cat(
  "Total calendar occurrences:",
  nrow(calendar_events),
  "\n\n"
)


# =========================================================
# 19. SHOW EVENTS BY DATE
# =========================================================

cat("Events by date:\n\n")

calendar_events |>
  count(date) |>
  print(
    n = Inf
  )


# =========================================================
# 20. SHOW THE FIRST 100 CALENDAR EVENTS
# =========================================================

cat("\n")
cat("Calendar events:\n\n")

calendar_events |>
  select(
    date,
    title,
    recurrence
  ) |>
  print(
    n = 100,
    width = Inf
  )

#check
calendar_events |>
  filter(!is.na(recurrence)) |>
  select(date, title, recurrence) |>
  print(n = 50, width = Inf)

#check
calendar_events |>
  filter(date >= Sys.Date()) |>
  count(date)

# =========================================================
# 21. PREPARE EVENTS FOR ICALENDAR
# =========================================================

escape_ics <- function(x) {
  
  x <- ifelse(is.na(x), "", x)
  
  x |>
    str_replace_all("\\\\", "\\\\\\\\") |>
    str_replace_all(";", "\\\\;") |>
    str_replace_all(",", "\\\\,") |>
    str_replace_all("\r?\n", "\\\\n")
}


# =========================================================
# 22. CREATE UNIQUE EVENT IDS
# =========================================================

calendar_events <- calendar_events |>
  mutate(
    
    event_date_text = as.character(date),
    
    uid = paste0(
      str_replace_all(
        url,
        "[^A-Za-z0-9]",
        ""
      ),
      "-",
      str_replace_all(
        event_date_text,
        "[^0-9]",
        ""
      ),
      "@visitfrederick"
    )
  )


# =========================================================
# 23. CREATE ICALENDAR HEADER
# =========================================================

ics_lines <- c(
  "BEGIN:VCALENDAR",
  "VERSION:2.0",
  "PRODID:-//Visit Frederick Events//EN",
  "CALSCALE:GREGORIAN",
  "METHOD:PUBLISH",
  "X-WR-CALNAME:Visit Frederick Events"
)


# =========================================================
# 24. ADD EACH EVENT
# =========================================================

for (i in seq_len(nrow(calendar_events))) {
  
  event <- calendar_events[i, ]
  
  
  # -------------------------------------------------------
  # Event start date
  # -------------------------------------------------------
  
  start_date <- format(
    event$date,
    "%Y%m%d"
  )
  
  
  # -------------------------------------------------------
  # Event end date
  #
  # For a recurring event, the occurrence is one day.
  #
  # For a non-recurring multi-day event, use its actual
  # end date.
  # -------------------------------------------------------
  
  if (is.na(event$recurrence)) {
    
    end_date <- format(
      event$original_end_date + 1,
      "%Y%m%d"
    )
    
  } else {
    
    end_date <- format(
      event$date + 1,
      "%Y%m%d"
    )
    
  }
  
  
  # -------------------------------------------------------
  # Description
  # -------------------------------------------------------
  
  description <- escape_ics(
    event$description
  )
  
  
  # -------------------------------------------------------
  # Title
  # -------------------------------------------------------
  
  title <- escape_ics(
    event$title
  )
  
  
  # -------------------------------------------------------
  # URL
  # -------------------------------------------------------
  
  url <- escape_ics(
    event$url
  )
  
  
  # -------------------------------------------------------
  # Add event to ICS
  # -------------------------------------------------------
  
  ics_lines <- c(
    ics_lines,
    
    "BEGIN:VEVENT",
    
    paste0(
      "UID:",
      event$uid
    ),
    
    paste0(
      "DTSTAMP:",
      format(
        Sys.time(),
        "%Y%m%dT%H%M%SZ",
        tz = "UTC"
      )
    ),
    
    paste0(
      "DTSTART;VALUE=DATE:",
      start_date
    ),
    
    paste0(
      "DTEND;VALUE=DATE:",
      end_date
    ),
    
    paste0(
      "SUMMARY:",
      title
    ),
    
    paste0(
      "DESCRIPTION:",
      description
    ),
    
    paste0(
      "URL:",
      url
    ),
    
    "END:VEVENT"
  )
}


# =========================================================
# 25. CLOSE THE CALENDAR
# =========================================================

ics_lines <- c(
  ics_lines,
  "END:VCALENDAR"
)


# =========================================================
# 26. WRITE THE ICS FILE
# =========================================================

writeLines(
  ics_lines,
  "visit_frederick.ics"
)


# =========================================================
# 27. CONFIRM FILE CREATION
# =========================================================

cat("\n")
cat("ICS file created successfully!\n\n")

cat(
  "File location:\n",
  normalizePath("visit_frederick.ics"),
  "\n\n"
)

cat(
  "Number of calendar events:",
  nrow(calendar_events),
  "\n"
)

id="t4921"
readLines("visit_frederick.ics", n = 40)
