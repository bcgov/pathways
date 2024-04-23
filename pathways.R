#### Determining Model Pathways ----
## Date:  March 2024
## Coder: Julie


#### load packages ----
library(tidyverse)
library(glue)


#### SET VALUES ----
##  Where is SEM analysis located? - this will likely NOT change
PATH_root <- "//SFP.IDIR.BCGOV/S152/S52010/WES Data & Frame/SEM Analysis/BC Public Service/"


## ** Which year and associated Amos Output are you working with?
YEAR      <- "2022"
FILE_sem  <- "/Final model/WES2022_FINAL.AmosOutput"    ## FILE_sem  <- "/Versions/WES2018_FINAL.AmosOutput"


## ** What are the shorthand names of the Management and the Roof drivers (as used in Amos)
dr_mgmt  <- c("Exec", "Sup")
dr_roof  <- c("Org_Sat_Latent", "Job_Sat_Latent", "Commit")


## ** Do you want to remove any negative estimates? If yes, set remove_neg_estimates as TRUE; else FALSE.
## (Because sometimes a path is significant, but negative. That is confusing to interpret and is often removed.)
remove_neg_estimates <- TRUE


## path strength minimums - these will likely NEVER change
path_vs  <- 0.300  ## Very Strong: 0.300 or greater
path_st  <- 0.200  ## Strong: 0.200 through 0.299
path_ms  <- 0.100  ## Moderately Strong: 0.100 through 0.199; Minimally Strong: 0.099 or less


#### load model ----
lines <- readLines(paste0(PATH_root, YEAR, FILE_sem))

temp <- tibble(orig = str_trim(lines)) %>%
  mutate(type = case_when(str_detect(orig, "nodecaption") ~ "caption",
                          str_detect(orig, "vname") ~ "variable",
                          TRUE ~ NA_character_),
         section = case_when(type == "caption" ~ 
                               str_sub(orig, start = str_locate(orig, 'nodecaption="')[,"end"]+1),
                             TRUE ~ NA_character_),
         section = str_sub(section, end = str_locate(section, '"')[,"start"]-1)) %>%
  fill(section)


#### pull connections ----
## connections are in standardized regression weights table
std_reg_weights <- temp %>%
  filter(str_starts(section, "Standardized Regression")) %>%
  ## get driver names and estimates
  mutate(path_end = case_when(str_detect(orig, 'nopad">') ~ 
                            str_sub(orig, start = str_locate(orig, 'nopad">')[,"end"]+1, end = -6)),
         path_start = case_when(str_detect(orig, '"B3">') ~
                            str_sub(orig, start = str_locate(orig, '"B3">')[,"end"]+1, end = -6)),
         estimate = case_when(str_detect(orig, '<td x="') ~ 
                         str_sub(orig, start = str_locate(orig, '<td x="')[,"end"]+1,
                                            end = str_locate(orig, '">')[, "start"]-1),
                         str_detect(orig, '<td nowrap="" x="-') ~ 
                           str_sub(orig, start = str_locate(orig, '<td nowrap="" x="')[,"end"]+1,
                                   end = str_locate(orig, '">')[, "start"]-1)),
         ## to pull lines together below
         tmp = ifelse(!is.na(path_end), row_number(), NA)) %>%
  fill(tmp) %>%
  filter(!is.na(path_end) | !is.na(path_start) | !is.na(estimate)) %>%
  select(-orig, -type, -section) %>%
  ## pull associated lines together
  pivot_longer(-tmp, names_to = "var", values_to = "val") %>%
  filter(!is.na(val)) %>%
  pivot_wider(names_from = "var", values_from = "val") %>%
  ## calculate path strengths
  mutate(estimate = as.numeric(estimate),
         type = ifelse(str_starts(path_end, "Q"), "question", "driver"),
         strength = ifelse(type == "driver",
                           cut(estimate, breaks = c(0, path_ms, path_st, path_vs, 1)),
                           NA),
         strength = factor(strength,
                           labels = c("Minimally Strong", "Moderately Strong", "Strong", "Very Strong")),
         connection = ifelse(type == "driver", paste0(path_start, "_to_", path_end), NA_character_))

## check for negative estimates
if(any(std_reg_weights$estimate < 0)) {
  stop("One or more estimates are negative. Check model. You likely want to drop this connection.")
}

if(remove_neg_estimates == TRUE) {
  ## REMOVE negative estimates
  std_reg_weights <- std_reg_weights %>% filter(estimate > 0)
  message("Note: Any negative estimates have been removed.")
}


#### path stats ----
## get driver order
tmp <- std_reg_weights %>%
  ## infinite loop if don't drop roof drivers
  filter(type == "driver", !path_start %in% dr_roof) %>%
  select(path_end, path_start)
dr_order <- vector(length = 0)
multis   <- vector(mode = "list", length = 0); m <- 1
while(dim(tmp)[1] > 0) {
  ## find driver(s) that are NOT path_end
  end <- setdiff(tmp$path_start, tmp$path_end)
  if(length(end) > 1) { multis[[m]] <- end; m <- m + 1 }
  ## add 'end' to dr_order vector
  dr_order <- c(dr_order, end)
  ## now remove 'end' from tmp
  tmp <- tmp %>% filter(!tmp$path_end %in% end, !tmp$path_start %in% end)
}; rm(tmp, end, m)
dr_order <- c(dr_order, dr_roof)
message(paste("*** Note:", length(multis), "sets of drivers are at the same level of path ends:"))
multis ## drivers at same level of path ends


## number of paths from and to each driver, and number of underlying questions
path_stats <- std_reg_weights %>%
  ## calc number of paths FROM each driver
  filter(!str_starts(path_end, "Q")) %>%
  group_by(driver = path_start) %>%
  summarize(paths_from = n()) %>%
  ## add number of paths TO each driver
  full_join(std_reg_weights %>%
              filter(!str_starts(path_end, "Q")) %>%
              group_by(driver = path_end) %>%
              summarize(paths_to = n()),
            by = "driver") %>%
  mutate(paths_to = ifelse(is.na(paths_to), 0, paths_to),
         ## calc number of connections to and from each driver
         n_connections = paths_from + paths_to) %>%
  ## add number of underlying questions in driver
  full_join(std_reg_weights %>%
              filter(str_starts(path_end, "Q")) %>%
              group_by(driver = path_start) %>%
              summarize(underlying_qs = n()),
            by = "driver") %>%
  mutate(block = case_when(driver %in% dr_roof ~ "Roof",
                           driver %in% dr_mgmt ~ "Foundation",
                           TRUE ~ "Building"), .before = 1) %>%
  ## add in driver order (from foundation through roof)
  full_join(tibble(driver = dr_order, order = 1:length(dr_order)), by = "driver") %>%
  arrange(order)
path_stats


#### create all paths ----
### * a. create 'base' to pull from
base <- std_reg_weights %>%
  filter(type == "driver") %>%
  # left_join(path_stats %>% select(path_end = driver, order), by = "path_end") %>%
  select(last = path_end, dr = path_start, weight = estimate) # order


### * b. run for Exec (z=1)
z <- 1
out <- base %>%
  ## start with Exec
  filter(dr == dr_mgmt[z]) %>%
  ## rename cols
  rename_with(~paste0(.x, z), -c("last")) %>%
  ## copy 'last' as next starter
  mutate(!!(paste0("dr", z+1)) := last)
## 'out' now has columns: last, dr1, weight1, dr2


### * c. run through remaining non-roof drivers (z=2, ...)
run_z <- 2:(length(dr_order[!dr_order %in% c("Exec", dr_roof)])+1)  ## start at 2 (b/c z=1 done), add 1 because start at 2
for(z in run_z) {
  num_z <- path_stats %>% filter(driver == dr_order[z]) %>% pull(paths_from) - 1
  rows_z <- which(out$last == dr_order[z])
  ## make tmp copy to work on dr
  tmp1 <- out %>% mutate(this_col = row_number())
  ## iterate through rows_z
  for(rz in seq_along(rows_z)) {
    ## filter for 'z'
    tmp2 <- tmp1 %>% filter(this_col %in% rows_z[rz])
    ## find number of driver in path
    dr_num <- tmp2 %>%
      ## get only driver columns
      select(starts_with("dr")) %>%
      ## drop cols long
      pivot_longer(everything(), names_to = "var", values_to = "val") %>%
      ## pull column name of driver we're working on
      filter(val == dr_order[z]) %>% pull(var) %>%
      ## get number in column name as number of driver in path
      str_replace("dr", "") %>% as.numeric()
    ## add empty rows
    for(nz in 1:num_z) {  tmp2 <- tmp2 %>% add_row()  }; rm(nz)
    ## fill empty rows
    tmp3 <- tmp2 %>% fill(everything())
    ## add new ends
    tmp4 <- tmp3 %>% select(-last) %>%
      ## drop columns that are fully NA
      select_if(~!any(is.na(.))) %>%
      bind_cols(base %>%
                  filter(dr == dr_order[z]) %>%
                  ## drop column of driver we're working on (ends up redundant when bind)
                  select(-dr) %>%
                  ## rename weight column with dr_num
                  rename_with(~paste0(.x, dr_num), -c("last")) %>%
                  ## add driver column of new end
                  mutate(!!(paste0("dr", dr_num+1)) := last))
    ## hold this rz's tmp4 for binding later
    assign(paste0("hold_rz_", rz), tmp4)
    rm(dr_num, tmp2, tmp3, tmp4)
  }
  ## add back non-z rows
  holds <- ls(pattern = "hold_rz_")
  tmp4 <- get(holds[1])
  if(length(holds) > 1) {
    for(h in 2:length(holds)) {  tmp4 <- bind_rows(tmp4, get(holds[h]))  }; rm(h, holds)
  }
  out <- tmp4 %>%
    bind_rows(tmp1 %>% filter(!this_col %in% rows_z)) %>%
    relocate(last, .before = 1) %>% select(-this_col)
  ## drop temporary files and variables
  rm(num_z, rows_z, tmp1, tmp4, rz, list = ls(pattern = "hold_rz_"))
  message(glue("*** completed paths for '{dr_order[z]}'"))
}; rm(z, run_z, base)


### * d. create full path variable and number of drivers
dr_cols <- out %>% select(starts_with("dr"), -dr1) %>% names()
out <- out %>% mutate(path = dr1)
for(i in seq_along(dr_cols)) {
  out <- out %>% mutate(path = paste(path, get(dr_cols[i]), sep = "/"))
}; rm(i, dr_cols)
out <- out %>%
  mutate(path = str_replace_all(path, "/NA", ""),
         num_drivers = str_count(path, "/") + 1)


### * e. calculate absolute path strengths
out <- out %>% 
  ## find last connection
  mutate(connection = case_when(last == dr_roof[1] ~ str_replace(path, glue("/{dr_roof[1]}"), glue("_to_{dr_roof[1]}")),
                                last == dr_roof[2] ~ str_replace(path, glue("/{dr_roof[2]}"), glue("_to_{dr_roof[2]}")),
                                last == dr_roof[3] ~ str_replace(path, glue("/{dr_roof[3]}"), glue("_to_{dr_roof[3]}")),
                                TRUE ~ path),
         connection = str_sub(connection, start = stringi::stri_locate_last(connection, regex = "/")[,"start"]+1)) %>%
  ## join in weight from last non-roof to roof driver
  left_join(std_reg_weights %>% filter(type == "driver") %>% select(connection, estimate), by = "connection") %>%
  ## multiply all weights together
  bind_cols(path_strength = apply(out %>% select(starts_with("weight")), 1, prod, na.rm = TRUE)) %>%
  ## divide out impact to roof (only way I could think of not multiplying in last connection's weight)
  mutate(path_strength = path_strength/estimate) %>%
  ## drop temporary variables needed to get path_strength
  select(-connection, -estimate)


### * f. remove duplicates
## I'm not sure why these are made; I *think* its because of parallel drivers.
out <- unique(out)


### * g. determine relative path strengths and rank
out <- out %>%
  ## calculate average and standard deviation of path strengths WITHIN num_driver groups
  group_by(num_drivers) %>%
  mutate(avg_path_strength = mean(path_strength, na.rm = TRUE),
         sd_path_strength  = sd(path_strength, na.rm = TRUE)) %>%
  ungroup() %>%
  ## determine whether path is stronger or weaker than average and one SD above average (for that number of drivers)
  mutate(#one_sd = avg_path_strength + sd_path_strength,
         rel_to_avg = case_when(is.na(path_strength) ~ NA_character_,
                                path_strength > avg_path_strength ~ "stronger",
                                path_strength <= avg_path_strength ~ "weaker"),
         rel_to_1sd_above_avg = case_when(is.na(path_strength) ~ NA_character_,
                                          path_strength > (avg_path_strength + sd_path_strength) ~ "stronger",
                                          path_strength <= (avg_path_strength + sd_path_strength) ~ "weaker"),
         ## rank paths, where 1 is strongest, and ties have same minimum rank
         path_rank = stringi::stri_rank((1-path_strength)))


### * h. format and save
pathways <- out %>%
  select(outcome = last, path, path_strength, rel_to_avg, rel_to_1sd_above_avg, path_rank, 
         num_drivers, everything(), -avg_path_strength, -sd_path_strength) %>%
  arrange(path_rank)


dim(pathways)[1]
pathways %>% group_by(outcome) %>% tally()


write_csv(pathways, paste0("pathways_", YEAR, ".csv"))

