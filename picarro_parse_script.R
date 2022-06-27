filter_data = function(raw_data){
  raw_data %>%
    mutate(date_time = ymd_hms(paste(DATE, TIME))) %>%
    filter(
      MPVPosition %% 1 == 0, solenoid_valves !=0
    ) %>%
    select(
      date_time,
      co2_12 = "12CO2_dry",
      co2_13 = "13CO2_dry",
      pressure = OutletValve,
      delta_raw = Delta_Raw,
      valve = MPVPosition
    ) %>%
    group_by(valve) %>%
    return()
  
}


filter_files = function(raw_data){
  raw_data %>%
    filter(
      MPVPosition != 16 & MPVPosition %% 1 == 0,
      solenoid_valves !=0
    ) %>%
    return()
  
}

n_interval = function(v_data){
  v_data %>%
    mutate(
      int_lag = difftime(date_time, lag(date_time), units = "secs"),
      int_lead = difftime(lead(date_time), date_time, units = "secs"),
      new_valve = case_when(
        int_lag > 10 ~ "start_time",
        is.na(int_lag) ~ "start_time",
        int_lead > 10 ~ "end_time",
        is.na(int_lead) ~ "end_time",
        T ~ "no"
      )
    ) %>%
    filter(
      new_valve != "no",
      int_lag<10 | int_lead < 10
    ) %>%
    select(date_time, new_valve) %>%
    mutate(
      n_sample = n()/2,
      n_sample = rep(1:n_sample, each = 2) - 1
    ) %>%
    select(n_sample, date_time, new_valve) %>%
    spread(key = new_valve, value = date_time) %>%
    return()
}

get_file_name <- function(f_data, e_time){
  f_data %>%
    filter(date_time == e_time) %>%
    pull(name) %>%
    .[[1]] %>%
    return()
}

mean_calc = function(data, e_time){
  
  avg_int = interval(e_time-seconds(30), e_time) 
  
  data %>%
    select(-name) %>%
    filter(date_time %within% avg_int) %>%
    select(-date_time) %>%
    summarise_all(mean) %>%
    return()
}


picarro_file_parser = function(data){
  co_data = data %>%
    mutate(raw = map(datapath, ~read_table(.x, col_names = T))) %>%
    mutate(temp = map(raw, filter_files)) %>%
    filter(map(temp, ~nrow(.x))>1) %>%
    mutate(filtered = map(raw, filter_data)) %>%
    select(name, filtered) %>%
    unnest(filtered) %>%
    arrange(date_time) %>%
    group_by(valve) %>%
    nest(data = -valve)
  
  int_data = co_data %>%
    mutate(valve_times = map(data, n_interval)) %>%
    select(valve, valve_times) %>%
    unnest(valve_times) %>%
    arrange(start_time) %>%
    rowid_to_column(var = "sample") %>%
    select(valve, sample, start_time, end_time)

  final_data = co_data %>%
    left_join(int_data) %>%
    mutate(
      v_range = interval(start_time, end_time),
      data = pmap(list(data, start_time, end_time), ~filter(..1, date_time >= ..2, date_time <= ..3)),
      quality =case_when(
        v_range < 90 ~ "short",
        v_range > 150 ~ "long",
        T ~ "Good"
      )
    ) %>%
    filter(!(valve == 16 & quality %in% c("short", "Good"))) %>%
    arrange(sample) %>%
    mutate(
    means = map2(data, end_time, mean_calc),
    file_name = map2(data, end_time, get_file_name),
    file_name = str_remove(file_name, pattern = "-DataLog_User.dat")
    ) %>%
    unnest(means) %>%
    select(
      file_name,
      valve,
      start_time,
      end_time,
      v_range,
      quality,
      pressure,
      co2_12,
      co2_13,
      delta_raw,
      data) %>%
    mutate(co2_total = co2_12 + co2_13) %>%
    ungroup(valve) %>%
    mutate_if(is.character, as.factor)
  
  temp = final_data %>%
    filter(valve == 16) %>%
    mutate(sample = "background")
  
  final_data %>%
    filter(valve != 16) %>%
    rowid_to_column(var = "sample") %>%
    mutate(sample = as.character(sample)) %>%
    bind_rows(temp) %>%
    return()
}



# 
# 
# x = tibble(
#   name = c("AtpE_time1-20190305-181624Z.dat", "AtpE_time2-20190305-231019Z-DataLog_User.dat"),
#   datapath = c(
#     "/home/tunasteak/box/projects/microbe_map/assays/data/Brett/03_2019_Picarro/AtpE_time6-20190306-173824Z-DataLog_User.dat",
#     "/home/tunasteak/box/projects/microbe_map/assays/data/Brett/03_2019_Picarro/AtpE_time4-20190306-050902Z-DataLog_User.dat"
#     
#   ))

