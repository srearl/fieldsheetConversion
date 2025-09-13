
utils::globalVariables(c(
  "start_lat_raw","start_long_raw","end_lat_raw","end_long_raw",
  "start_lat_dd","start_long_dd","end_lat_dd","end_long_dd",
  "net_number","net","net_time_raw","depth","depth_raw","depth_m",
  "angle","angle_deg","flow","flow_raw","flow_counts",
  "volume","volume_raw","volume_calc","mwo","mwo_raw","comments",
  "tow_file","cruise","tow_label","date","date_raw",
  "local_start","local_end","gmt_start","gmt_end",
  "wind_speed","wind_direction","sea_state","general_comments",
  ".data"  # when you reference .data inside mutate
))