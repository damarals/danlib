nba_court_path <- function() {
  circle_points <- function(center = c(0, 0), radius = 1, npoints = 360) {
    angles <- seq(0, 2 * pi, length.out = npoints)
    return(data.frame(x = center[1] + radius * cos(angles),
                      y = center[2] + radius * sin(angles)))
  }
  width <- 50
  height <- 94 / 2
  key_height <- 19
  inner_key_width <- 12
  outer_key_width <- 16
  backboard_width <- 6
  backboard_offset <- 4
  neck_length <- 0.5
  hoop_radius <- 0.75
  hoop_center_y <- backboard_offset + neck_length + hoop_radius
  three_point_radius <- 23.75
  three_point_side_radius <- 22
  three_point_side_height <- 14

  court_points <- data.frame(x = c(width/2, width/2, -width/2, -width/2, width/2),
                             y = c(height, 0, 0, height, height), desc = "perimeter")

  court_points <- court_points %>%
    rbind(data.frame(x = c(outer_key_width/2, outer_key_width/2, -outer_key_width/2, -outer_key_width/2),
                     y = c(0, key_height, key_height, 0), desc = "outer_key"))

  court_points <- court_points %>%
    rbind(data.frame(x = c(-backboard_width/2, backboard_width/2),
                     y = c(backboard_offset, backboard_offset), desc = "backboard"))

  court_points <- court_points %>%
    rbind(data.frame(x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"))

  foul_circle <- circle_points(center = c(0, key_height), radius = inner_key_width/2)

  foul_circle_top <- dplyr::filter(foul_circle, y > key_height) %>%
    dplyr::mutate(desc = "foul_circle_top")

  foul_circle_bottom <- dplyr::filter(foul_circle, y < key_height) %>%
    dplyr::mutate(angle = atan((y - key_height)/x) * 180/pi,
                  angle_group = floor((angle - 5.625)/11.25),
                  desc = paste0("foul_circle_bottom_", angle_group)) %>%
    dplyr::filter(angle_group %% 2 == 0) %>%
    dplyr::select(x, y, desc)

  hoop <- circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    dplyr::mutate(desc = "hoop")

  restricted <- circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    dplyr::filter(y >= hoop_center_y) %>%
    dplyr::mutate(desc = "restricted")

  three_point_circle <- circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    dplyr::filter(y >= three_point_side_height, y >= hoop_center_y)

  three_point_line <- data.frame(x = c(three_point_side_radius, three_point_side_radius,
                                       three_point_circle$x, -three_point_side_radius,
                                       -three_point_side_radius),
                                 y = c(0, three_point_side_height, three_point_circle$y,
                                       three_point_side_height, 0), desc = "three_point_line")

  rbind(court_points, foul_circle_top, foul_circle_bottom, hoop, restricted, three_point_line)
}

geom_nba_court <- function(...) {
  ggplot2::geom_path(ggplot2::aes(x = x, y = y, group = desc),
                     data = nba_court_path(), ...)
}

theme_nba_court <- function() {
  list(
    ggplot2::coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)),
    ggplot2::theme_minimal(base_size = 22),
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = 'white', color = 'white'),
                   panel.background = ggplot2::element_rect(fill = "white", color = "white"),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   legend.margin = ggplot2::margin(-1, 0, 0, 0, unit = "lines"),
                   legend.position = "bottom",
                   legend.key = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = ggplot2::rel(1.0)))
  )
}

hex_bounds <- function(x, binwidth) {
  c(plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6)
}

calculate_hex_coords <- function(shots, binwidths) {
  xbnds <- hex_bounds(shots$loc_x, binwidths[1])
  xbins <- diff(xbnds)/binwidths[1]
  ybnds <- hex_bounds(shots$loc_y, binwidths[2])
  ybins <- diff(ybnds)/binwidths[2]

  hb <- hexbin::hexbin(x = shots$loc_x, y = shots$loc_y,
                       xbins = xbins, xbnds = xbnds, ybnds = ybnds,
                       shape = ybins/xbins, IDs = TRUE)

  shots$hexbin_id <- hb@cID

  hexbin_stats <- shots %>%
    dplyr::group_by(hexbin_id) %>%
    dplyr::summarise(hex_attempts = dplyr::n(),
                     hex_pct = mean(shot_made_numeric),
                     hex_points_scored = sum(shot_made_numeric * shot_value),
                     hex_points_per_shot = mean(shot_made_numeric * shot_value),
                     .groups = "drop_last")

  hexbin_ids_to_zones <- shots %>%
    dplyr::group_by(hexbin_id, shot_zone_range, shot_zone_area) %>%
    dplyr::summarise(attempts = dplyr::n(), .groups = "drop_last") %>%
    dplyr::ungroup() %>%
    dplyr::arrange(hexbin_id, desc(attempts)) %>%
    dplyr::group_by(hexbin_id) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::select(hexbin_id, shot_zone_range, shot_zone_area)

  hexbin_stats <- dplyr::inner_join(hexbin_stats, hexbin_ids_to_zones,
                                    by = "hexbin_id")

  # from hexbin package, see: https://github.com/edzer/hexbin
  sx <- hb@xbins/diff(hb@xbnds)
  sy <- (hb@xbins * hb@shape)/diff(hb@ybnds)
  dx <- 1/(2 * sx)
  dy <- 1/(2 * sqrt(3) * sy)
  origin_coords <- hexbin::hexcoords(dx, dy)

  hex_centers <- hexbin::hcell2xy(hb)

  hexbin_coords <- lapply(1:hb@ncells, function(i) {
    data.frame(x = origin_coords$x + hex_centers$x[i],
               y = origin_coords$y + hex_centers$y[i],
               center_x = hex_centers$x[i],
               center_y = hex_centers$y[i],
               hexbin_id = hb@cell[i])
  }) %>% Reduce(rbind, .)

  dplyr::inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")
}


shots2hexbins <- function(shots, league_averages, binwidths = c(1.5, 1.5),
                          min_radius_factor = .25, fg_diff_limits = c(-0.15, 0.15),
                          fg_pct_limits = c(0.2, 0.7), pps_limits = c(0.5, 1.5)) {
  if (nrow(shots) == 0) return(list())

  shots <- shots %>%
    dplyr::rename_all(tolower) %>%
    dplyr::mutate(loc_x = as.numeric(as.character(loc_x))/10,
                  loc_y = as.numeric(as.character(loc_y))/10 + 5.25,
                  shot_distance = as.numeric(as.character(shot_distance)),
                  shot_made_numeric = as.numeric(as.character(shot_made_flag)),
                  shot_made_flag = factor(shot_made_flag, levels = c("1", "0"),
                                          labels = c("made", "missed")),
                  shot_attempted_flag = as.numeric(as.character(shot_attempted_flag)),
                  shot_value = ifelse(tolower(shot_type) == "3pt field goal", 3, 2),
                  game_date = as.Date(game_date, format = "%Y%m%d"))

  league_averages <- league_averages %>%
    dplyr::rename_all(tolower) %>%
    dplyr::mutate(fga = as.numeric(as.character(fga)),
                  fgm = as.numeric(as.character(fgm)),
                  fg_pct = as.numeric(as.character(fg_pct)),
                  shot_value = ifelse(shot_zone_basic %in% c("Above the Break 3", "Backcourt",
                                                             "Left Corner 3", "Right Corner 3"), 3, 2))

  grouped_shots <- dplyr::group_by(shots, shot_zone_range, shot_zone_area)

  zone_stats <- grouped_shots %>%
    dplyr::summarise(zone_attempts = dplyr::n(),
                     zone_pct = mean(shot_made_numeric),
                     zone_points_scored = sum(shot_made_numeric * shot_value),
                     zone_points_per_shot = mean(shot_made_numeric * shot_value),
                     .groups = "drop_last")

  league_zone_stats <- league_averages %>%
    dplyr::group_by(shot_zone_range, shot_zone_area) %>%
    dplyr::summarise(league_pct = sum(fgm) / sum(fga),
                     .groups = "drop_last")

  hex_data <- calculate_hex_coords(shots, binwidths = binwidths)

  join_keys <- c("shot_zone_area", "shot_zone_range")

  hex_data <- hex_data %>%
    dplyr::inner_join(zone_stats, by = join_keys) %>%
    dplyr::inner_join(league_zone_stats, by = join_keys)

  max_hex_attempts <- max(hex_data$hex_attempts)

  hex_data <- dplyr::mutate(hex_data,
                            radius_factor = min_radius_factor + (1 - min_radius_factor) * log(hex_attempts + 1)/log(max_hex_attempts + 1),
                            adj_x = center_x + radius_factor * (x - center_x),
                            adj_y = center_y + radius_factor * (y - center_y),
                            bounded_fg_diff = pmin(pmax(zone_pct - league_pct, fg_diff_limits[1]), fg_diff_limits[2]),
                            bounded_fg_pct = pmin(pmax(zone_pct, fg_pct_limits[1]), fg_pct_limits[2]),
                            bounded_points_per_shot = pmin(pmax(zone_points_per_shot, pps_limits[1]), pps_limits[2]))

  list(hex_data = hex_data, fg_diff_limits = fg_diff_limits,
       fg_pct_limits = fg_pct_limits, pps_limits = pps_limits)

}
