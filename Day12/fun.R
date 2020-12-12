move_ship <- function(directions){
  coord <- c(0, 0)
  facing <- "E"
  for(i in 1:length(directions)){
    action <- substring(directions[i], 1, 1)
    steps <- substring(directions[i], 2, nchar(directions[i]))
    steps <- as.numeric(steps)
    if (action == "N"){
      coord <- coord + c(0, -steps)
    }
    if (action == "S"){
      coord <- coord + c(0, steps)
    }
    if (action == "E"){
      coord <- coord + c(steps, 0)
    }
    if (action == "W"){
      coord <- coord + c(-steps, 0)
    }
    if (action == "L"){
      facing <- turn_left(facing = facing, degrees = steps)
    }
    if (action == "R"){
      facing <- turn_right(facing = facing, degrees = steps)
    }
    if (action == "F"){
      coord <- case_when(
        facing == "N" ~ coord + c(0, -steps),
        facing == "S" ~ coord + c(0, steps),
        facing == "E" ~ coord + c(steps, 0),
        facing == "W" ~ coord + c(-steps, 0)
      )
    }
  }
  return(coord)
}

turn_left <- function(facing, degrees){
  if (facing == "N"){
    new_facing <- case_when(
      degrees == 90 ~ "W",
      degrees == 180 ~ "S",
      degrees == 270 ~ "E"
    )
  }
  if (facing == "S"){
    new_facing <- case_when(
      degrees == 90 ~ "E",
      degrees == 180 ~ "N",
      degrees == 270 ~ "W"
    )
  }
  if (facing == "E"){
    new_facing <- case_when(
      degrees == 90 ~ "N",
      degrees == 180 ~ "W",
      degrees == 270 ~ "S"
    )
  }
  if (facing == "W"){
    new_facing <- case_when(
      degrees == 90 ~ "S",
      degrees == 180 ~ "E",
      degrees == 270 ~ "N"
    )
  }
  return(new_facing)
}

turn_right <- function(facing, degrees){
  if (facing == "N"){
    new_facing <- case_when(
      degrees == 90 ~ "E",
      degrees == 180 ~ "S",
      degrees == 270 ~ "W"
    )
  }
  if (facing == "S"){
    new_facing <- case_when(
      degrees == 90 ~ "W",
      degrees == 180 ~ "N",
      degrees == 270 ~ "E"
    )
  }
  if (facing == "E"){
    new_facing <- case_when(
      degrees == 90 ~ "S",
      degrees == 180 ~ "W",
      degrees == 270 ~ "N"
    )
  }
  if (facing == "W"){
    new_facing <- case_when(
      degrees == 90 ~ "N",
      degrees == 180 ~ "E",
      degrees == 270 ~ "S"
    )
  }
  return(new_facing)
}

move_ship_with_waypoint <- function(directions, wp){
  coord <- c(0, 0)
  for(i in 1:length(directions)){
    action <- substring(directions[i], 1, 1)
    steps <- substring(directions[i], 2, nchar(directions[i]))
    steps <- as.numeric(steps)
    if (action == "N"){
      wp <- wp + c(0, -steps)
    }
    if (action == "S"){
      wp <- wp + c(0, steps)
    }
    if (action == "E"){
      wp <- wp + c(steps, 0)
    }
    if (action == "W"){
      wp <- wp + c(-steps, 0)
    }
    if (action == "L"){
      wp <- turn_wp_left(wp = wp, degrees = steps)
    }
    if (action == "R"){
      wp <- turn_wp_right(wp = wp, degrees = steps)
    }
    if (action == "F"){
      coord <- coord + steps * wp
    }
  }
  return(coord)
}

turn_wp_left <- function(wp, degrees){
  new_wp <- case_when(
    degrees == 90 ~ c(wp[2], -wp[1]),
    degrees == 180 ~ c(-wp[1], -wp[2]),
    degrees == 270 ~  c(-wp[2], wp[1]),
  )
  return(new_wp)
}

turn_wp_right <- function(wp, degrees){
  new_wp <- case_when(
    degrees == 90 ~ c(-wp[2], wp[1]),
    degrees == 180 ~ c(-wp[1], -wp[2]),
    degrees == 270 ~ c(wp[2], -wp[1])
  )
  return(new_wp)
}
