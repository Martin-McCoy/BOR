#' Default Elevations for Mead/Powell for use in sliders
#' @export
mead_powell_slider_defaults <-
  structure(
    list(
      label = c("Powell Pool Elevation", "Mead Pool Elevation"),
      min = c(3400, 975),
      max = c(3600, 1100),
      value = c(3525,
                1020),
      step = c(5, 5)
    ),
    class = "data.frame",
    row.names = c(NA,
                  -2L)
  )

#' Groupings of Dams in the CRB
#' @export
dam_groupings <- list(
  CRSP = list(
    `Wayne N. Aspinall Unit` = paste(c("Blue Mesa", "Crystal", "Morrow Point"), "Dam"),
    `Flaming Gorge Unit` = "Flaming Gorge Dam",
    `Navajo Unit` = "Navajo Dam",
    `Glen Canyon Unit` = "Glen Canyon Dam"
  ),
  Hoover = "Hoover Dam",
  `Lower Colorado` = paste(c("Parker", "Davis"), "Dam")
)
