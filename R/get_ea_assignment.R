################################################################################
#
#' 
#' Get EA assignment data
#' 
#
################################################################################

get_ea_assignment_grand_bassa <- function(path) {
  team1 <- openxlsx::read.xlsx(
    xlsxFile = path,
    sheet = "Team 1"
  ) |>
    subset(select = c(CCNAME, DNAME, CLNAME, EFEACODE)) |>
    (\(x) data.frame(team = "Team 1", x))()
  
  team2 <- openxlsx::read.xlsx(
    xlsxFile = path,
    sheet = "Team 2"
  ) |>
    subset(select = c(CCNAME, DNAME, CLNAME, EFEACODE)) |>
    (\(x) data.frame(team = "Team 2", x))()
  
  team3 <- openxlsx::read.xlsx(
    xlsxFile = path,
    sheet = "Team 3"
  ) |>
    subset(select = c(CCNAME, DNAME, CLNAME, EFEACODE)) |>
    (\(x) data.frame(team = "Team 3", x))()
  
  team4 <- openxlsx::read.xlsx(
    xlsxFile = path,
    sheet = "Team 4"
  ) |>
    subset(select = c(CCNAME, DNAME, CLNAME, EFEACODE)) |>
    (\(x) data.frame(team = "Team 4", x))()
  
  rbind(team1, team2, team3, team4)
}


get_ea_assignment_urban_montserrado <- function(path) {
  team5 <- openxlsx::read.xlsx(
    xlsxFile = path,
    sheet = "Team 5"
  ) |>
    subset(select = c(CCNAME, DNAME, CLNAME, EFEACODE)) |>
    (\(x) data.frame(team = "Team 5", x))()
  
  team6 <- openxlsx::read.xlsx(
    xlsxFile = path,
    sheet = "Team 6"
  ) |>
    subset(select = c(CCNAME, DNAME, CLNAME, EFEACODE)) |>
    (\(x) data.frame(team = "Team 6", x))()
  
  rbind(team5, team6)
}
