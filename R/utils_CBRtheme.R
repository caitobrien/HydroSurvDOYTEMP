#' CBRtheme
#'
#' @description A utils function setting adjusting the default shinydashboard theme
#'
#' @return The return value, will reflect the CBR website color theme
#'
#' @noRd

CBRtheme<- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#024c63",
    aqua = "#024c63",
    blue = "#024c63"
  ),
  fresh::adminlte_sidebar(
    width = "300px",
    dark_bg = "#F8F8F8",
    dark_color = "black",
    dark_hover_bg = "lightgrey", #none of the colors seem to do anything other than bg//switch to css option for more piecemeal customization
    dark_hover_color = "#024c63",
    dark_submenu_bg = "#F8F8F8",
    dark_submenu_color = "black",
    dark_submenu_hover_color = "lightgrey",
    light_submenu_color = "black",
    light_submenu_hover_color = "#024c63"
  ),
  fresh::adminlte_global(
    content_bg = "#FFF",
    box_bg = "#F8F8F8",
    info_box_bg = "#F8F8F8"
  )
)

