#' @title Tax graph
#'
#' @description TODO
#'
#' @param demand_fun TODO
#' @param supply_fun TODO
#' @param supply_tax TODO
#' @param names TODO
#' @param title TODO
#' @param xlab TODO
#' @param ylab TODO
#' @param colors TODO
#' @param shaded TODO
#' @param xlim TODO
#' @param ylim TODO
#' @param max_x TODO
#'
#' @author
#' \itemize{
#' \item{Weiss, Andrew.}
#' }
#'
#' @examples
#'
#' # Data
#' demand <- function(Q) 20 - 0.5 * Q
#' demand_new <- function(Q) demand(Q) + 5
#' supply <- function(Q) 2 + 0.25 * Q
#' supply_new <- function(Q) supply(Q) + 5
#'
#' supply_tax <- function(Q) supply(Q) + 5
#'
#' demand_elastic <- function(Q) 10 - 0.05 * Q
#' demand_inelastic <- function(Q) 20 - 2 * Q
#'
#' supply_elastic <- function(Q) 2 + 0.05 * Q
#' supply_elastic_tax <- function(Q) supply_elastic(Q) + 5
#' supply_inelastic <- function(Q) 2 + 1.5 * Q
#' supply_inelastic_tax <- function(Q) supply_inelastic(Q) + 5
#'
#' normal_taxes <- tax_graph(demand, supply, supply_tax, NULL)
#' normal_taxes_shaded <- tax_graph(demand, supply, supply_tax, NULL, shaded = TRUE)
#' normal_taxes_shaded$p
#'
#' @import ggplot2 dplyr
#' @export
tax_graph <- function(demand_fun, supply_fun, supply_tax, names = c("Consumer surplus", "Producer surplus", "DWL", "Consumer tax burden", "Producer tax burden"),
                      title = NULL, xlab = "Product (Q)",
                      ylab = "Price (P)", colors, shaded = FALSE, xlim = c(0, 45),
                      ylim = c(0, 20), max_x = 45) {

  if(missing(colors)) {
    # Aurora and Frost color palettes from Nord
    # https://github.com/arcticicestudio/nord
    nord_red <- "#BF616A"  # nord11
    nord_orange <- "#D08770"  # nord12
    nord_yellow <- "#EBCB8B"  # nord13
    nord_green <- "#A3BE8C"  # nord14
    nord_purple <- "#B48EAD"  # nord15
    nord_lt_blue <- "#81A1C1"  # nord9
    nord_dk_blue <- "#5E81AC"  # nord10
  } else {

    if(length(colors) != 7 ) {

      warning("You must provide 7 colors. Default colors will be used instead")

      nord_red <- "#BF616A"  # nord11
      nord_orange <- "#D08770"  # nord12
      nord_yellow <- "#EBCB8B"  # nord13
      nord_green <- "#A3BE8C"  # nord14
      nord_purple <- "#B48EAD"  # nord15
      nord_lt_blue <- "#81A1C1"  # nord9
      nord_dk_blue <- "#5E81AC"  # nord10

    } else {

      nord_red <- colors[1]
      nord_orange <- colors[2]
      nord_yellow <- colors[3]
      nord_green <- colors[4]
      nord_purple <- colors[5]
      nord_lt_blue <- colors[6]
      nord_dk_blue <- colors[7]

    }

  }

  midpoint <- function(ymin, ymax) {
    ymax + (ymin - ymax) / 2
  }

  print_details <- function(coordinates, areas, areas_intermediate) {
    coordinates_r <- lapply(coordinates, round, digits = 2)
    areas_r <- lapply(areas, round, digits = 2)
    areas_intermediate_r <- lapply(areas_intermediate, round, digits = 2)

    glue::glue("
- Pre-tax quantity: **{coordinates_r$qstar_comp}**
- Pre-tax price: **\\${coordinates_r$pstar_comp}**
- Pre-tax consumer surplus: **\\${areas_r$con_surplus}** ($1/2 \\times {areas_intermediate_r$con_surplus_base} \\times {areas_intermediate_r$con_surplus_height}$)
- Pre-tax producer surplus: **\\${areas_r$pro_surplus}** ($1/2 \\times {areas_intermediate_r$pro_surplus_base} \\times {areas_intermediate_r$pro_surplus_height}$)
<!-- -->
- Post-tax quantity: **{coordinates_r$qstar_tax}**
- Post-tax price: **\\${coordinates_r$pstar_tax}**
- Post-tax consumer surplus: **\\${areas_r$con_surplus_tax}** ($1/2 \\times {areas_intermediate_r$con_surplus_tax_base} \\times {areas_intermediate_r$con_surplus_tax_height}$)
- Post-tax producer surplus: **\\${areas_r$pro_surplus_tax}** ($1/2 \\times {areas_intermediate_r$pro_surplus_tax_base} \\times {areas_intermediate_r$pro_surplus_tax_height}$)
<!-- -->
- Deadweight loss: **\\${areas_r$dwl}** ($1/2 \\times {areas_intermediate_r$dwl_base} \\times {areas_intermediate_r$dwl_height}$)
<!-- -->
- Total tax incidence (revenue raised): **\\${areas_r$total_incidence}** ($({coordinates_r$pstar_tax} - {coordinates_r$psupplied_tax}) \\times {coordinates_r$qstar_tax}$)
- Consumer tax incidence: **\\${areas_r$con_incidence}** ($({coordinates_r$pstar_tax} - {coordinates_r$pstar_comp}) \\times {coordinates_r$qstar_tax}$)
- Producer tax incidence: **\\${areas_r$pro_incidence}** ($({coordinates_r$pstar_comp} - {coordinates_r$psupplied_tax}) \\times {coordinates_r$qstar_tax}$)
- Percent of tax borne by consumers: **{scales::percent(areas$con_incidence_pct)}** (${areas_r$con_incidence} / {areas_r$total_incidence}$)
- Percent of tax borne by producers: **{scales::percent(areas$pro_incidence_pct)}** (${areas_r$pro_incidence} / {areas_r$total_incidence}$)
     ")
  }


  pts <- function(x) {
    as.numeric(grid::convertUnit(grid::unit(x, "pt"), "mm"))
  }

  # update_geom_defaults("text", list(family = "IBM Plex Sans Condensed SemiBold"))
  # update_geom_defaults("label", list(family = "IBM Plex Sans Condensed SemiBold"))

  theme_econ <- function(base_size = 11, axis_line = FALSE) {
    # update_geom_defaults("label", list(family = "IBM Plex Sans Condensed Light"))
    # update_geom_defaults("text", list(family = "IBM Plex Sans Condensed Light"))

    ret <- theme_bw(base_size) +
      theme(axis.title.y = element_text(margin = margin(r = 10)),
            axis.title.x = element_text(margin = margin(t = 10)),
            plot.title = element_text(size = rel(1.4), face = "plain"),
            plot.subtitle = element_text(size = rel(1), face = "plain"),
            plot.caption = element_text(size = rel(0.8), color = "grey50", face = "plain"),
            strip.text = element_text(size = rel(1), face = "plain"),
            legend.title = element_text(size = rel(0.8)),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            strip.background = element_rect(fill = "#ffffff", colour=NA),
            panel.spacing.y = unit(1.5, "lines"),
            legend.key = element_blank(),
            legend.spacing = unit(0.1, "lines"),
            legend.box.margin = margin(t = -0.25, unit = "lines"),
            legend.margin = margin(t = 0))

    if (axis_line) {
      ret <- ret + theme(axis.line = element_line(color = "black", size = 0.25))
    }

    ret
 }

  equilibrium <- uniroot(function(x) supply_fun(x) - demand_fun(x), c(0, max_x))$root
  equilibrium_tax <- uniroot(function(x) supply_tax(x) - demand_fun(x), c(0, max_x))$root

  x_q_tax <- seq(0, equilibrium_tax, 0.1)
  x_q_dwl <- seq(equilibrium_tax, equilibrium, 0.1)

  surplus_labels <- tribble(
    ~x, ~y, ~text, ~fill,
    1, midpoint(demand_fun(equilibrium_tax), max(demand_fun(x_q_tax))),
    names[1], nord_green,
    1, midpoint(min(supply_fun(x_q_tax)), supply_fun(equilibrium_tax)),
    names[2], nord_lt_blue,
    equilibrium_tax + 1, midpoint(min(supply_fun(x_q_dwl)), max(demand_fun(x_q_dwl))),
    names[3], nord_purple,
    1, midpoint(demand_fun(equilibrium), demand_fun(equilibrium_tax)),
    names[4], nord_yellow,
    1, midpoint(supply_fun(equilibrium), supply_fun(equilibrium_tax)),
    names[5], nord_yellow
  )


  if (shaded) {
    base_plot <- ggplot(data = tibble(x = 0:max_x), mapping = aes(x = x)) +
      geom_ribbon(data = tibble(x = x_q_tax),
                  aes(x = x,
                      ymin = demand_fun(equilibrium_tax), ymax = demand_fun(x_q_tax)),
                  alpha = 0.3, fill = nord_green) +
      geom_ribbon(data = tibble(x = x_q_tax),
                  aes(x = x,
                      ymin = supply_fun(x_q_tax), ymax = supply_fun(equilibrium_tax)),
                  alpha = 0.3, fill = nord_lt_blue) +
      geom_ribbon(data = tibble(x = x_q_dwl),
                  aes(x = x,
                      ymin = supply_fun(x_q_dwl), ymax = demand_fun(x_q_dwl)),
                  alpha = 0.3, fill = nord_purple) +
      geom_ribbon(data = tibble(x = x_q_tax),
                  aes(x = x,
                      ymin = demand_fun(equilibrium), ymax = demand_fun(equilibrium_tax)),
                  alpha = 0.3, fill = nord_yellow) +
      geom_ribbon(data = tibble(x = x_q_tax),
                  aes(x = x,
                      ymin = supply_fun(equilibrium), ymax = supply_fun(equilibrium_tax)),
                  alpha = 0.3, fill = nord_yellow)
  } else {
    base_plot <- ggplot(data = tibble(x = 0:max_x), mapping = aes(x = x))
  }

  full_plot <- base_plot +
    geom_segment(aes(x = equilibrium, xend = equilibrium,
                     y = -Inf, yend = supply_fun(equilibrium)),
                 color = "grey50", size = 0.5, linetype = "dashed") +
    geom_segment(aes(x = -Inf, xend = equilibrium,
                     y = supply_fun(equilibrium), yend = supply_fun(equilibrium)),
                 color = "grey50", size = 0.5, linetype = "dashed") +
    geom_segment(aes(x = equilibrium_tax, xend = equilibrium_tax,
                     y = -Inf, yend = supply_tax(equilibrium_tax)),
                 color = "grey50", size = 0.5, linetype = "dashed") +
    geom_segment(aes(x = -Inf, xend = equilibrium_tax,
                     y = supply_tax(equilibrium_tax), yend = supply_tax(equilibrium_tax)),
                 color = "grey50", size = 0.5, linetype = "dashed") +
    geom_segment(aes(x = -Inf, xend = equilibrium_tax,
                     y = supply_fun(equilibrium_tax), yend = supply_fun(equilibrium_tax)),
                 color = "grey50", size = 0.5, linetype = "dashed") +
    stat_function(fun = supply_fun, size = 1.5, color = nord_red) +
    stat_function(fun = supply_tax, size = 1.5, color = nord_orange) +
    stat_function(fun = demand_fun, size = 1.5, color = nord_dk_blue) +
    annotate(geom = "label", x = 38, y = supply_fun(38), label = "S",
             size = 4, fill = nord_red, color = "white") +
    annotate(geom = "label", x = 38, y = supply_tax(38), label = "S[tax]",
             size = 4, fill = nord_orange, color = "white", parse = TRUE) +
    annotate(geom = "label", x = 38, y = demand_fun(38), label = "D",
             size = 4, fill = nord_dk_blue, color = "white") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
    coord_cartesian(xlim, ylim) +
    labs(x = xlab, y = ylab, title = title) +
    theme_econ(13, axis_line = TRUE) +
    theme(panel.grid = element_blank())

  if (shaded) {
    final_plot <- full_plot +
      geom_label(data = surplus_labels, aes(x = x, y = y, label = text, fill = fill),
                 hjust = "left", size = 4, color = "white") +
      scale_fill_identity()
  } else {
    final_plot <- full_plot
  }

  coordinates <- list(qstar_comp = equilibrium,
                      pstar_comp = demand_fun(equilibrium),
                      qstar_tax = equilibrium_tax,
                      pstar_tax = demand_fun(equilibrium_tax),
                      psupplied_tax = supply_fun(equilibrium_tax))

  # Consumer surplus pre tax
  con_surplus_height <- demand_fun(0) - coordinates$pstar_comp
  con_surplus_base <- coordinates$qstar_comp
  con_surplus <- 0.5 * con_surplus_base * con_surplus_height

  # Consumer surplus post tax
  con_surplus_tax_height <- demand_fun(0) - coordinates$pstar_tax
  con_surplus_tax_base <- coordinates$qstar_tax
  con_surplus_tax <- 0.5 * con_surplus_tax_base * con_surplus_tax_height

  # Producer surplus pre tax
  pro_surplus_height <- coordinates$pstar_comp - supply_fun(0)
  pro_surplus_base <- coordinates$qstar_comp
  pro_surplus <- 0.5 * pro_surplus_base * pro_surplus_height

  # Producer surplus pre tax
  pro_surplus_tax_height <- coordinates$psupplied_tax - supply_fun(0)
  pro_surplus_tax_base <- coordinates$qstar_tax
  pro_surplus_tax <- 0.5 * pro_surplus_tax_base * pro_surplus_tax_height

  # DWL
  dwl_height <- coordinates$pstar_tax - coordinates$psupplied_tax
  dwl_base <- coordinates$qstar_comp - coordinates$qstar_tax
  dwl <- 0.5 * dwl_base * dwl_height

  # Tax incidence
  incidence_base <- coordinates$qstar_tax
  con_incidence_height <- coordinates$pstar_tax - coordinates$pstar_comp
  pro_incidence_height <- coordinates$pstar_comp - coordinates$psupplied_tax

  con_incidence <- incidence_base * con_incidence_height
  pro_incidence <- incidence_base * pro_incidence_height
  total_incidence <- con_incidence + pro_incidence
  con_incidence_pct <- con_incidence / total_incidence
  pro_incidence_pct <- pro_incidence / total_incidence

  areas <- list(con_surplus = con_surplus,
                con_surplus_tax = con_surplus_tax,
                pro_surplus = pro_surplus,
                pro_surplus_tax = pro_surplus_tax,
                dwl = dwl,
                con_incidence = con_incidence,
                pro_incidence = pro_incidence,
                total_incidence = total_incidence,
                con_incidence_pct = con_incidence_pct,
                pro_incidence_pct = pro_incidence_pct)

  areas_intermediate <- list(con_surplus_height = con_surplus_height,
                             con_surplus_base = con_surplus_base,
                             con_surplus_tax_height = con_surplus_tax_height,
                             con_surplus_tax_base = con_surplus_tax_base,
                             pro_surplus_height = pro_surplus_height,
                             pro_surplus_base = pro_surplus_base,
                             pro_surplus_tax_height = pro_surplus_tax_height,
                             pro_surplus_tax_base = pro_surplus_tax_base,
                             dwl_height = dwl_height,
                             dwl_base = dwl_base,
                             incidence_base = incidence_base,
                             con_incidence_height = con_incidence_height,
                             pro_incidence_height = pro_incidence_height)

  return(list(p = final_plot, coordinates = coordinates,
              areas = areas, areas_intermediate = areas_intermediate,
              details = print_details(coordinates, areas, areas_intermediate)))
}



