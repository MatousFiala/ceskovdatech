library(tidyverse)
library(jsonlite)

write_chart <- function(df, id, type = c("line", "bar", "map", "pyramid"), title, subtitle, aes, source, data_note, options = NULL, tabs = NULL) {
    #' Writes charts data and metadata in a csv and json file in chart_data folder, respectivelly
    #' df = long df with chart data 
    #' id = slug of chart. used for file names
    #' type = line, bar, map or pyramid (population pyramid: x = age, y = count, fill = the two sides,
    #'   the first value on the left, frame = the year shown; see README)
    #' title, subtitle = text; "[word::tooltip]" underlines the word and shows the tooltip on hover
    #' aes = list of aesthetic mappings, each a list of:
    #'   type (x, y, colour, fill, linetype, geometry, highlight, frame), field (df col), label,
    #'   labeller (df col shown to the reader instead of field), unit (shown on axis ticks, e.g. "mld. Kč"),
    #'   format (exclusive with labeller): "number" [2005 -> 2 005; default], "year" [2005 -> 2005],
    #'     "percent" [0.25 -> 25 %; for shares, unit defaults to "%"].
    #'   A line or bar chart whose x has format = "year" gets a year slider.
    #'   highlight: a TRUE/FALSE column; TRUE rows stand out (by colour, or by weight if colour is used). Needs no label, never shown
    #'   frame: a column whose values are shown one at a time, picked with a slider (pyramid)
    #'   show = FALSE: no legend (or axis title) for this aesthetic and no row in the data tooltip
    #'   tab = id of the tab this aesthetic belongs to (see tabs); without it the aesthetic is used in every tab
    #'   scheme, reverse, midpoint, fill_domain (map fill only): its own colour scale, overriding options (e.g. one per tab)
    #' tabs = optional list of tabs, each list(id, label): a switch in the chart that swaps the aesthetics
    #'   which have that `tab`, e.g. one y per tab. The first tab is shown first. NULL = no tabs.
    #'   All tabs share x (line, bar) or geometry (map), so that aesthetic cannot have a tab
    #' source = one source, or a list of several: list(name, dataset, url, retrieved).
    #'   Sources with the same name are shown together: "ČSÚ (1, 2), Maddison"
    #' data_note = text shown in a tooltip under the chart. For methodology comments and such
    #' options = optional named list of display options, see README (e.g. list(y_zero = FALSE, sort = "desc"))
    #'   options$annotations: reference lines on line and bar charts, each list(x = ...) or list(y = ...),
    #'   with label = "" (text along the line), dash = FALSE for a solid line (dashed by default) and
    #'   align = "top" | "centre" | "bottom" (where the label sits; default top); tab = "" shows it in that tab only
    #'   options$facet = TRUE: a toggle that splits the chart into one small chart per series (line: colour,
    #'   bar: fill), each panel with its own y axis; options$facet_scale = "shared" puts them on one axis
    # write_chart(
    #     df = ,
    #     id = "",
    #     type = "",
    #     title = "",
    #     subtitle = "",
    #     aes = list(list(type = "", field = "", label = "", format = ""),
    #             list(type = "", field = "", label = "", format = "", unit = "")),
    #     source = list(list(name = "", dataset = "", url = "", retrieved = format(Sys.Date()))),
    #     data_note = "",
    #     tabs = list(list(id = "", label = ""))   # optional, then aes get tab = ""
    # )
    type <- match.arg(type)
    if (!dir.exists("chart_data")) {
        dir.create("chart_data")
    }
    if (!is.null(source$name)) source <- list(source) # a single source
    if (!is.null(tabs$id)) tabs <- list(tabs) # a single tab
    annotations <- options$annotations
    if (!is.null(annotations$x) || !is.null(annotations$y)) options$annotations <- annotations <- list(annotations) # a single one
    aeses <- map_chr(aes, ~.x$type)
    # Aesthetic types present in each tab (its own + the shared ones); one "tab" when there are none
    tab_ids <- map_chr(tabs, ~as.character(.x$id %||% NA))
    aes_tabs <- map_chr(aes, ~as.character(.x$tab %||% NA))
    views <- map(if (length(tabs)) tab_ids else NA, ~aeses[is.na(aes_tabs) | aes_tabs %in% .x])
    required <- list(line = c("x", "y"), bar = c("x", "y"), map = c("geometry", "fill"), pyramid = c("x", "y", "fill", "frame"))[[type]]

    needed <- map_chr(aes, ~.x$field)
    formats <- unlist(map(aes, ~.x$format))
    stopifnot(
              "Config references missing columns" = all(needed %in% names(df)),
              "Format must be number, year or percent" = all(formats %in% c("number", "year", "percent")),
              "Source must be a list of lists, each with a name (check the brackets)" =
                  is.null(names(source)) && all(map_lgl(source, ~is.list(.x) && !is.null(.x$name))),
              "Tabs must be a list of lists, each with an id and a label (check the brackets)" =
                  is.null(names(tabs)) && all(map_lgl(tabs, ~is.list(.x) && !is.null(.x$id) && !is.null(.x$label))),
              "Tab ids must be unique" = !anyDuplicated(tab_ids),
              "An aesthetic has a tab that is not in tabs" = all(aes_tabs %in% c(tab_ids, NA)),
              "Line and bar require x and y aesthetic, map geometry and fill, pyramid x, y, fill and frame (in every tab)" =
                  all(map_lgl(views, ~all(required %in% .x))),
              "Tabs share the x aesthetic (line, bar) or the geometry aesthetic (map): it cannot have a tab" =
                  all(is.na(aes_tabs[aeses == required[1]])),
              "An aesthetic type is used twice (in the same tab)" = !any(map_lgl(views, ~anyDuplicated(.x) > 0)),
              "Annotations must be a list of lists, each with either x or y (line and bar charts only)" =
                  is.null(annotations) || (type %in% c("line", "bar") && is.null(names(annotations)) &&
                      all(map_lgl(annotations, ~is.list(.x) && xor(!is.null(.x$x), !is.null(.x$y))))),
              "Annotation align must be top, centre or bottom" =
                  all(map_lgl(annotations, ~is.null(.x$align) || .x$align %in% c("top", "centre", "center", "bottom"))),
              "An annotation has a tab that is not in tabs" =
                  all(map_lgl(annotations, ~is.null(.x$tab) || .x$tab %in% tab_ids)),
              "A pyramid's fill has exactly two values (left and right side)" =
                  type != "pyramid" || n_distinct(df[[keep(aes, ~.x$type == "fill")[[1]]$field]]) == 2
    )
    csv_name <- paste0(id, ".csv")
    write_csv(df, file.path("chart_data", csv_name), na = "")
    config <- list(
       id = id, 
       data = csv_name,
       type = type, 
       title = title, subtitle = subtitle, 
       tabs = tabs,
       aes = aes,
       source = source,
       data_note = data_note,
       options = options,
       last_updated = format(Sys.Date()),
       rows = nrow(df)
    )
    config <- Filter(Negate(is.null), config)
    write_json(config, 
       file.path("chart_data", paste0(id, ".json")),
       auto_unbox = T,
       pretty = T)
    invisible(config)
}
