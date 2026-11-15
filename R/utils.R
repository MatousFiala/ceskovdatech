library(tidyverse)
library(jsonlite)

write_chart <- function(df, id, type = c("line", "bar", "map"), title, subtitle, aes, source, data_note, options = NULL) {
    #' Writes charts data and metadata in a csv and json file in chart_data folder, respectivelly
    #' df = long df with chart data 
    #' id = slug of chart. used for file names
    #' type = currently, either line, bar or map 
    #' title, subtitle = text; "[word::tooltip]" underlines the word and shows the tooltip on hover
    #' aes = list of aesthetic mappings, each a list of:
    #'   type (x, y, colour, fill, linetype, geometry, highlight), field (df col), label,
    #'   labeller (df col shown to the reader instead of field), unit (shown on axis ticks, e.g. "mld. Kč"),
    #'   format (exclusive with labeller): "number" [2005 -> 2 005; default], "year" [2005 -> 2005],
    #'     "percent" [0.25 -> 25 %; for shares, unit defaults to "%"].
    #'   A line or bar chart whose x has format = "year" gets a year slider.
    #'   highlight: a TRUE/FALSE column; TRUE rows stand out (by colour, or by weight if colour is used). Needs no label, never shown
    #'   show = FALSE: no legend (or axis title) for this aesthetic and no row in the data tooltip
    #' source = one source, or a list of several: list(name, dataset, url, retrieved).
    #'   Sources with the same name are shown together: "ČSÚ (1, 2), Maddison"
    #' data_note = text shown in a tooltip under the chart. For methodology comments and such
    #' options = optional named list of display options, see README (e.g. list(y_zero = FALSE, sort = "desc"))
    # write_chart(
    #     df = ,
    #     id = "",
    #     type = "",
    #     title = "",
    #     subtitle = "",
    #     aes = list(list(type = "", field = "", label = "", format = ""),
    #             list(type = "", field = "", label = "", format = "", unit = "")),
    #     source = list(list(name = "", dataset = "", url = "", retrieved = format(Sys.Date()))),
    #     data_note = ""
    # )
    type <- match.arg(type)
    if (!dir.exists("chart_data")) {
        dir.create("chart_data")
    }
    aeses <- map_chr(aes, ~.x$type)

    needed <- map_chr(aes, ~.x$field)
    formats <- unlist(map(aes, ~.x$format))
    if (!is.null(source$name)) source <- list(source) # a single source
    stopifnot(
              "Config references missing columns" = all(needed %in% names(df)),
              "Format must be number, year or percent" = all(formats %in% c("number", "year", "percent")),
              "Source must be a list of lists, each with a name (check the brackets)" =
                  is.null(names(source)) && all(map_lgl(source, ~is.list(.x) && !is.null(.x$name))),
              "Line requires x and y aesthetic" = type != "line" ||  all(c("x", "y") %in% aeses),
              "Bar requires x and y aesthetic" = type != "bar" ||  all(c("x", "y") %in% aeses),
              "Map requires geometry and fill aesthetic" = type != "map" ||  all(c("geometry", "fill") %in% aeses)
    )
    csv_name <- paste0(id, ".csv")
    write_csv(df, file.path("chart_data", csv_name), na = "")
    config <- list(
       id = id, 
       data = csv_name,
       type = type, 
       title = title, subtitle = subtitle, 
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
