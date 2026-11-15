# ============================================================
# Our Shoes: reusable Shiny tab (UI + server module)
# Use AFTER source("engine.R") in the main app.
# Requires: shiny, bslib, engine.R, pca.rds, shoes_meta.csv,
#           shoes/, our_shoes/, our_shoes_meta.csv
# ============================================================

our_shoes_categories <- c("Boots", "Sandals", "Shoes", "Slippers")

load_our_shoes <- function(csv_file = "our_shoes_meta.csv",
                           photo_dir = "our_shoes") {
  empty <- data.frame(filename = character(), member = character(),
                      category = character(), stringsAsFactors = FALSE)

  if (!file.exists(csv_file)) {
    warning("Our Shoes metadata was not found: ", csv_file)
    return(empty)
  }
  photos <- read.csv(csv_file, stringsAsFactors = FALSE,
                     fileEncoding = "UTF-8-BOM", check.names = FALSE)
  columns <- c("filename", "member", "category")
  if (!all(columns %in% names(photos))) {
    stop("our_shoes_meta.csv needs columns: filename, member, category")
  }
  photos <- photos[, columns, drop = FALSE]
  if (nrow(photos) == 0L) return(empty)

  for (column in columns) {
    photos[[column]] <- trimws(as.character(photos[[column]]))
  }
  if (anyNA(photos) || any(!nzchar(as.matrix(photos)))) {
    stop("Our Shoes metadata has an empty or missing value.")
  }
  # Do not let untrusted CSV rows access arbitrary files outside our_shoes/.
  if (any(photos$filename != basename(photos$filename)) ||
      any(!grepl("\\.(jpg|jpeg|png)$", photos$filename, ignore.case = TRUE))) {
    stop("Each filename must be a JPG/PNG basename, without any directory.")
  }
  if (anyDuplicated(photos$filename)) {
    stop("Each filename in our_shoes_meta.csv must be unique.")
  }
  if (any(!photos$category %in% our_shoes_categories)) {
    stop("Categories must be one of: Boots, Sandals, Shoes, Slippers.")
  }

  existing <- file.exists(file.path(photo_dir, photos$filename))
  if (any(!existing)) {
    warning("Missing Our Shoes image files: ",
            paste(photos$filename[!existing], collapse = ", "))
  }
  photos[existing, , drop = FALSE]
}

register_our_shoes_resources <- function(photo_dir = "our_shoes",
                                         catalogue_dir = "shoes") {
  if (dir.exists(photo_dir)) {
    shiny::addResourcePath("our-shoes-photos",
                           normalizePath(photo_dir, mustWork = TRUE))
  }
  if (dir.exists(catalogue_dir)) {
    shiny::addResourcePath("our-shoes-catalogue",
                           normalizePath(catalogue_dir, mustWork = TRUE))
  }
}

our_shoes_ui <- function(id, photos) {
  ns <- shiny::NS(id)
  choices <- stats::setNames(
    photos$filename,
    paste(photos$member, "·", photos$category, "·", photos$filename)
  )

  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .ourshoes-intro { margin: 6px 0 16px; }
      .ourshoes-intro p { color: var(--bs-secondary-color, #656b73); }
      .ourshoes-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(135px, 1fr)); gap: 12px; }
      .ourshoes-tile { border: 1px solid var(--bs-border-color, #ddd); padding: 10px; border-radius: 12px; }
      .ourshoes-tile img { display: block; width: 100%; height: 125px; object-fit: contain; border-radius: 8px; background: #f4f6f8; }
      .ourshoes-name { font-weight: 600; margin-top: 8px; overflow-wrap: anywhere; }
      .ourshoes-label { font-size: 13px; opacity: 0.75; overflow-wrap: anywhere; }
      .ourshoes-large-photo { width: 100%; max-height: 260px; object-fit: contain; background: #f4f6f8; border-radius: 10px; padding: 10px; }
      .ourshoes-result { padding: 12px; border-radius: 10px; margin: 10px 0; border: 1px solid; }
      .ourshoes-correct { color: #176b39; border-color: #8ec8a7; background: #eaf8ef; }
      .ourshoes-wrong { color: #9f2525; border-color: #e5aaaa; background: #fff0f0; }
      .ourshoes-caption { font-size: 13px; opacity: 0.75; margin-top: 6px; }
    ")),
    shiny::div(class = "ourshoes-intro",
      shiny::h3("Our Shoes"),
      shiny::p("Can PCA recognize real shoes photographed by our team? Choose a photo to see its closest catalogue matches.")),

    bslib::card(
      bslib::card_header("Our Shoe Collection"),
      shiny::uiOutput(ns("collection_summary")),
      shiny::uiOutput(ns("gallery"))
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header("Choose a Shoe"),
        shiny::selectInput(ns("selected_shoe"), "Member / category / photo",
                           choices = choices, width = "100%"),
        shiny::numericInput(ns("K"), "Number of PCs (K)",
                            value = 100, min = 1, max = 2000, step = 1),
        shiny::uiOutput(ns("selected_photo"))
      ),
      bslib::card(
        bslib::card_header("Prediction & Best Match"),
        shiny::uiOutput(ns("prediction")),
        shiny::uiOutput(ns("best_match"))
      )
    ),
    bslib::card(
      bslib::card_header("Top 5 Similar Catalogue Shoes"),
      shiny::uiOutput(ns("top5_photos")),
      shiny::tableOutput(ns("top5_table"))
    )
  )
}

our_shoes_server <- function(id, photos, photo_dir = "our_shoes") {
  shiny::moduleServer(id, function(input, output, session) {
    own_src <- function(filename) {
      paste0("our-shoes-photos/", utils::URLencode(filename, reserved = TRUE))
    }
    cat_src <- function(id) {
      paste0("our-shoes-catalogue/", sprintf("%04d.png", id))
    }

    output$collection_summary <- shiny::renderUI({
      if (nrow(photos) == 0L) {
        return(shiny::p("No team photos yet. Add images to our_shoes/ and rows to our_shoes_meta.csv, then restart the app."))
      }
      counts <- table(photos$member)
      message <- sprintf("%d photos from %d team members.",
                         nrow(photos), length(counts))
      if (any(counts != 3L)) {
        message <- paste(message,
                         "Each member should provide exactly 3 different shoes.")
      }
      shiny::p(message)
    })

    output$gallery <- shiny::renderUI({
      if (nrow(photos) == 0L) return(NULL)
      shiny::div(class = "ourshoes-grid",
        lapply(seq_len(nrow(photos)), function(i) {
          row <- photos[i, , drop = FALSE]
          shiny::div(class = "ourshoes-tile",
            shiny::tags$img(src = own_src(row$filename),
                            alt = paste("Photo of", row$category), loading = "lazy"),
            shiny::div(class = "ourshoes-name", row$member),
            shiny::div(class = "ourshoes-label", row$category),
            shiny::div(class = "ourshoes-label", row$filename)
          )
        })
      )
    })

    selected <- shiny::reactive({
      shiny::req(nrow(photos) > 0L, input$selected_shoe)
      i <- match(input$selected_shoe, photos$filename)
      shiny::validate(shiny::need(!is.na(i), "Please choose a valid photo."))
      photos[i, , drop = FALSE]
    })

    output$selected_photo <- shiny::renderUI({
      row <- selected()
      shiny::tagList(
        shiny::tags$img(class = "ourshoes-large-photo",
                        src = own_src(row$filename), alt = row$filename),
        shiny::p(class = "ourshoes-caption",
                 paste("Actual category:", row$category))
      )
    })

    # Calculated only when the selected photo changes, not when K changes.
    # photo_score() uses engine.R's grayscale/resize/padding and the existing pca.rds.
    query_scores <- shiny::reactive({
      row <- selected()
      path <- file.path(photo_dir, row$filename)
      shiny::validate(shiny::need(file.exists(path), "Photo file not found."))
      photo_score(path)
    })

    result <- shiny::reactive({
      shiny::req(input$K)
      K <- suppressWarnings(as.integer(input$K))
      shiny::validate(shiny::need(
        length(K) == 1L && !is.na(K) && K >= 1L &&
          K <= ncol(pca$x) && K <= length(query_scores()),
        "K must be between 1 and the number of available PCs."
      ))
      zq <- query_scores()

      if (K == 1L) {
        # Original engine.R drops matrix dimensions when K=1.
        # Same distance, ranking, and majority-vote logic as search_shoe().
        d <- abs(pca$x[, 1] - zq[1])
        top5 <- order(d)[1:5]
        categories <- meta$category[top5]
        list(top5 = top5, dist = d[top5], category = categories,
             pred = names(which.max(table(categories))))
      } else {
        search_shoe(zq, K = K, exclude_id = NULL)
      }
    })

    output$prediction <- shiny::renderUI({
      row <- selected()
      res <- result()
      correct <- identical(as.character(res$pred), as.character(row$category))
      shiny::div(class = paste("ourshoes-result",
                               if (correct) "ourshoes-correct" else "ourshoes-wrong"),
        shiny::tags$strong(if (correct) "CORRECT" else "INCORRECT"),
        shiny::div(paste("Predicted:", res$pred)),
        shiny::div(paste("Actual:", row$category))
      )
    })

    output$best_match <- shiny::renderUI({
      res <- result()
      id <- res$top5[1]
      shiny::tagList(
        shiny::h5("Closest catalogue photo"),
        shiny::tags$img(class = "ourshoes-large-photo", src = cat_src(id),
                        alt = paste("Catalogue shoe", id)),
        shiny::p(class = "ourshoes-caption",
                 sprintf("ID %04d · %s · distance %.2f",
                         id, res$category[1], res$dist[1]))
      )
    })

    output$top5_photos <- shiny::renderUI({
      res <- result()
      shiny::div(class = "ourshoes-grid",
        lapply(seq_along(res$top5), function(i) {
          id <- res$top5[i]
          shiny::div(class = "ourshoes-tile",
            shiny::tags$img(src = cat_src(id),
                            alt = paste("Catalogue shoe", id), loading = "lazy"),
            shiny::div(class = "ourshoes-name",
                       sprintf("#%d · %04d", i, id)),
            shiny::div(class = "ourshoes-label", res$category[i]),
            shiny::div(class = "ourshoes-label",
                       sprintf("Distance %.2f", res$dist[i]))
          )
        })
      )
    })

    output$top5_table <- shiny::renderTable({
      res <- result()
      data.frame(
        Rank = seq_along(res$top5),
        `Catalogue ID` = sprintf("%04d", res$top5),
        Category = res$category,
        Distance = round(res$dist, 2),
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, rownames = FALSE)
  })
}
