library(shiny)
library(dplyr)

anime_df <- read.csv("anime_data.csv", stringsAsFactors = FALSE)
anime_df$score[is.na(anime_df$score)] <- 0
anime_df$year[is.na(anime_df$year)] <- 0
anime_df$display_id <- ifelse(is.na(anime_df$id) | anime_df$id == "", seq_len(nrow(anime_df)), anime_df$id)
if (!"image_url" %in% names(anime_df)) anime_df$image_url <- ""
if (!"synopsis" %in% names(anime_df)) anime_df$synopsis <- ""

TL <- list(
  en = list(
    title = "Anime Dashboard",
    search = "SEARCH", search_ph = "Title...",
    type = "TYPE", status = "STATUS", genre = "GENRE", year = "YEAR",
    anime_found = "anime found", no_results = "No results found.",
    episodes = "Episodes", score = "Score", studio = "Studio",
    duration = "Duration", source = "Source", rating = "Rating",
    synopsis = "Synopsis:", unknown = "Unknown", na = "N/A",
    page = "Page"
  ),
  ru = list(
    title = "\u0410\u043d\u0438\u043c\u0435 \u0414\u0430\u0448\u0431\u043e\u0440\u0434",
    search = "\u041f\u041e\u0418\u0421\u041a", search_ph = "\u041d\u0430\u0437\u0432\u0430\u043d\u0438\u0435...",
    type = "\u0422\u0418\u041f", status = "\u0421\u0422\u0410\u0422\u0423\u0421", genre = "\u0416\u0410\u041d\u0420", year = "\u0413\u041e\u0414",
    anime_found = "\u0430\u043d\u0438\u043c\u0435 \u043d\u0430\u0439\u0434\u0435\u043d\u043e", no_results = "\u041d\u0438\u0447\u0435\u0433\u043e \u043d\u0435 \u043d\u0430\u0439\u0434\u0435\u043d\u043e.",
    episodes = "\u042d\u043f\u0438\u0437\u043e\u0434\u044b", score = "\u0420\u0435\u0439\u0442\u0438\u043d\u0433", studio = "\u0421\u0442\u0443\u0434\u0438\u044f",
    duration = "\u0414\u043b\u0438\u0442\u0435\u043b\u044c\u043d\u043e\u0441\u0442\u044c", source = "\u0418\u0441\u0442\u043e\u0447\u043d\u0438\u043a", rating = "\u0420\u0435\u0439\u0442\u0438\u043d\u0433",
    synopsis = "\u0421\u044e\u0436\u0435\u0442:", unknown = "\u041d\u0435\u0438\u0437\u0432\u0435\u0441\u0442\u043d\u043e", na = "\u041d/\u0414",
    page = "\u0421\u0442\u0440\u0430\u043d\u0438\u0446\u0430"
  )
)
tl <- function(lang, key) TL[[lang]][[key]]

showAnimeModal <- function(r, lang) {
  img_html <- if (!is.null(r$image_url) && nchar(r$image_url) > 0) {
    tags$img(src = r$image_url, style = "width:100%; height:300px; object-fit:cover; border-radius:10px 10px 0 0;",
             onerror = "this.style.display='none';this.nextElementSibling.style.display='flex';")
  } else {
    div(style = "width:100%;height:300px;background:#1a1a2e;display:flex;align-items:center;justify-content:center;color:#333;font-size:60px;border-radius:10px 10px 0 0;", "?")
  }
  modal_fallback <- if (!is.null(r$image_url) && nchar(r$image_url) > 0) {
    div(style = "width:100%;height:300px;background:#1a1a2e;display:flex;align-items:center;justify-content:center;color:#333;font-size:60px;border-radius:10px 10px 0 0;display:none;", "?")
  } else { NULL }
  genre_html <- paste(sapply(unlist(strsplit(r$genres, ", ")), function(g) {
    paste0('<span style="background:rgba(233,69,96,0.2);color:#e94560;padding:4px 10px;border-radius:15px;font-size:12px;margin:2px;display:inline-block;">', trimws(g), '</span>')
  }), collapse = "")
  detail <- function(label, value) {
    div(style = "margin-bottom:8px;",
        tags$span(style = "color:#888;font-size:12px;", label), tags$br(),
        tags$span(style = "color:#e0e0e0;font-size:14px;font-weight:600;", value))
  }
  showModal(modalDialog(
    tags$head(tags$style(HTML(paste0(
      ".modal-content{background:#16213e;color:#e0e0e0;border-radius:15px;max-width:700px;overflow:hidden;}",
      ".modal-header{background:none;border:none;padding:0;}.modal-body{padding:20px 25px;}",
      ".modal-footer{border-top:1px solid #1a1a2e;padding:10px 25px;}",
      ".modal-close{position:absolute;top:10px;right:15px;background:rgba(0,0,0,.6);color:#fff;border:none;border-radius:50%;width:35px;height:35px;font-size:18px;cursor:pointer;z-index:10;}"
    )))),
    footer = NULL, size = "l", easyClose = TRUE,
    tags$button(class = "modal-close", `data-dismiss` = "modal", "\u2715"),
    img_html, modal_fallback,
    tags$h2(style = "color:#fff;margin:15px 0 5px 0;font-size:22px;", r$title),
    tags$p(style = "color:#888;font-size:14px;margin:0 0 12px 0;", r$title_japanese),
    if (nchar(genre_html) > 0) div(style = "margin-bottom:15px;", HTML(genre_html)),
    div(style = "display:grid;grid-template-columns:1fr 1fr;gap:12px;margin-bottom:15px;",
        detail(tl(lang, "type"), r$type), detail(tl(lang, "score"), paste(r$score, "/ 10")),
        detail(tl(lang, "episodes"), ifelse(r$episodes > 0, r$episodes, tl(lang, "unknown"))),
        detail(tl(lang, "status"), r$status), detail(tl(lang, "year"), ifelse(r$year > 0, r$year, tl(lang, "unknown"))),
        detail(tl(lang, "year"), ifelse(nchar(r$season) > 0, r$season, tl(lang, "na"))),
        detail(tl(lang, "source"), r$source), detail(tl(lang, "studio"), r$studios),
        detail(tl(lang, "duration"), r$duration),
        detail(tl(lang, "rating"), ifelse(nchar(r$rating) > 0, r$rating, tl(lang, "na")))
    ),
    if (!is.null(r$synopsis) && nchar(r$synopsis) > 0) {
      div(style = "margin-top:10px;padding:15px;background:#0f0f23;border-radius:10px;",
          tags$strong(style = "color:#e94560;", tl(lang, "synopsis")),
          tags$span(style = "color:#bbb;font-size:13px;line-height:1.6;", r$synopsis))
    }
  ))
}

js_i18n <- function() {
  tags$script(HTML('
    var translations = {
      en: {
        title: "Anime Dashboard", search: "SEARCH", search_ph: "Title...",
        type: "TYPE", status: "STATUS", genre: "GENRE", year: "YEAR",
        anime_found: "anime found", no_results: "No results found.",
        page: "Page"
      },
      ru: {
        title: "\\u0410\\u043d\\u0438\\u043c\\u0435 \\u0414\\u0430\\u0448\\u0431\\u043e\\u0440\\u0434",
        search: "\\u041f\\u041e\\u0418\\u0421\\u041a", search_ph: "\\u041d\\u0430\\u0437\\u0432\\u0430\\u043d\\u0438\\u0435...",
        type: "\\u0422\\u0418\\u041f", status: "\\u0421\\u0422\\u0410\\u0422\\u0423\\u0421", genre: "\\u0416\\u0410\\u041d\\u0420", year: "\\u0413\\u041e\\u0414",
        anime_found: "\\u0430\\u043d\\u0438\\u043c\\u0435 \\u043d\\u0430\\u0439\\u0434\\u0435\\u043d\\u043e", no_results: "\\u041d\\u0438\\u0447\\u0435\\u0433\\u043e \\u043d\\u0435 \\u043d\\u0430\\u0439\\u0434\\u0435\\u043d\\u043e.",
        page: "\\u0421\\u0442\\u0440\\u0430\\u043d\\u0438\\u0446\\u0430"
      }
    };
    var currentLang = "en";
    function switchLang(lang) {
      currentLang = lang;
      var t = translations[lang];
      document.getElementById("header_title_text").innerText = t.title;
      document.querySelector("[data-i18n=search]").innerText = t.search;
      document.querySelector("[data-i18n=status]").innerText = t.status;
      document.querySelector("[data-i18n=type]").innerText = t.type;
      document.querySelector("[data-i18n=genre]").innerText = t.genre;
      document.querySelector("[data-i18n=year]").innerText = t.year;
      document.getElementById("found_text").innerText = " " + t.anime_found;
      var ph = document.getElementById("search");
      if (ph) ph.placeholder = t.search_ph;
      document.getElementById("lang_en").className = lang === "en" ? "lang-btn active" : "lang-btn";
      document.getElementById("lang_ru").className = lang === "ru" ? "lang-btn active" : "lang-btn";
      Shiny.setInputValue("lang", lang, {priority: "event"});
    }
    Shiny.addCustomMessageHandler("updateUI", function(msg) {
      var t = translations[msg.lang];
      document.getElementById("header_title_text").innerText = t.title;
      document.querySelector("[data-i18n=search]").innerText = t.search;
      document.querySelector("[data-i18n=type]").innerText = t.type;
      document.querySelector("[data-i18n=status]").innerText = t.status;
      document.querySelector("[data-i18n=genre]").innerText = t.genre;
      document.querySelector("[data-i18n=year]").innerText = t.year;
      document.getElementById("anime_count_text").innerText = msg.count + " anime";
      document.getElementById("found_text").innerText = " " + t.anime_found;
      var ph = document.getElementById("search");
      if (ph) ph.placeholder = t.search_ph;
      document.getElementById("lang_en").className = msg.lang === "en" ? "lang-btn active" : "lang-btn";
      document.getElementById("lang_ru").className = msg.lang === "ru" ? "lang-btn active" : "lang-btn";
    });
  '))
}

ui <- fluidPage(
  tags$head(tags$style(HTML(paste0(
    "*{font-family:'Segoe UI',Arial,sans-serif;}",
    "body{background:#0f0f23;color:#e0e0e0;margin:0;}",
    ".header-bar{background:linear-gradient(135deg,#1a1a2e,#16213e);padding:15px 30px;display:flex;align-items:center;justify-content:space-between;box-shadow:0 2px 10px rgba(0,0,0,.3);}",
    ".header-bar h1{margin:0;color:#e94560;font-size:24px;}",
    ".lang-toggle{display:flex;border-radius:6px;overflow:hidden;border:1px solid #333;}",
    ".lang-btn{background:transparent;color:#888;border:none;padding:6px 14px;cursor:pointer;font-size:13px;font-weight:600;transition:all .2s;}",
    ".lang-btn.active{background:#e94560;color:#fff;}",
    ".lang-btn:hover:not(.active){background:#1a1a2e;color:#e0e0e0;}",
    ".filters{background:#16213e;padding:15px 30px;display:flex;gap:15px;flex-wrap:wrap;align-items:flex-end;border-bottom:1px solid #1a1a2e;}",
    ".filter-group{display:flex;flex-direction:column;gap:4px;}",
    ".filter-group label{font-size:12px;color:#888;text-transform:uppercase;}",
    ".filter-group select,.filter-group input[type=text]{background:#0f0f23;color:#e0e0e0;border:1px solid #333;padding:8px 12px;border-radius:6px;min-width:160px;font-size:14px;}",
    ".stats-bar{padding:10px 30px;background:#0a0a1a;display:flex;gap:20px;font-size:13px;color:#888;border-bottom:1px solid #1a1a2e;}",
    ".stats-bar span{color:#e94560;font-weight:bold;}",
    ".grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(180px,1fr));gap:15px;padding:20px 30px;}",
    ".card{background:#16213e;border-radius:10px;overflow:hidden;cursor:pointer;transition:transform .2s,box-shadow .2s;position:relative;}",
    ".card:hover{transform:translateY(-4px);box-shadow:0 8px 25px rgba(233,69,96,.3);}",
    ".card img{width:100%;height:250px;object-fit:cover;background:#1a1a2e;}",
    ".card-info{padding:10px;}",
    ".card-title{font-size:13px;font-weight:600;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;color:#fff;}",
    ".card-meta{font-size:11px;color:#888;margin-top:4px;}",
    ".card-score{position:absolute;top:8px;right:8px;background:rgba(0,0,0,.7);color:#ffd700;padding:3px 8px;border-radius:12px;font-size:12px;font-weight:bold;}",
    ".card-type{position:absolute;top:8px;left:8px;background:rgba(233,69,96,.8);color:#fff;padding:2px 8px;border-radius:12px;font-size:10px;}",
    ".no-img{width:100%;height:250px;background:linear-gradient(135deg,#1a1a2e,#16213e);display:flex;align-items:center;justify-content:center;color:#333;font-size:40px;}",
    ".pagination{display:flex;justify-content:center;gap:10px;padding:20px;}",
    ".pagination button{background:#16213e;color:#e0e0e0;border:1px solid #333;padding:8px 16px;border-radius:6px;cursor:pointer;font-size:14px;}",
    ".pagination button:hover{border-color:#e94560;color:#e94560;}",
    ".pagination button:disabled{opacity:.3;cursor:default;}",
    ".pagination .page-info{padding:8px;color:#888;font-size:14px;}",
    ".modal-backdrop{background:rgba(0,0,0,.85)!important;}"
  )))),
  js_i18n(),
  div(class = "header-bar",
      tags$h1(id = "header_title_text", "Anime Dashboard"),
      div(style = "display:flex;align-items:center;gap:15px;",
          tags$div(class = "lang-toggle",
                   tags$button(class = "lang-btn active", id = "lang_en", "EN",
                               onclick = "switchLang('en')"),
                   tags$button(class = "lang-btn", id = "lang_ru", "RU",
                               onclick = "switchLang('ru')")),
          tags$span(style = "color:#888;font-size:14px",
                    span(id = "anime_count_text"), span(id = "found_text", " anime found")))),
  div(class = "filters",
      div(class = "filter-group", tags$label(id = "lbl_search", `data-i18n` = "search", "SEARCH"),
          textInput("search", "", placeholder = "Title...", width = "220px")),
      div(class = "filter-group", tags$label(id = "lbl_type", `data-i18n` = "type", "TYPE"),
          selectInput("type_f", "", choices = c("All", "TV", "Movie", "OVA", "ONA", "Special", "Music"))),
      div(class = "filter-group", tags$label(id = "lbl_status", `data-i18n` = "status", "STATUS"),
          selectInput("status_f", "", choices = c("All", "Finished Airing", "Currently Airing", "Not yet aired"))),
      div(class = "filter-group", tags$label(id = "lbl_genre", `data-i18n` = "genre", "GENRE"),
          selectInput("genre_f", "", choices = c("All"))),
      div(class = "filter-group", tags$label(id = "lbl_year", `data-i18n` = "year", "YEAR"),
          selectInput("year_f", "", choices = c("All")))),
  uiOutput("anime_grid"),
  uiOutput("pagination_ui")
)

server <- function(input, output, session) {
  lang <- reactiveVal("en")
  observeEvent(input$lang, lang(input$lang))

  all_genres <- tryCatch(sort(unique(unlist(strsplit(anime_df$genres[anime_df$genres != ""], ", ")))), error = function(e) character(0))
  all_years <- sort(unique(anime_df$year[anime_df$year > 0]), decreasing = TRUE)
  updateSelectInput(session, "genre_f", choices = c("All", all_genres))
  updateSelectInput(session, "year_f", choices = c("All", as.character(all_years)))

  observe(session$onFlushed(function() {
    session$sendCustomMessage("updateCount", nrow(anime_df))
  }, once = TRUE))

  output$anime_count <- renderUI({ NULL })

  per_page <- reactiveVal(40)
  current_page <- reactiveVal(1)

  filtered <- reactive({
    df <- anime_df
    if (!is.null(input$search) && input$search != "") {
      df <- df[grepl(input$search, df$title, ignore.case = TRUE) | grepl(input$search, df$title_japanese, ignore.case = TRUE), ]
    }
    if (input$type_f != "All") df <- df[df$type == input$type_f, ]
    if (input$status_f != "All") df <- df[df$status == input$status_f, ]
    if (input$genre_f != "All") df <- df[grepl(input$genre_f, df$genres), ]
    if (input$year_f != "All") df <- df[df$year == as.integer(input$year_f), ]
    df[order(-df$score), ]
  })

  observeEvent(list(input$search, input$type_f, input$status_f, input$genre_f, input$year_f), current_page(1))

  observe({
    n <- nrow(filtered())
    l <- lang()
    session$sendCustomMessage("updateUI", list(count = nrow(anime_df), found = n, lang = l))
  })

  observeEvent(input$selected_anime, {
    sel_id <- as.character(input$selected_anime)
    r <- anime_df[as.character(anime_df$display_id) == sel_id, ]
    if (nrow(r) > 0) showAnimeModal(r[1, ], lang())
  })

  output$anime_grid <- renderUI({
    df <- filtered()
    pp <- per_page(); pg <- current_page()
    total <- nrow(df); start <- (pg - 1) * pp + 1; end <- min(pg * pp, total)
    if (start > total) return(div(style = "padding:40px;text-align:center;color:#888;", tl(lang(), "no_results")))
    page_df <- df[start:end, ]
    cards <- lapply(seq_len(nrow(page_df)), function(i) {
      r <- page_df[i, ]
      score_text <- ifelse(r$score > 0, sprintf("%.1f", r$score), "?")
      img_html <- if (!is.null(r$image_url) && nchar(r$image_url) > 0) {
        tags$img(src = r$image_url, alt = r$title, loading = "lazy", onerror = "this.style.display='none';this.nextElementSibling.style.display='flex';")
      } else { div(class = "no-img", "?") }
      fallback_img <- if (!is.null(r$image_url) && nchar(r$image_url) > 0) {
        div(class = "no-img", style = "display:none;", "?")
      } else { NULL }
      div(class = "card",
          onclick = paste0("Shiny.setInputValue('selected_anime', '", r$display_id, "', {priority: 'event'})"),
          div(class = "card-score", score_text), div(class = "card-type", r$type),
          img_html, fallback_img,
          div(class = "card-info", div(class = "card-title", title = r$title, r$title),
              div(class = "card-meta", paste0(r$type, " | ", ifelse(r$year > 0, r$year, "N/A")))))
    })
    div(class = "grid", cards)
  })

  output$pagination_ui <- renderUI({
    l <- lang()
    df <- filtered(); pp <- per_page(); pg <- current_page()
    total <- nrow(df); total_pages <- max(1, ceiling(total / pp))
    btns <- list()
    btns[[length(btns) + 1]] <- tags$button("\u00AB", onclick = paste0("Shiny.setInputValue('go_page', ", max(1, pg - 1), ")"), disabled = if (pg <= 1) "disabled" else NA)
    for (p in max(1, min(pg - 2, total_pages - 4)):min(total_pages, max(pg + 2, 5))) {
      btns[[length(btns) + 1]] <- tags$button(p, style = if (p == pg) "background:#e94560;color:#fff;border-color:#e94560;" else "", onclick = paste0("Shiny.setInputValue('go_page', ", p, ")"))
    }
    btns[[length(btns) + 1]] <- tags$button("\u00BB", onclick = paste0("Shiny.setInputValue('go_page', ", min(total_pages, pg + 1), ")"), disabled = if (pg >= total_pages) "disabled" else NA)
    page_text <- paste0(tl(l, "page"), " ", pg, " / ", total_pages)
    btns[[length(btns) + 1]] <- tags$span(class = "page-info", id = "page_info", page_text)
    div(class = "pagination", btns)
  })

  observeEvent(input$go_page, current_page(input$go_page))
}

shinyApp(ui = ui, server = server)
