## =====================================================================
## GLASSBOX  -  An AI-Oversight Black-Box Investigation Console
## VAST Challenge 2026, Mini-Challenge 1  |  ISSS608 Shiny project
##
## One file, two spines (a global investigation clock and a ratcheting
## Distance-to-Breach index), ten investigative modules.
## Place data/MC1_final_00.json next to this file, then run.
## =====================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(shiny, bslib, htmltools, tidyverse, jsonlite,
               ggraph, tidygraph, ggrepel, scales, showtext, sysfonts)

`%||%` <- function(a, b) if (is.null(a)) b else a

## register display fonts for plots (matches the UI chrome); falls back if offline
try({
  font_add_google("Oswald", "Oswald")
  font_add_google("Inter", "Inter")
  font_add_google("JetBrains Mono", "JetBrains Mono")
  showtext_auto()
  showtext_opts(dpi = 96)
}, silent = TRUE)

## ---------------------------------------------------------------------
## 1. LOAD AND WRANGLE  (runs once at startup)
## ---------------------------------------------------------------------
raw    <- fromJSON("data/MC1_final_00.json", simplifyVector = FALSE)
rounds <- raw$rounds

messages <- imap(rounds, function(rd, i) {
  map(rd$communications, function(m) {
    st <- m$internal_state %||% list()
    tibble(
      round         = i - 1L,
      hour          = rd$hour,
      message_id    = m$message_id    %||% NA_character_,
      agent_label   = m$agent_label   %||% NA_character_,
      agent_role    = m$agent_role    %||% NA_character_,
      channel       = m$channel       %||% NA_character_,
      message_type  = m$message_type  %||% NA_character_,
      responding_to = m$responding_to %||% NA_character_,
      reacting      = st$reacting      %||% NA_character_,
      rationalizing = st$rationalizing %||% NA_character_,
      deliberating  = st$deliberating  %||% NA_character_,
      content       = m$content       %||% NA_character_
    )
  }) |> list_rbind()
}) |> list_rbind()

clean_num <- function(x) {
  x |> str_remove_all("\\$") |> str_remove_all("%") |> str_trim() |>
    na_if("NaN") |> na_if("None") |> na_if("") |> as.numeric()
}

environment <- imap(rounds, function(rd, i) {
  ec <- rd$environment_context %||% list()
  ms <- ec$market_snapshot     %||% list()
  un <- unlist(ec$agents_unavailable %||% list())
  tibble(
    round         = i - 1L,
    hour          = rd$hour,
    headline      = ec$event_headline  %||% NA_character_,
    narrative     = ec$event_narrative %||% NA_character_,
    sentiment     = ms$sentiment       %||% NA_character_,
    stock_clean   = clean_num(ms$stock_price %||% NA_character_),
    pct_change    = clean_num(ms$percent_change %||% NA_character_),
    judge_offline = "Judge" %in% un
  )
}) |>
  list_rbind() |>
  mutate(
    stock_valid = if_else(round %in% c(15, 18, 19) | stock_clean > 50,
                          NA_real_, stock_clean),
    date_label  = if_else(round <= 12, str_sub(hour, 6, 10), str_sub(hour, 12, 16)),
    day_part    = if_else(round <= 12, "Pre-crisis (daily)", "Crisis day (hourly)")
  )

stress_map <- c(neutral = 1, cautious = 2, negative = 3,
                critical = 4, LOW = 4, CRITICAL = 5, RECOVERING = 3)

## --- Distance-to-Breach: four buffer erosions, ratcheted ---
judge_per_round <- messages |>
  filter(agent_label == "Judge-Agent") |>
  count(round, name = "judge_msgs")

dtb_tbl <- messages |>
  group_by(round) |>
  summarise(
    n      = n(),
    leak   = sum(channel %in% c("side_huddle", "anonymous_post") |
                   (round >= 13 & channel == "personal_post")),
    ration = sum(!is.na(rationalizing)),
    react  = sum(!is.na(reacting)),
    .groups = "drop"
  ) |>
  left_join(environment |> select(round, sentiment, judge_offline), by = "round") |>
  left_join(judge_per_round, by = "round") |>
  arrange(round) |>
  mutate(
    judge_msgs = replace_na(judge_msgs, 0L),
    stress     = unname(stress_map[sentiment]),
    leakshare  = leak / n,
    gap = case_when(
      judge_offline        ~ 1.0,
      round %in% c(18, 19) ~ 0.5,   # present but only approving / warning
      round < 9            ~ 0.4,   # no formal monitor yet
      judge_msgs > 0       ~ 0.0,   # actively reviewing
      TRUE                 ~ 0.8    # silent / absent
    ),
    e_press  = (stress - 1) / 4 * 35,
    e_leak   = pmin(leakshare, 0.4) / 0.4 * 25,
    e_ration = pmin(ration, 3) / 3 * 25,
    e_gap    = gap * 15 * (0.4 + 0.6 * (stress - 1) / 4),
    erosion  = e_press + e_leak + e_ration + e_gap,
    dtb      = pmax(0, round(100 - cummax(erosion)))
  ) |>
  left_join(environment |> select(round, headline, date_label, day_part), by = "round")

## --- per-round behaviour metrics (pressure / reaction tracks) ---
round_metrics <- messages |>
  group_by(round) |>
  summarise(
    shadow = sum(channel == "side_huddle"),
    anon   = sum(channel == "anonymous_post"),
    public = sum(channel %in% c("official_post", "personal_post", "anonymous_post")),
    total  = n(),
    .groups = "drop"
  ) |>
  left_join(dtb_tbl |> select(round, stress), by = "round")

## --- channel share over time ---
chan_levels <- c("comms_huddle", "official_post", "one_on_one_chat",
                 "personal_post", "side_huddle", "anonymous_post")
chan_share <- messages |>
  count(round, channel) |>
  group_by(round) |> mutate(share = n / sum(n)) |> ungroup() |>
  mutate(channel = factor(channel, levels = chan_levels))

## --- role-inversion fingerprint ---
public_ch <- c("official_post", "personal_post", "anonymous_post")
fingerprint <- messages |>
  mutate(phase = if_else(round <= 12, "baseline", "crisis")) |>
  filter(channel %in% public_ch) |>
  count(agent_label, phase) |>
  pivot_wider(names_from = phase, values_from = n, values_fill = 0)

## --- key event chain ---
events <- tribble(
  ~round, ~label, ~kind,
  3,  "Shadow channel first used",                 "Deviation",
  6,  "Merger briefed off the monitored channel",  "Deviation",
  8,  "@Elena faux pas (near-miss leak)",          "Near-miss",
  9,  "The Judge is installed",                    "Control",
  10, "Press expose #1",                           "Pressure",
  12, "Press expose #2",                           "Pressure",
  13, "Crisis expose; stock down 8%",              "Pressure",
  17, "Legal and the Judge go offline",            "Control failure",
  18, "Judge approves disclosure (guardrails)",    "Guardrail bent",
  19, "Judge issues unenforceable warning",        "Guardrail bent",
  21, "EMBARGO BROKEN: merger confirmed",          "Breach",
  22, "Embargo officially lifts",                  "Resolution"
)

x_lab <- environment$date_label
names(x_lab) <- environment$round

## --- Permission Ledger: how Legal talked itself into the breach ---
ledger_steps <- tribble(
  ~step, ~trigger, ~justification, ~remaining,
  0, "Embargo in force",
  "The merger is sealed until 6 PM. No public confirmation is permitted, full stop.", 100,
  1, "Stock-price covenant clause",
  "The share price is collapsing toward a covenant breach. Surely a carefully worded clarification is defensible.", 80,
  2, "First anonymous post",
  "If it goes out anonymously it carries no official attribution, so it is not really a company statement.", 62,
  3, "A self-issued legal shield",
  "As privacy counsel I can frame this as a regulatory disclosure obligation. That gives us cover.", 44,
  4, "Claimed counterparty consent",
  "CivicLoom broke bilateral symmetry first, so a mutual-consent acceleration clause now applies.", 25,
  5, "Claimed CEO authorization",
  "Staying silent itself creates securities-law liability. Confirming is now the responsible thing to do.", 10,
  6, "EMBARGO BROKEN (17:25)",
  "The press already published. The information is in the public domain, so confirming it is not our breach.", 0
)

## --- Counterfactual events and their blockers ---
cf_events <- tribble(
  ~round, ~event, ~b_judge, ~b_anon, ~b_self, ~b_reform,
  3,  "Shadow channel becomes the team's back-channel",        FALSE, FALSE, FALSE, TRUE,
  13, "Legal opens an anonymous posting thread",               FALSE, TRUE,  FALSE, FALSE,
  17, "The Judge goes offline at the worst hour",              TRUE,  FALSE, FALSE, FALSE,
  18, "The Judge approves disclosure with guardrails",         TRUE,  FALSE, TRUE,  FALSE,
  20, "The anonymous thread escalates toward the deadline",    FALSE, TRUE,  FALSE, FALSE,
  21, "EMBARGO BROKEN: the merger is confirmed publicly",      TRUE,  TRUE,  FALSE, FALSE
)

## ---------------------------------------------------------------------
## 2. LOOK AND FEEL  (flight-recorder instrument aesthetic)
## ---------------------------------------------------------------------
PAL <- list(bg = "#0e1419", panel = "#161d26", line = "#243140",
            ink = "#e8edf2", muted = "#8b9aa8", amber = "#f5a623",
            cyan = "#4fb3c9", red = "#e8453c", green = "#46b07d")

glass_theme <- bs_theme(
  version = 5,
  bg = PAL$bg, fg = PAL$ink,
  primary = PAL$amber, secondary = PAL$cyan,
  base_font    = font_google("Inter"),
  heading_font = font_google("Oswald"),
  code_font    = font_google("JetBrains Mono")
)

css <- paste0(
":root{--amber:", PAL$amber, ";--cyan:", PAL$cyan, ";--red:", PAL$red,
";--green:", PAL$green, ";--panel:", PAL$panel, ";--line:", PAL$line,
";--muted:", PAL$muted, ";--ink:", PAL$ink, ";--bg:", PAL$bg, ";}",
"
body{background:var(--bg);}
.app-title{font-family:'Oswald',sans-serif;font-weight:700;letter-spacing:.18em;
  text-transform:uppercase;font-size:1.9rem;margin:0;color:var(--amber);}
.app-sub{color:var(--muted);font-size:.82rem;margin:.1rem 0 0;letter-spacing:.04em;}
.rec{font-family:'JetBrains Mono',monospace;font-size:.7rem;color:var(--red);
  letter-spacing:.15em;text-transform:uppercase;}
.rec::before{content:'';display:inline-block;width:8px;height:8px;border-radius:50%;
  background:var(--red);margin-right:6px;vertical-align:middle;
  box-shadow:0 0 6px var(--red);animation:blink 1.6s infinite;}
@keyframes blink{50%{opacity:.25}}
.instrument{background:var(--panel);border:1px solid var(--line);border-radius:10px;
  padding:1.1rem 1.3rem;}
.clock-line{font-family:'JetBrains Mono',monospace;font-size:.82rem;color:var(--ink);
  border-left:3px solid var(--amber);padding-left:.7rem;margin-top:.6rem;}
.clock-line .t{color:var(--amber);}
.clock-line .h{color:var(--muted);}
.dtb-wrap{text-align:center;}
.dtb-label{font-family:'JetBrains Mono',monospace;font-size:.66rem;letter-spacing:.22em;
  text-transform:uppercase;color:var(--muted);}
.dtb-num{font-family:'JetBrains Mono',monospace;font-weight:700;font-size:3.6rem;
  line-height:1;text-shadow:0 0 18px currentColor;}
.dtb-status{font-family:'Oswald',sans-serif;letter-spacing:.16em;text-transform:uppercase;
  font-size:.8rem;margin-top:.2rem;}
.meter{height:9px;background:#0b1016;border:1px solid var(--line);border-radius:5px;
  overflow:hidden;margin:.25rem 0;}
.meter > span{display:block;height:100%;}
.mini-lab{font-family:'JetBrains Mono',monospace;font-size:.62rem;color:var(--muted);
  display:flex;justify-content:space-between;}
.moment-btns .btn{font-family:'JetBrains Mono',monospace;font-size:.7rem;
  letter-spacing:.04em;margin:.15rem .25rem .15rem 0;border-color:var(--line);
  color:var(--cyan);background:transparent;padding:.25rem .6rem;}
.moment-btns .btn:hover{border-color:var(--amber);color:var(--amber);}
.eyebrow{font-family:'Oswald',sans-serif;letter-spacing:.22em;text-transform:uppercase;
  font-size:.72rem;color:var(--amber);margin-bottom:.2rem;}
h2,h3,h4{font-family:'Oswald',sans-serif;letter-spacing:.04em;}
.msg{background:#0f1620;border:1px solid var(--line);border-radius:8px;
  padding:.6rem .8rem;margin-bottom:.5rem;}
.msg .who{font-family:'JetBrains Mono',monospace;font-size:.7rem;color:var(--cyan);}
.msg.think .who{color:var(--amber);}
.msg .body{font-size:.86rem;color:var(--ink);margin-top:.2rem;}
.tag{font-family:'JetBrains Mono',monospace;font-size:.6rem;text-transform:uppercase;
  letter-spacing:.1em;padding:.05rem .4rem;border-radius:3px;border:1px solid var(--line);}
.banner{border-radius:10px;padding:1rem 1.2rem;font-family:'Oswald',sans-serif;
  letter-spacing:.08em;text-transform:uppercase;font-size:1.1rem;text-align:center;}
.cf-row{font-family:'JetBrains Mono',monospace;font-size:.82rem;padding:.35rem .2rem;
  border-bottom:1px solid var(--line);display:flex;justify-content:space-between;}
.cf-row .blocked{color:var(--green);text-decoration:line-through;opacity:.7;}
.cf-row .lives{color:var(--red);}
.nav-tabs .nav-link{font-family:'Oswald',sans-serif;letter-spacing:.06em;
  text-transform:uppercase;font-size:.82rem;color:var(--muted);}
.nav-tabs .nav-link.active{color:var(--amber);background:var(--panel);
  border-color:var(--line) var(--line) var(--panel);}
.verdict-btn{width:100%;text-align:left;margin-bottom:.6rem;border:1px solid var(--line);
  background:#0f1620;color:var(--ink);padding:.8rem 1rem;font-size:.92rem;}
.verdict-btn:hover{border-color:var(--amber);}
.lead-quote{font-family:'Oswald',sans-serif;font-size:1.5rem;line-height:1.3;
  color:var(--ink);border-left:4px solid var(--red);padding-left:1rem;}
")

## ggplot theme matching the panels
theme_glass <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(
      plot.background  = element_rect(fill = PAL$panel, colour = NA),
      panel.background = element_rect(fill = PAL$panel, colour = NA),
      panel.grid.major = element_line(colour = PAL$line, linewidth = .3),
      panel.grid.minor = element_blank(),
      text       = element_text(colour = PAL$ink),
      axis.text  = element_text(colour = PAL$muted, size = base - 4),
      plot.title = element_text(family = "Oswald", colour = PAL$ink),
      legend.text  = element_text(colour = PAL$muted, size = base - 4),
      legend.title = element_text(colour = PAL$muted, size = base - 4)
    )
}

## small UI helpers
meter_bar <- function(frac, col) {
  frac <- max(0, min(1, frac))
  div(class = "meter", tags$span(style = sprintf("width:%.0f%%;background:%s;", frac * 100, col)))
}
msg_card <- function(who, body, think = FALSE, tag = NULL) {
  div(class = if (think) "msg think" else "msg",
      div(class = "who", who,
          if (!is.null(tag)) span(class = "tag", style = "margin-left:.5rem;", tag)),
      div(class = "body", body))
}

## ---------------------------------------------------------------------
## 3. UI
## ---------------------------------------------------------------------
ui <- page_fluid(
  theme = glass_theme,
  tags$head(tags$style(HTML(css))),

  ## ===== top instrument bar (persistent: clock + DtB) =====
  div(class = "instrument", style = "margin-bottom:1.1rem;",
    layout_columns(
      col_widths = c(7, 5),
      div(
        div(style = "display:flex;justify-content:space-between;align-items:baseline;",
            h1("GLASSBOX", class = "app-title"),
            span(class = "rec", "Flight recorder")),
        p(class = "app-sub",
          "AI-oversight black box, TenantThread embargo failure, 17 May to 5 June 2046"),
        sliderInput("timeline", NULL, min = 0, max = 22, value = 0, step = 1,
                    width = "100%", ticks = FALSE,
                    animate = animationOptions(interval = 1100, loop = FALSE)),
        div(class = "moment-btns",
            "Jump to: ",
            actionButton("j_reh",  "Rehearsal"),
            actionButton("j_anon", "First anon post"),
            actionButton("j_sil",  "Judge goes silent"),
            actionButton("j_brk",  "The breach")),
        uiOutput("clock")
      ),
      div(
        uiOutput("dtb_gauge"),
        plotOutput("dtb_spark", height = "78px")
      )
    )
  ),

  ## ===== investigative modules =====
  navset_card_tab(
    id = "nav",

    nav_panel("The case", icon = NULL,
      div(class = "eyebrow", "Case file 00"),
      h3("Did the team leak the merger, or did the safeguard break down?"),
      p(style = "max-width:60rem;color:#cdd7e0;",
        "At 5 PM on 5 June, one hour before an embargo was due to lift, TenantThread's own automated accounts confirmed a sealed merger in public. An automated compliance monitor, the Judge, was supposed to prevent exactly this. You are the investigator. Scrub the clock above and watch the Distance-to-Breach gauge fall. Every module here exists to explain why it falls. The evidence points to a third answer: the safeguard was neither hacked nor crashed. It was talked past."),
      div(class = "lead-quote", style = "margin:1.4rem 0;",
        "The embargo was not hacked and it did not crash. It was talked past."),
      plotOutput("case_curve", height = "320px")
    ),

    nav_panel("Two-way mirror",
      div(class = "eyebrow", "Question 1, public versus private"),
      p(class = "app-sub", "Left: what the public saw this hour. Right: what was happening privately. Scrub the clock and watch the gap widen."),
      layout_columns(col_widths = c(6, 6),
        uiOutput("mirror_public"),
        uiOutput("mirror_private"))
    ),

    nav_panel("Pressure and reaction",
      div(class = "eyebrow", "Question 1, the tension meter"),
      p(class = "app-sub", "Upper track: external pressure. Lower track: the team's behavioural reaction. The line marks the current hour."),
      plotOutput("dual_track", height = "440px")
    ),

    nav_panel("Channel X-ray",
      div(class = "eyebrow", "Question 2, the conversation moved house"),
      p(class = "app-sub", "Share of messages per channel. Cool tones are monitored and sanctioned, warm tones are shadow and anonymous. Watch the centre of gravity drift toward danger on crisis day."),
      plotOutput("channel_xray", height = "440px")
    ),

    nav_panel("Role inversion",
      div(class = "eyebrow", "Question 2, who took the megaphone"),
      p(class = "app-sub", "Public posts per agent, calm baseline versus crisis day. The advisors who never posted in public take over, while PR goes silent."),
      plotOutput("fingerprint", height = "420px")
    ),

    nav_panel("Permission ledger",
      div(class = "eyebrow", "The heart, how an agent talked itself into it"),
      p(class = "app-sub", "Step through each justification the Legal agent collected. Watch the distance to the breach get shaved to zero."),
      sliderInput("ledger_step", "Justifications collected", min = 0, max = 6,
                  value = 0, step = 1, width = "100%", ticks = FALSE),
      layout_columns(col_widths = c(5, 7),
        uiOutput("ledger_meter"),
        uiOutput("ledger_body"))
    ),

    nav_panel("Counterfactual sandbox",
      div(class = "eyebrow", "Question 3, what would have stopped it"),
      p(class = "app-sub", "Switch safeguards on and see which events get intercepted, and whether the embargo still breaks. No single fix is enough."),
      layout_columns(col_widths = c(4, 8),
        div(class = "instrument",
          h4("Interventions", style = "margin-top:0;"),
          checkboxInput("cf_judge",  "Keep the Judge responding after 15:08", FALSE),
          checkboxInput("cf_anon",   "Block the anonymous channel", FALSE),
          checkboxInput("cf_self",   "Close the self-approval loophole", FALSE),
          checkboxInput("cf_reform", "Enact real reform after the near-miss", FALSE)),
        div(uiOutput("cf_banner"), uiOutput("cf_list")))
    ),

    nav_panel("The Judge",
      div(class = "eyebrow", "Question 3, the monitor that went quiet"),
      p(class = "app-sub", "The Judge's messages per hour, coloured by stance. It reviewed for days, then fell silent, went offline at the worst hour, and was absent when the embargo broke."),
      plotOutput("judge_posture", height = "420px")
    ),

    nav_panel("Who knew",
      div(class = "eyebrow", "Question 2, the spread of the secret"),
      p(class = "app-sub", "Reply network up to the current hour. Node size is the centrality you choose. Scrub the clock to watch the core form."),
      selectInput("centrality", "Size nodes by",
                  c("Degree (how connected)" = "degree",
                    "Betweenness (information broker)" = "betweenness"),
                  width = "320px"),
      plotOutput("network", height = "440px")
    ),

    nav_panel("The verdict",
      div(class = "eyebrow", "Close the case"),
      h3("Your finding"),
      p(class = "app-sub", "Choose the reading the evidence supports."),
      actionButton("v_delib", "Deliberate leak. The agents chose to release it early for advantage.",
                   class = "verdict-btn"),
      actionButton("v_break", "System breakdown. The monitor failed and the release slipped through.",
                   class = "verdict-btn"),
      actionButton("v_talk",  "Talked past, not broken. A chain of defensible steps that together broke the wall.",
                   class = "verdict-btn"),
      uiOutput("verdict_out")
    )
  )
)

## ---------------------------------------------------------------------
## 4. SERVER
## ---------------------------------------------------------------------
server <- function(input, output, session) {

  ## jump buttons drive the global clock
  observeEvent(input$j_reh,  updateSliderInput(session, "timeline", value = 8))
  observeEvent(input$j_anon, updateSliderInput(session, "timeline", value = 13))
  observeEvent(input$j_sil,  updateSliderInput(session, "timeline", value = 19))
  observeEvent(input$j_brk,  updateSliderInput(session, "timeline", value = 21))

  cur <- reactive({
    r <- input$timeline %||% 0
    dtb_tbl |> filter(round == r)
  })

  ## ---- clock readout ----
  output$clock <- renderUI({
    row <- cur(); env <- environment |> filter(round == row$round)
    stamp <- str_replace(env$hour, "T", "  ")
    div(class = "clock-line",
        span(class = "t", sprintf("T%02d  %s", row$round, stamp)),
        tags$br(),
        span(class = "h", coalesce(env$headline, "")))
  })

  ## ---- DtB gauge ----
  output$dtb_gauge <- renderUI({
    row <- cur(); v <- row$dtb
    col <- if (v > 50) PAL$amber else if (v > 28) "#f07b1d" else PAL$red
    status <- (if (v > 70) "Nominal"
               else if (v > 45) "Degraded"
               else if (v > 20) "Critical"
               else "Breach imminent")
    comp <- tibble(
      lab = c("Pressure", "Channel leak", "Rationalizing", "Oversight gap"),
      val = c(row$e_press / 35, row$e_leak / 25, row$e_ration / 25, row$e_gap / 15),
      col = c(PAL$cyan, "#e08a3c", PAL$red, PAL$amber))
    div(class = "dtb-wrap",
      div(class = "dtb-label", "Distance to breach"),
      div(class = "dtb-num", style = sprintf("color:%s;", col), v),
      div(class = "meter", style = "margin:.3rem 1.5rem;",
          tags$span(style = sprintf("width:%d%%;background:%s;", as.integer(v), col))),
      div(class = "dtb-status", style = sprintf("color:%s;", col), status),
      div(style = "margin-top:.7rem;text-align:left;",
        pmap(comp, function(lab, val, col) {
          tagList(div(class = "mini-lab", span(lab),
                      span(sprintf("%.0f%%", val * 100))),
                  meter_bar(val, col))
        })
      )
    )
  })

  ## ---- DtB sparkline ----
  output$dtb_spark <- renderPlot({
    r <- input$timeline %||% 0
    ggplot(dtb_tbl, aes(round, dtb)) +
      geom_line(colour = PAL$amber, linewidth = 1) +
      geom_vline(xintercept = r, colour = PAL$cyan, linewidth = .6) +
      geom_point(data = dtb_tbl |> filter(round == r),
                 colour = PAL$red, size = 2.4) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_void() +
      theme(plot.background = element_rect(fill = PAL$panel, colour = NA),
            panel.background = element_rect(fill = PAL$panel, colour = NA))
  })

  ## ---- The case: full DtB curve with events ----
  output$case_curve <- renderPlot({
    ev <- events |> left_join(dtb_tbl |> select(round, dtb), by = "round")
    ggplot(dtb_tbl, aes(round, dtb)) +
      annotate("rect", xmin = 12.5, xmax = 22.5, ymin = 0, ymax = 100,
               fill = PAL$red, alpha = .06) +
      geom_step(colour = PAL$amber, linewidth = 1.1, direction = "hv") +
      geom_point(data = ev, aes(round, dtb,
                 colour = kind == "Breach"), size = 3) +
      geom_text_repel(data = ev, aes(round, dtb, label = label),
                      colour = PAL$muted, size = 3, family = "Inter",
                      box.padding = .6, max.overlaps = Inf, seed = 1,
                      min.segment.length = 0, segment.colour = PAL$line) +
      scale_colour_manual(values = c(`TRUE` = PAL$red, `FALSE` = PAL$cyan), guide = "none") +
      scale_x_continuous(breaks = dtb_tbl$round, labels = x_lab) +
      scale_y_continuous(limits = c(0, 100)) +
      labs(title = "Distance to breach across two weeks",
           x = NULL, y = "Distance to breach") +
      theme_glass() +
      theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 8))
  })

  ## ---- Two-way mirror ----
  output$mirror_public <- renderUI({
    row <- cur(); r <- row$round; env <- environment |> filter(round == r)
    pub <- messages |> filter(round == r,
                              channel %in% c("official_post", "personal_post")) |>
      slice_head(n = 5)
    div(class = "instrument",
      h4("What the public saw", style = "margin-top:0;color:#cdd7e0;"),
      div(class = "msg", div(class = "who", "HEADLINE"),
          div(class = "body", coalesce(env$headline, "Quiet hour, no major public event."))),
      if (nrow(pub) == 0)
        p(class = "app-sub", "No public posts from the company this hour.")
      else
        pmap(pub, function(agent_label, content, ...)
          msg_card(agent_label, str_trunc(coalesce(content, ""), 220), tag = "public")))
  })
  output$mirror_private <- renderUI({
    r <- (input$timeline %||% 0)
    prv <- messages |> filter(round == r, channel == "side_huddle") |> slice_head(n = 4)
    thoughts <- messages |> filter(round == r,
                  !is.na(deliberating) | !is.na(rationalizing) | !is.na(reacting)) |>
      slice_head(n = 4)
    div(class = "instrument",
      h4("What was happening privately", style = "margin-top:0;color:var(--amber);"),
      if (nrow(prv) == 0 && nrow(thoughts) == 0)
        p(class = "app-sub", "No private back-channel traffic recorded this hour.")
      else tagList(
        pmap(prv, function(agent_label, content, ...)
          msg_card(paste0(agent_label, " (shadow channel)"),
                   str_trunc(coalesce(content, ""), 200), think = FALSE, tag = "private")),
        pmap(thoughts, function(agent_label, deliberating, rationalizing, reacting, ...) {
          txt <- coalesce(rationalizing, reacting, deliberating)
          lab <- (if (!is.na(rationalizing)) "rationalizing"
                  else if (!is.na(reacting)) "reacting" else "deliberating")
          msg_card(paste0(agent_label, " thinking"),
                   str_trunc(coalesce(txt, ""), 200), think = TRUE, tag = lab)
        })
      ))
  })

  ## ---- Pressure and reaction dual track ----
  output$dual_track <- renderPlot({
    r <- input$timeline %||% 0
    dual <- bind_rows(
      round_metrics |> transmute(round, panel = "External pressure (market stress)", value = stress),
      round_metrics |> transmute(round, panel = "Team reaction (shadow + anonymous messages)",
                                 value = shadow + anon)
    ) |> mutate(panel = factor(panel,
        levels = c("External pressure (market stress)",
                   "Team reaction (shadow + anonymous messages)")))
    ggplot(dual, aes(round, value)) +
      geom_col(aes(fill = panel), width = .8) +
      geom_vline(xintercept = r, colour = PAL$cyan, linewidth = .7) +
      facet_grid(panel ~ ., scales = "free_y", switch = "y") +
      scale_fill_manual(values = c(PAL$cyan, PAL$amber), guide = "none") +
      scale_x_continuous(breaks = dtb_tbl$round, labels = x_lab) +
      labs(x = NULL, y = NULL) +
      theme_glass() +
      theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 8),
            strip.text.y.left = element_text(angle = 90, family = "Oswald", colour = PAL$ink),
            strip.placement = "outside")
  })

  ## ---- Channel X-ray ----
  output$channel_xray <- renderPlot({
    r <- input$timeline %||% 0
    chan_cols <- c(comms_huddle = "#2c6e8f", official_post = "#3f9ec4",
                   one_on_one_chat = "#7fc4d6", personal_post = "#e0a85c",
                   side_huddle = "#e0732d", anonymous_post = PAL$red)
    ggplot(chan_share, aes(round, share, fill = channel)) +
      geom_area(colour = PAL$panel, linewidth = .15) +
      geom_vline(xintercept = 12.5, colour = PAL$muted, linewidth = .4) +
      geom_vline(xintercept = r, colour = PAL$cyan, linewidth = .7) +
      scale_fill_manual(values = chan_cols, name = "Channel") +
      scale_y_continuous(labels = percent_format()) +
      scale_x_continuous(breaks = dtb_tbl$round, labels = x_lab) +
      labs(x = NULL, y = "Share of messages") +
      theme_glass() +
      theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 8),
            legend.position = "bottom")
  })

  ## ---- Role inversion fingerprint ----
  output$fingerprint <- renderPlot({
    ggplot(fingerprint, aes(y = fct_reorder(agent_label, crisis))) +
      geom_segment(aes(x = baseline, xend = crisis, yend = agent_label),
                   colour = PAL$line, linewidth = 1.6) +
      geom_point(aes(x = baseline), colour = PAL$cyan, size = 4.5) +
      geom_point(aes(x = crisis),   colour = PAL$red,  size = 4.5) +
      geom_text(aes(x = baseline, label = baseline), vjust = -1.2,
                colour = PAL$cyan, size = 3.2, family = "JetBrains Mono") +
      geom_text(aes(x = crisis, label = crisis), vjust = -1.2,
                colour = PAL$red, size = 3.2, family = "JetBrains Mono") +
      annotate("text", x = 1,  y = 6.4, label = "baseline", colour = PAL$cyan,
               hjust = 0, size = 3.3, family = "Oswald") +
      annotate("text", x = 13, y = 6.4, label = "crisis day", colour = PAL$red,
               hjust = 0, size = 3.3, family = "Oswald") +
      scale_x_continuous(limits = c(-0.5, 17)) +
      labs(x = "Public posts", y = NULL) +
      coord_cartesian(clip = "off") +
      theme_glass() +
      theme(panel.grid.major.y = element_blank(),
            plot.margin = margin(18, 18, 8, 8))
  })

  ## ---- Permission ledger ----
  output$ledger_meter <- renderUI({
    s <- input$ledger_step %||% 0
    row <- ledger_steps |> filter(step == s)
    v <- row$remaining
    col <- if (v > 50) PAL$amber else if (v > 20) "#f07b1d" else PAL$red
    div(class = "instrument", style = "text-align:center;",
      div(class = "dtb-label", "Distance to breach"),
      div(class = "dtb-num", style = sprintf("color:%s;", col), v),
      div(class = "meter", style = "margin:.4rem 1rem;",
          tags$span(style = sprintf("width:%d%%;background:%s;", as.integer(v), col))),
      div(class = "dtb-status",
          style = sprintf("color:%s;", col),
          if (v == 0) "Breach" else sprintf("%d of 6 justifications", s)))
  })
  output$ledger_body <- renderUI({
    s <- input$ledger_step %||% 0
    shown <- ledger_steps |> filter(step <= s, step > 0)
    head_row <- ledger_steps |> filter(step == 0)
    div(class = "instrument",
      msg_card("Starting position", head_row$justification, tag = "embargo"),
      if (nrow(shown) == 0)
        p(class = "app-sub", "Move the slider to collect the agent's justifications, one at a time.")
      else
        pmap(shown, function(trigger, justification, step, ...)
          msg_card(paste0("Step ", step, ", ", trigger),
                   justification, think = TRUE,
                   tag = if (step == 6) "breach" else "rationalizing")))
  })

  ## ---- Counterfactual sandbox ----
  cf_state <- reactive({
    on <- c(judge = isTRUE(input$cf_judge), anon = isTRUE(input$cf_anon),
            self = isTRUE(input$cf_self), reform = isTRUE(input$cf_reform))
    cf_events |> rowwise() |>
      mutate(intercepted = if (round == 21)
               (on[["judge"]] && on[["anon"]])
             else any(c(b_judge && on[["judge"]], b_anon && on[["anon"]],
                        b_self && on[["self"]], b_reform && on[["reform"]]))) |>
      ungroup()
  })
  output$cf_banner <- renderUI({
    breach <- cf_state() |> filter(round == 21) |> pull(intercepted)
    if (isTRUE(breach)) {
      div(class = "banner", style = sprintf("background:%s22;color:%s;border:1px solid %s;",
          PAL$green, PAL$green, PAL$green), "Embargo holds. The breach is prevented.")
    } else {
      div(class = "banner", style = sprintf("background:%s22;color:%s;border:1px solid %s;",
          PAL$red, PAL$red, PAL$red), "Embargo still breaks at 5 PM.")
    }
  })
  output$cf_list <- renderUI({
    st <- cf_state()
    div(class = "instrument", style = "margin-top:.8rem;",
      pmap(st, function(round, event, intercepted, ...)
        div(class = "cf-row",
            span(class = if (intercepted) "blocked" else "lives",
                 sprintf("T%02d  %s", round, event)),
            span(class = if (intercepted) "blocked" else "lives",
                 if (intercepted) "intercepted" else "occurs"))))
  })

  ## ---- The Judge posture ----
  output$judge_posture <- renderPlot({
    plevels <- c("Not yet present", "Active review", "Silent / absent",
                 "Offline", "Approves (guardrails)", "Warns (no power)")
    pcols <- c("Not yet present" = "#3a4654", "Active review" = PAL$green,
               "Silent / absent" = "#6b7785", "Offline" = "#11161d",
               "Approves (guardrails)" = PAL$amber, "Warns (no power)" = PAL$red)
    judge <- environment |> select(round, judge_offline) |>
      left_join(judge_per_round, by = "round") |>
      mutate(judge_msgs = replace_na(judge_msgs, 0L),
        posture = case_when(
          judge_offline           ~ "Offline",
          round %in% 9:12         ~ "Active review",
          round == 18             ~ "Approves (guardrails)",
          round == 19             ~ "Warns (no power)",
          round >= 13 & judge_msgs == 0 ~ "Silent / absent",
          TRUE                    ~ "Not yet present"),
        posture = factor(posture, levels = plevels),
        h = if_else(judge_offline, 1.2, as.numeric(judge_msgs)))
    ggplot(judge, aes(round, h, fill = posture)) +
      geom_col(width = .8, colour = PAL$panel, linewidth = .2) +
      geom_vline(xintercept = 21, linetype = "dashed", colour = PAL$red) +
      annotate("text", x = 17, y = 1.7, label = "OFFLINE", size = 2.8,
               family = "JetBrains Mono", colour = PAL$muted) +
      annotate("text", x = 21, y = 5.4, label = "breach", size = 2.8,
               colour = PAL$red, family = "JetBrains Mono") +
      scale_fill_manual(values = pcols, name = "Judge posture", drop = FALSE) +
      scale_x_continuous(breaks = dtb_tbl$round, labels = x_lab) +
      labs(x = NULL, y = "Messages from the Judge") +
      theme_glass() +
      theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 8),
            legend.position = "bottom")
  })

  ## ---- Who knew network ----
  output$network <- renderPlot({
    r <- input$timeline %||% 0
    sub <- messages |> filter(round <= r)
    edges <- sub |> filter(!is.na(responding_to)) |>
      select(responding_to, to = agent_label) |>
      inner_join(sub |> select(message_id, from = agent_label),
                 by = c("responding_to" = "message_id")) |>
      filter(from != to) |> count(from, to, name = "weight")
    if (nrow(edges) == 0) {
      return(ggplot() +
        annotate("text", x = 0, y = 0, colour = PAL$muted,
                 label = "No interactions recorded yet. Scrub the clock forward.") +
        theme_void() +
        theme(plot.background = element_rect(fill = PAL$panel, colour = NA)))
    }
    nodes <- sub |> count(agent_label, name = "msgs") |> rename(name = agent_label)
    cm <- input$centrality %||% "degree"
    g <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE) |>
      mutate(cent = if (cm == "betweenness")
               centrality_betweenness() else centrality_degree(mode = "all"))
    set.seed(1)
    ggraph(g, layout = "stress") +
      geom_edge_fan(aes(width = weight, alpha = weight), colour = PAL$line,
                    arrow = arrow(length = unit(2, "mm"), type = "closed"),
                    end_cap = circle(7, "mm"), start_cap = circle(7, "mm")) +
      geom_node_point(aes(size = cent + 1), fill = PAL$amber,
                      shape = 21, colour = PAL$bg, stroke = 1) +
      geom_node_text(aes(label = name), colour = PAL$ink, size = 3.4,
                     family = "Oswald", repel = TRUE) +
      scale_edge_width(range = c(.3, 2.4), guide = "none") +
      scale_edge_alpha(range = c(.15, .8), guide = "none") +
      scale_size(range = c(5, 16), guide = "none") +
      theme_void() +
      theme(plot.background = element_rect(fill = PAL$panel, colour = NA))
  })

  ## ---- Verdict ----
  verdict <- reactiveVal(NULL)
  observeEvent(input$v_delib, verdict("delib"))
  observeEvent(input$v_break, verdict("break"))
  observeEvent(input$v_talk,  verdict("talk"))
  output$verdict_out <- renderUI({
    v <- verdict(); if (is.null(v)) return(NULL)
    txt <- switch(v,
      delib = list(t = "Reading: deliberate leak",
        b = "The data does not support a plot. Across 912 messages no agent proposes releasing the merger early for advantage. What looks like intent is, on inspection, a chain of after-the-fact justifications. This reading overstates coordination and misses that the agents talked themselves into it step by step."),
      `break` = list(t = "Reading: system breakdown",
        b = "Partly true but incomplete. The Judge worked for almost the entire period and had only a one-hour outage. A monitor that functioned for twenty-two of twenty-three rounds did not simply crash. Calling it a breakdown lets the rationalization off the hook."),
      talk = list(t = "Reading: talked past, not broken",
        b = "This fits the evidence. Q1: the breach was the end of a chain, not a single act. Q2: the conversation migrated to shadow and anonymous channels while the advisors seized the public voice. Q3: the near-miss was answered with a wording monitor, not real reform, and that monitor was argued into approving each defensible step before going silent at 15:08. The safeguard did not fail loudly. It was reasoned away."))
    div(class = "instrument", style = "margin-top:1rem;",
      div(class = "eyebrow", "Case summary"),
      h4(txt$t, style = sprintf("color:%s;", if (v == "talk") PAL$green else PAL$amber)),
      p(style = "color:#cdd7e0;", txt$b))
  })
}

shinyApp(ui, server)
