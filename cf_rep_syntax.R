library(plyr)
library(tidyverse)
library(broom)
library(magrittr)
library(lubridate)
library(purrrlyr)
library(miceadds)
library(emmeans)
library(ggstance)

# table 2

"https://github.com/thomasjwood/conjecture_falcon/raw/master/t1.RDS" %>% 
  url %>% 
  gzcon %>% 
  readRDS %>% 
  select(
    -c(1:4)
  ) %>% 
  factanal(
    factors = 4
  ) %>% 
  print(
    cutoff = .39
  ) 

# figure 1


d1 <- "https://github.com/thomasjwood/conjecture_falcon/raw/master/t2.RDS" %>% 
  url %>% 
  gzcon %>% 
  readRDS %>% 
  group_by(
  study, wave2
  ) %>% 
  by_slice(
    function(i){
      
      c("dv_pres",
        "dv_resp",
        "dv_conf",
        "dv_vals",
        "dv_policy") %>% 
        str_c(
          " ~  cond2"
        ) %>% 
        map(
          possibly(
            function(j)

            lm.cluster(
              as.formula(j), 
              data = i,
              cluster = i$clst
            ), 
            NULL
          )
        )
    }
  )


d1$tl <-d1$.out %>% 
  map(
    function(i)
      
      i %>%
      map(
        possibly(
          function(j)
            
            j %>%
            summary %>% 
            tbl_df %>% 
            set_colnames(
              c("estimate",
                "std_err",
                "t_val",
                "p_val")
            ) %>% 
            mutate(
              xval = j %>% 
                summary %>% 
                attr("dimnames") %>% 
                extract2(1),
              dv = j$lm_res$model %>% 
                names %>% 
                extract2(1),
              n = j$lm_res %>% 
                nobs(),
              rsqr= j$lm_res %>% 
                summary %>% 
                use_series("r.squared")
            ),
          NULL
        )
      )
  )

d1_1 <- d1 %>% 
  select(
    -.out
  ) %>%
  tbl_df %>% 
  pmap_dfr(
    function(study, wave2, tl)
      
      tl %>% 
      bind_rows %>% 
      mutate(
        study = study,
        wave2 = wave2
      )
  )

d1_2 <- d1_1 %>% 
  filter(
    wave2 %>% 
      equals("w1") %>% 
      not &
      study %>% 
      str_detect("harvey") %>% 
      not &
      xval %>% 
      str_detect(
        "treatment"
      )
  ) %>% 
  mutate(
    dv_lab = dv %>% 
      mapvalues(
        c("dv_pres", "dv_resp", "dv_conf", "dv_vals", "dv_policy"),
        c("Combined presidential scale",
          "Trump enjoys elites' respect",
          "Confident in Trump as a national steward",
          "Trump exemplifies presidential values",
          "Share Trump's policy positions")
      ) %>% 
      factor(
        c("Trump enjoys elites' respect",
          "Confident in Trump as a national steward",
          "Trump exemplifies presidential values",
          "Combined presidential scale",
          "Share Trump's policy positions")
      ),
    dv_fac = dv_lab %>% 
      mapvalues(
        c("Trump enjoys elites' respect",
          "Confident in Trump as a national steward",
          "Trump exemplifies presidential values",
          "Combined presidential scale",
          "Share Trump's policy positions"),
        c("Separate batteries",
          "Combined presidentialism scale",
          "Share Trump's policy positions") %>% 
          rep(c(3, 1, 1))
      ) %>% 
      factor(
        c("Separate batteries",
          "Combined presidentialism scale",
          "Share Trump's policy positions")
      ),
    study_lab = study %>% 
      mapvalues(
        c("inauguration 2017",
          "sotu 2017",
          "sotu 2018", 
          "sotu 2019"),
        c("Inauguration",
          str_c(
            "State of the Union\n(",
            2017:2019,
            ")"
          )
        )
      ) %>% 
      factor(
        c("Inauguration",
          str_c(
            "State of the Union\n(",
            2017:2019,
            ")"
          )
        )
      ),
    est_lab = estimate %>% 
      round(2) %>% 
      as.character %>% 
      str_sub(2) %>%
      str_pad(3, "right", "0") %>% 
      str_c(
        c("***", "**", "*", "") %>% 
          extract(
            p_val %>%
              findInterval(
                c(-Inf, .001, .01, .05, Inf)
              )
          )
      )
  )

d1_2$est_lab[
  d1_2$estimate %>% 
    equals(
      d1_2$estimate %>%
        abs %>%
        min
    )
  ] <- ".001"

d1_2$est_lab[
  d1_2$estimate %>% 
    is_less_than(
      0
    )
] %<>% 
  str_replace_all(
    fixed("0."),
    "-."
  ) %>% 
  str_pad(
    width = 4, side = "right", pad = "0"
  )

ggplot() +
  geom_bar(
    aes(dv_lab, estimate, fill = wave2, group = wave2),
    stat = "identity",
    position = position_dodge2(
      padding = .3,
      width = 2,
      preserve = "single"),
    data = d1_2,
    size = .25,
    width = .75,
    color = "black") +
  geom_text(
    aes(dv_lab, 
        estimate %>% 
          is_less_than(0) %>% 
          ifelse(
            estimate %>% 
              subtract(.05),
            estimate %>% 
              add(.05)
          ), 
        group = wave2, label = est_lab),
    position = position_dodge2(
      preserve = "single",
      width = .75),
    data = d1_2,
    size = 3,
    color = "black") +
  facet_grid(
    study_lab ~ dv_fac, 
    scales = "free_x",
    space = "free_x",
    labeller = label_wrap_gen(width = 25)
  ) +
  scale_x_discrete(
    breaks = d1_2$dv_lab %>% 
      factor %>% 
      levels,
    labels = d1_2$dv_lab %>% 
      factor %>% 
      levels %>% 
      extract(1:3) %>% 
      str_wrap(width = 20) %>% 
      c("", "")
  ) +
  scale_fill_grey(
    start = .5, 
    end = .99, 
    breaks = str_c("w", 2:4),
    labels = str_c("Wave ", 2:4)
  ) +
  labs(
    x = "",
    y = "Average treatment effect (7pt scale)",
    fill =  "",
    caption  = "Note *** p <.001; ** p < .01; * p < .05"
  ) +
  scale_y_continuous(
    breaks = NULL 
  ) 


#  Figure 3

d2 <- "https://github.com/thomasjwood/conjecture_falcon/raw/master/t3.RDS" %>% 
  url %>% 
  gzcon %>% 
  readRDS


d2_1 <- expand.grid(
  cond2 = c(
    "control", "treatment"
  ),
  partyid2 = c(
    "democrat", "independent", "republican"
    )
  )

d2$cond2 %<>%
  factor(
    d2$cond2 %>%
      levels %>%
      rev
  )

d2_2 <- d2 %>% 
  group_by(
    study, wave2
  ) %>% 
  by_slice(
    function(i)
      
      
      lm.cluster(
        as.formula("dv_pres ~ cond2 * partyid2"),
        data = i,
        cluster = i$clst
      ) %>% 
      extract2("lm_res") %>% 
      emmeans(
        pairwise ~ cond2 | partyid2,
        data = i, 
      ) %>% 
      extract2("contrasts") %>% 
      tidy %>% 
      mutate(
        partyid2 = partyid2 %>% 
          as.character
      ),
    .collate = "rows"
  ) %>% 
  filter(
    study %>% 
      str_detect("harvey ") %>% 
      not &
      wave2 %>% 
      equals("w1") %>% 
      not
  )  %>%  
  mutate(
    lab = estimate %>% 
      round(2) %>% 
      as.character %>% 
      str_replace(fixed("0."), ".") %>% 
      str_pad(
        width = 3, side = "right", pad = "0"
      ) %>% 
      str_c(
        c("***", "**", "*", "") %>% 
          extract(
            p.value %>%
              findInterval(
                c(-Inf, .001, .01, .05, Inf)
              )
          )
      ),
    partyid2 = partyid2 %>% 
      factor(
        c("democrat",
          "independent",
          "republican")
      ),
    lo = estimate %>% 
      subtract(
        std.error %>% 
          multiply_by(1.96)
      ),
    hi = estimate %>% 
      add(
        std.error %>% 
          multiply_by(1.96)
      )
  ) 

d2_2 %<>% 
  mutate(
    wave2 = wave2 %>% 
      mapvalues(
        str_c("w", 2:4),
        str_c("Wave ", 2:4)
      ),
    study = study %>% 
      str_replace_all(
        c("inauguration" = "Inauguration",
          "sotu" = "SOTU")
      ) %>%
      str_replace_all(
        " ", "\n"
      ) %>% 
      factor(
        c("Inauguration 2017",
          str_c("SOTU ", 2017:2019)) %>%
          str_replace_all(
            " ", "\n"
          )
      )
  ) 

d2_2 %>% 
  ggplot() +
  geom_vline(
    aes(
      xintercept  = xint
    ),
    data = nesting(
      study = d2_2$study,
      wave2 = d2_2$wave2
    ) %>% 
      mutate(
        xint = 0
      ),
    size = .15,
    linetype = "dashed"
  ) +
  geom_linerangeh(
    aes(
      y = 0,
      xmin = lo,
      xmax = hi,
      color = partyid2
    ),
    position = position_dodgev(height = .3),
  ) +
  geom_point(
    aes(
      y = 0,
      x = estimate,
      color = partyid2
    ),
    shape = 21,
    fill = "white",
    size = 6,
    position = position_dodgev(height = .3),
  ) +
  geom_text(
    aes(
      y = 0,
      x = estimate,
      label = lab,
      group = partyid2
    ),
    size = 2,
    position = position_dodgev(height = .3),
  ) +
  geom_label(
    label.size = 0,
    # fill = "green",
    fill = "grey97",
    aes(x = lo %>% 
          subtract(.35),
        y = 0,
        label = partyid2 %>% 
          str_to_title,
        color = partyid2),
    position = position_dodgev(height = .3),
    data = d2_2 %>% 
      filter(
        wave2 == "Wave 2" &
          study == "Inauguration\n2017"  
      ),
    size = 2.5,
    fontface = "italic",
  ) +
  facet_grid(
    study ~ wave2
  ) +
  scale_y_continuous(
    breaks = NULL
  ) +
  scale_colour_grey(
    start = .01, 
    end = .6
  ) +
  labs(
    x = "Conditional difference (7pt scale)",
    y =  ""
  ) 


# figure 3

d3 <- "https://github.com/thomasjwood/conjecture_falcon/raw/master/t4.RDS" %>% 
  url %>%
  gzcon %>% 
  readRDS

d4 <- "https://github.com/thomasjwood/conjecture_falcon/raw/master/t5.RDS" %>% 
  url %>%
  gzcon %>% 
  readRDS

l_m2 <- 1e3 %>% 
  rerun(
    d3 %>% 
    names %>% 
    str_subset("dv2_") %>% 
    str_c(
      " ~ ",
      d3 %>% 
        names %>% 
        extract(5:9) %>% 
        str_c(collapse = " + ")
      ) %>% 
      map_df(
        function(i)
          d4 %>% 
          mutate(
            fit = glm(i,
                      data = d3 %>% 
                        sample_frac(1, T),
                      family = binomial()) %>% 
              predict(
                newdata = d4,
                type = "response"
              )
          )
      )
    ) %>% 
  bind_rows %>% 
  tbl_df %>% 
  mutate(dv = names(d3) %>% 
           str_subset("dv2_") %>% 
           rep(each = 10) %>% 
           rep(times = 1000), 
         iter = 1:1000 %>% 
           rep(each = 40),
         rowind = 1:10 %>% 
           rep(times = 4000),
         type = 1:2 %>% 
           rep(times = 20000)
  )

et <- l_m2 %>% 
  select(
    -fact_pres:-ideol, -rowind
  ) %>% 
  spread(type, fit) %>% 
  mutate(eff = `2` - `1`,
         dv = dv %>% 
           mapvalues(
             names(d3) %>% 
               str_subset("dv2_"),
             c("If the election were held today, would you want to see the Democratic party control the House of Representatives?",
               "Should President Trump be impeached and removed from office?",
               "Did President Trump's campaign collude with the Russian goverment in the 2016 presidential election?",
               "Would you take part in a protest against President Trump?")
           ) %>% 
           factor(
             c("If the election were held today, would you want to see the Democratic party control the House of Representatives?",
               "Should President Trump be impeached and removed from office?",
               "Did President Trump's campaign collude with the Russian goverment in the 2016 presidential election?",
               "Would you take part in a protest against President Trump?")
           )
  )


library(ggbeeswarm)

et$varying %<>% 
  factor(
    et %>% 
      group_by(varying) %>% 
      summarise(
        mu = eff %>% 
          mean
      ) %>% 
      arrange(
        desc(mu)
      ) %>% 
      use_series(varying)
  )

et$varying %<>% 
  mapvalues(
    c("fact_pres",
      "fact_approve",
      "ft_trump",
      "ideol",
      "party"),
    c("Presidentialism",
      "Approval",
      "Trump Affect",
      "Ideology",
      "Partisanship")
  )

et_labs <- et %>% 
  group_by(
    varying, dv
  ) %>% 
  summarise(
    mu = eff %>% 
      mean,
    lab = mu %>% 
      round(2) %>% 
      as.character %>% 
      str_replace("0.", ".")
  )

et %>% 
  ggplot() +
  geom_vline(
    xintercept = 0,
    linetype = 2
  ) +
  geom_quasirandom(
    width = .2,
    # binwidth = .05,
    # varwidth = .15,
    aes(eff, 
        varying,
        color = varying %>% 
          str_detect("Pres") %>% 
          ifelse(
            "grey5",
            "grey50"
          )
    ),
    # method = "tukey", 
    alpha = .1,
    groupOnX = F) +
  geom_point(
    aes(mu, 
        varying),
    data = et_labs,
    shape = 21,
    size = 7,
    fill = "grey98",
    color = "black"
  ) +
  geom_text(
    aes(mu, 
        varying, 
        label = lab,
        fontface = varying %>% 
          str_detect("Pres") %>% 
          ifelse(
            "bold", "plain"
          )
    ),
    size  = 2.5,
    data = et_labs
  ) +
  labs(
    x = "Effect of 1 sd increase in predictive covariate on\nprobability of agreeing with faceted survey item",
    y = ""
  ) +
  facet_grid(
    dv ~ .,
    labeller = label_wrap_gen(width = 35)
  ) +
  scale_color_manual(
    values = c("grey30",
               "grey70")
  )