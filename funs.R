## functions ####
numstyle <- function(x){
  sapply(x, function(y) prettyNum(y,big.mark =" ",digits=2,nsmall=ifelse(y<9.95,1,0)))
}
summary_survey <- function(.data, dependent, explanatory, cont = "mean", cont_range = FALSE, 
                           p = FALSE, column = FALSE, digits = c(1, 1, 3, 1), fit_id = FALSE){
  # Define variable type
  explanatory_type = .data$variables %>% 
    select(explanatory) %>% 
    purrr::map(is.numeric)
  
  # Hypothesis test
  if(p){
    p_tests = explanatory %>% 
      purrr::map2(explanatory_type,
                  ~if(!.y){
                    survey::svychisq(as.formula(paste0("~", ., "+", dependent)), .data)$p.value %>% 
                      p_tidy(digits[3])
                  } else if (.y & cont == "mean"){
                    survey::svyttest(as.formula(paste0(.x, "~", dependent)), .data)$p.value %>% 
                      p_tidy(digits[3])
                  } else if (.y & cont == "median"){
                    survey::svyranktest(as.formula(paste0(.x, "~", dependent)), .data)$p.value %>% 
                      p_tidy(digits[3])
                  }
      )
  }
  
  # Output table
  explanatory %>% 
    purrr::map2(explanatory_type,
                ~ if(!.y){
                  survey::svytable(as.formula(paste0("~", .x, "+", dependent)), .data) %>% 
                    as.data.frame(stringsAsFactors = FALSE) %>% 
                    { if(column) {
                      dplyr::group_by(., !! sym(dependent)) %>% 
                        dplyr::mutate(
                          total = sum(Freq),
                          prop = 100 * Freq / total
                        )
                    } else { 
                      dplyr::group_by_at(., 1) %>% 
                        dplyr::mutate(
                          total = sum(Freq),
                          prop = 100 * Freq / total
                        )
                    }
                    } %>% 
                    dplyr::mutate(
                      value = paste0(Freq %>% round_tidy(digits[4]), " (", 
                                     prop %>% round_tidy(digits[4]), ")")
                    ) %>%
                    dplyr::select(-total, -prop, -Freq) %>% 
                    tidyr::pivot_wider(names_from = !! dependent, values_from = value) %>% 
                    dplyr::mutate(
                      label = names(.)[1]
                    ) %>% 
                    dplyr::rename(levels = 1)
                } else {
                  { if(cont == "mean") {
                    survey::svyby(as.formula(paste0("~", .x)), as.formula(paste0("~", dependent)), .data, svymean, na.rm = TRUE) %>%
                      dplyr::mutate(
                        value = paste0(!! sym(.x) %>% round_tidy(digits[1]), " (", 
                                       se %>%  round_tidy(digits[2]), ")") 
                      ) %>% 
                      dplyr::select(-c(.x, se)) %>% 
                      tidyr::pivot_wider(names_from = !! dependent, values_from = value) %>% 
                      dplyr::mutate(
                        label = .x,
                        levels = "Mean (SE)"
                      )
                  } else if(cont == "median"){
                    survey::svyby(as.formula(paste0("~", .x)), as.formula(paste0("~", dependent)), .data, 
                                  svyquantile, quantiles = 1:3/4, ci = TRUE, na.rm = TRUE) %>% 
                      dplyr::rename(Q1 = 2,
                                    Q2 = 3,
                                    Q3 = 4) %>% 
                      dplyr::mutate(
                        IQR = Q3 - Q1) %>% 
                        { if(cont_range){
                          dplyr::mutate(., 
                                        value = paste0(Q2 %>% round_tidy(digits[1]), " (", 
                                                       Q1 %>%  round_tidy(digits[2]), " to ",
                                                       Q3 %>% round_tidy(digits[2]), ")")
                          )                        
                        } else {
                          dplyr::mutate(.,
                                        value = paste0(Q2 %>% round_tidy(digits[1]), " (", 
                                                       IQR %>%  round_tidy(digits[2]), ")")
                          )
                        }} %>% 
                      dplyr::select(-c(2:8)) %>%                      
                      tidyr::pivot_wider(names_from = !! dependent, values_from = value) %>% 
                      dplyr::mutate(
                        label = .x,
                        levels = "Median (IQR)"
                      )
                  }
                  }
                }) %>% 
    
    # Add hypothesis test
                { if(p){
                  purrr::map2_df(., p_tests,
                                 ~ mutate(.x,
                                          p = .y)
                  )} else {
                    dplyr::bind_rows(.)
                  }} %>%
    dplyr::select(label, levels, dplyr::everything()) %>% 
    as.data.frame() %>%
    { if(fit_id){
      levels_id = .$levels
      drop = levels_id %in% c("Mean (SE)", "Median (IQR)")
      levels_id[drop] = ""
      mutate(., 
             fit_id = paste0(label, levels_id),
             index = 1:dim(.)[1])
    } else {
      .
    }} %>% 
    rm_duplicate_labels()
}
summary_cont <- function(.data, dependent = NULL, explanatory, ...){
  sum_mean <- summary_factorlist(.data, dependent, explanatory, cont="mean", cont_cut=2, ...) %>% 
    rowid_to_column("order")
  sum_meadian <- 
    summary_factorlist(.data, dependent, explanatory, cont="median", cont_range=TRUE, cont_cut=2, ...) %>% 
    mutate(label="") %>% 
    rowid_to_column("order") %>% mutate(order=order+0.5)
  rbind(sum_mean,sum_meadian) %>% arrange(order) %>% select(-order)
}
n_perc <- function(x,digits=1){paste0(sum(x)," (",round(mean(x)*100,digits),")")}
meansd <- function(x,digits=1){paste0(round(mean(x),digits)," (",round(sd(x),digits),")")}
mediqr <- function(x){paste0(round(median(x),1)," (",round(quantile(x,0.25),1),"-",round(quantile(x,0.75),1),")")}
mean_sd_med_iqr <- function(x,digits=1){
  paste0(
    round(mean(x),digits)," (",round(sd(x),digits),")__",
    round(median(x),1)," (",round(quantile(x,0.25),1),"-",round(quantile(x,0.75),1),")"
  )
}
n_mean_sd_med_iqr <- function(x,digits=1){
  paste0(
    length(x),"__",
    round(mean(x),digits)," (",round(sd(x),digits),")__",
    round(median(x),1)," (",round(quantile(x,0.25),1),"-",round(quantile(x,0.75),1),")"
  )
}

twoway_stat <- function(df,row_var,col_var,value_var,fun=length,rowTotal=TRUE,colTotal=TRUE){
  #  df %>% mutate(overall_=factor(1))
  row_var  <- enquo(row_var)
  print(row_var)
  col_var  <- enquo(col_var)
  value_var<- enquo(value_var)
  
  varlab <- pull(df,!!row_var) %>% var_label()
  varlab <- ifelse(is.null(varlab),select(df,!!row_var) %>% attr("names"),varlab)
  n_cat <- pull(df,!!row_var) %>% n_distinct()
  
  df <- df %>% select(!!row_var, !!col_var, !!value_var)
  suppressWarnings(rbind(
    df %>% group_by(!!row_var         ,!!col_var)          %>% summarise_all(fun), 
    df %>% group_by(!!row_var:='Total',!!col_var)          %>% summarise_all(fun),
    df %>% group_by(!!row_var         ,!!col_var:="Total") %>% summarise_all(fun),
    df %>% group_by(!!row_var:='Total',!!col_var:="Total") %>% summarise_all(fun)
  )) %>%  
    pivot_wider(names_from = !!col_var, values_from = !!value_var)  %>% 
    add_column(var=c(varlab,rep("",n_cat)), .before=1) %>% 
    rename("level"=2) -> df2
  if(!colTotal){
    df2 <- df2 %>% select(-Total)
  }
  if(!rowTotal){
    df2 <- df2 %>% filter(level!="Total")
  }
  df2
}


#
#  overwrite  survey_var.grouped_svy to avoid errors "
# 
# survey_var.grouped_svy <- function(
#   x, na.rm = FALSE, vartype = c("se", "ci", "var"), level = 0.95, df = Inf,
#   .svy = current_svy(), ...
# ) {
#   if (!is.null(vartype)) {
#     vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
#   }
#   if (missing(x)) stop("survey_var() can't be used with regards to the grouping variable.")
#   stop_for_factor(x)
#   if (is.logical(x)) x <- as.integer(x)
#   .svy <- set_survey_vars(.svy, x)
#   grps_formula <- survey::make.formula(group_vars(.svy))
#   
# #  if (any(dplyr::count(.svy$variables)$n < 2)) stop("Population variance can't be computed because some groups contain less than 2 observations.")
#   
#   stat <- survey::svyby(
#     ~`__SRVYR_TEMP_VAR__`, grps_formula, .svy, survey::svyvar,
#     na.rm = na.rm
#   )
#   out <- get_var_est(
#     stat, vartype, grps = group_vars(.svy), level = level, df = df, deff = FALSE
#   )
#   out
# }
# 
# environment(custom_survey_var.grouped_svy) <- asNamespace('srvyr')
# assignInNamespace("survey_var.grouped_svy", customGenomePlot, ns = "srvyr")
# 
# 
# 


