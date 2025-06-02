#' SRS search
#'
#' @param query
#' @param method
#'
#' @returns
#' @export

srs_search <- function(query, method) {
  request(
    "https://cdxapps.epa.gov/oms-substance-registry-services/rest-api/autoComplete/nameSearch"
  ) |>
    req_url_query(
      #begins, contains, exact
      term = query,
      qualifier = method
    ) |>
    req_headers(
      accept = "*/*"
    ) |>
    #req_dry_run()
    req_perform() %>%
    resp_body_json() %>%
    map(., as_tibble) %>%
    list_rbind()
}

#' SRS details
#'
#' @param query
#'
#' @returns
#' @export

srs_details <- function(query) {
  request(
    "https://cdxapps.epa.gov/oms-substance-registry-services/rest-api/substance/itn/"
  ) |>
    req_url_path_append(query) %>%
    # req_url_query(
    #   excludeSynonyms = "true"
    # ) |>
    req_headers(
      accept = "application/json"
    ) |>
    #req_dry_run()
    req_perform() %>%
    resp_body_json() %>%
    pluck(., 1) %>%
    modify_at(
      "synonyms",
      ~ length(.x)
    ) %>%
    flatten() %>%
    compact() %>%
    map(
      .,
      ~ if (is.null(.x)) {
        NA
      } else {
        .x
      }
    ) %>%
    as_tibble()
}


`%ni%` <- Negate(`%in%`)


#' Compare different string hashes against each other
#'
#' @param target
#' @param dict
#'
#' @returns
#' @export

similar_hash <- function(
  target,
  target_by,
  dict,
  dict_by,
  osa = NA,
  lcs = NA,
  hamm = NA,
  cos = NA,
  jw = NA,
  jacc = NA
) {
  target_by <- rlang::enquo(target_by)
  dict_by <- rlang::enquo(dict_by)

  methods <- c("osa", "lv", "dl", "hamming", "lcs", "cosine", "jaccard", "jw")

  init <-
    map(
      methods,
      ~ {
        stringdist_join(
          x = target,
          y = dict,
          by = c(!!target_by := !!dict_by),
          #by = c(rlang::quo_name(target_by) = rlang::quo_name(dict_by)),
          # mode = 'right',
          method = .x,
          # max_dist = 9,,
          ignore_case = TRUE,
          distance_col = "dist"
        )
      },
      .progress = T
    ) %>%
    set_names(., methods)
}

similar_hash <- function(
  target,
  target_by,
  dict,
  dict_by,
  osa = 1,
  lcs = 1,
  hamm = 1,
  cos = NA,
  jw = NA,
  jacc = NA
) {
  methods <- c("osa", "lv", "dl", "hamming", "lcs", "cosine", "jaccard", "jw")

  init <-
    map(
      methods,
      ~ {
        stringdist_join(
          x = target,
          y = dict,
          by = setNames(dict_by, target_by),
          method = .x,
          ignore_case = FALSE,
          distance_col = "dist"
        )
      },
      .progress = T
    ) %>%
    set_names(., methods)

  {
    #
    hash_1 <-
      keep(init, names(init) %in% c("osa", "lv", "dl")) %>%
      list_rbind(names_to = "method") %>%
      filter(dist <= quantile(dist, 0.05))

    #
    hash_2 <-
      keep(init, names(init) %in% c("lcs")) %>%
      list_rbind(names_to = "method") %>%
      filter(dist <= quantile(dist, 0.05))

    hash_3 <-
      keep(init, names(init) %in% c("hamming")) %>%
      list_rbind(names_to = "method") %>%
      filter(dist <= quantile(dist, 0.05))

    #
    hash_4 <-
      keep(init, names(init) %in% c("cosine", "jw")) %>%
      list_rbind(names_to = "method") %>%
      filter(
        method == 'cosine' &
          dist <= quantile(dist, 0.05) |
          method == 'jw' & dist <= quantile(dist, 0.05)
      )

    #
    hash_5 <-
      keep(init, names(init) %in% c("jaccard")) %>%
      list_rbind(names_to = "method") %>%
      filter(
        dist <= quantile(dist, 0.05)
      )

    hash <- bind_rows(
      hash_1,
      hash_2,
      hash_3,
      hash_4,
      hash_5
    )

    hash_cur <- hash %>%
      arrange(method, dist) %>%
      distinct(param, method, .keep_all = TRUE) %>%
      pivot_wider(
        id_cols = c(param, idx_n),
        names_from = c(method),
        values_from = c(preferredName, dist),
        values_fn = function(x) paste(x, collapse = ",")
      )

    return(hash_cur)
  }
}
