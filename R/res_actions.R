#' A set of actions to a pply to an 'rset' object
#' 
#' @param res An object with class 'rset' (or 'rsplit').
#' @param len The length that the final object should be.
#' 
#' @noRd
res_truncate <- function(res, len) {
  if(nrow(res) == len) {
    return(res)
  }
  res[1:len,]
}

res_combine <- function(res, len) {
  if(nrow(res) == len) {
    return(res)
  }
  res_len <- nrow(res)
  combine_with <- rep(seq_len(len), length.out = res_len - len)
  combine_indexes <- purrr::map(seq_len(len), 
                                ~ which(combine_with == .) + len)
  to_combine <- purrr::map2(seq_len(len), combine_indexes, ~ {
    res$splits[c(.x, .y)]
  })
  
  lres <- res_truncate(res, len)
  lres$splits <- purrr::map(to_combine, combine_same_rsets)
  lres
}

res_combine_end <- function(res, len) {
  if(nrow(res) == len) {
    return(res)
  }
  
  res_len <- nrow(res)
  combine_indexes <- (len+1L):res_len
  to_combine <- res$splits[combine_indexes]
  
  lres <- res_truncate(res, len)
  lres$splits[[len]] <- combine_same_rsets(to_combine)
  lres
}

res_combine_random <- function(res, len) {
  if(nrow(res) == len) {
    return(res)
  }
  
  res_len <- nrow(res)
  combine_with <- c(
    safe_sample(rep(seq_len(len), (res_len %/% len) - 1L)),
    safe_sample(seq_len(len)[seq_len(res_len %% len)])
  )
  combine_indexes <- purrr::map(seq_len(len), 
                                ~ which(combine_with == .) + len)
  to_combine <- purrr::map2(seq_len(len), combine_indexes, ~ {
    res$splits[c(.x, .y)]
  })
  
  lres <- res_truncate(res, len)
  lres$splits <- purrr::map(to_combine, combine_same_rsets)
  lres
}

res_recycle <- function(res, len) {
  if(nrow(res) == len) {
    return(res)
  }
  
  res_len <- nrow(res)
  copy_of <- rep_len(seq_len(res_len), length.out <- len-res_len)
  to_copy <- purrr::map(copy_of, ~ {
    res$splits[.]
  })
  
  lres <- res_extend(res, len)
  lres$splits[(res_len + 1):len] <- to_copy
  lres
}

res_recycle_random <- function(res, len) {
  if(nrow(res) == len) {
    return(res)
  }
  
  res_len <- nrow(res)
  copy_of <- c(
    safe_sample(rep(seq_len(res_len), (len %/% res_len) - 1L)),
    safe_sample(seq_len(res_len)[seq_len(len %% res_len)])
  )
  to_copy <- purrr::map(copy_of, ~ {
    res$splits[.]
  })
  
  lres <- res_extend(res, len)
  lres$splits[(res_len + 1):len] <- to_copy
  lres
}

res_extend <- function(res, len) {
  extended_res <- tibble::add_row(res, splits = rep(NA, len - nrow(res)))
  id_prefix <- stringr::str_extract(res$id[1], "^[:alpha:]+")
  extended_res$id <- recipes::names0(len, prefix = id_prefix)
  extended_res
}

safe_sample <- function(x, ...) {
  if(length(x == 1)) {
    x
  } else {
    sample(x, ...)
  }
}

combine_same_rsets <- function(splits) {
  full_splits <- purrr::map(splits, rsample::populate)
  
  data <- full_splits[[1]]$data
  
  split_indexes <- full_splits %>%
    purrr::map(~ {list(.$in_id, .$out_id)})
  
  split_indexes %>%
    purrr::transpose() %>%
    purrr::map(combine_indices) %>%
    purrr::set_names() %>%
    rlang::set_names("analysis", "assessment") %>%
    rsample::make_splits(data = data)
}

combine_indices <- function(list) {
  purrr::flatten_int(list) %>%
    unique()
}
