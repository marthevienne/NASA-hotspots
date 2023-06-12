add_row_to_df <- function(df, row, order_by = "none", decreasing = F) {
  res <- rbind(df, row)
  if (order_by == "none") {
    return(res)
  } else {
    if (decreasing) {
      res <- res %>%
        arrange_at(order_by, desc)
    } else {
      res <- res %>%
        arrange_at(order_by)
    }
    return(res)
  }
}