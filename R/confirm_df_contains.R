confirm_df_contains <- function(df, name, type=NULL) {
  if(!inherits(df,'data.frame')) {
    rlang::abort('df must be a data frame.')
  }
  cols = colnames(df)
  if(!name %in% cols) {
    rlang::abort(paste0('df must contain the column "',name,'".'))
  }
  if(!is.null(type)) {
    if(!inherits(df[[name]],type)) {
      rlang::abort(paste0('Column "',name,'" must be of type "',type,'".'))
    }
  }
  return(TRUE)
}
