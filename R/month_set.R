#' @title Get Monthly AT Internet Data - Set Period
#' @description This function allows you to get monthly data from 'AT Internet' using set perdiod, with a start date and an end date.
#' METRICS are visits, visitors, conversions visits, conversions rate, visits with sales, time spent per pages, bounce rate
#'
#' @param ATuser Your 'AT Internet' user login
#' @param ATpassword Your 'AT Internet' password login
#' @param space Analysis space
#' @param start_date Start date YYYY-MM
#' @param end_date End date YYYY-MM
#'
#' @param dim_source Optional - DIMENSION Trafic Source : Direct+traffic, Search+engines, "Referrer+sites", "Social+Campaigns"
#' @param dim_device Optional - DIMENSION Device Type
#' @param dim_level2 Optional - DIMENSION Sub-set of your site
#'
#' @param segment Optional - SEGMENT applied to the result set
#'
#' @param pagination  By defaut the pagination is set to 1. This allows you to get 10,000 rows of data. If you need more, you must make multiple calls to the API using the pagination feature.
#'
#' @return month_set extract the data you need from 'AT Internet' database, according to parameters you entered and then provides a dataset.
#'
#' @import httr
#' @import data.table
#'
#' @examples
#'\dontrun{
#'ATuser <- "user_email_adress"
#'ATpassword <- "my_password"
#'space <- 113736
#'start_date <- "2018-11"
#'end_date <- "2018-11"
#'dim_source <- "Search+engines"
#'dim_device <- "Desktop"
#'dim_level2 <- "MyLevel2"
#'segment <- 204529097
#'pagination <- 1
#'
#' dataset <- month_set(ATuser, ATpassword, space, start_date, end_date, dim_source, dim_device, dim_level2, segment, pagination)
#' dataset2 <- month_set(ATuser, ATpassword, space, start_date, end_date, dim_source=NULL, dim_device=NULL, dim_level2, segment, pagination)
#'}
#'
#' @export

month_set <- function(ATuser, ATpassword, space, start_date, end_date, dim_source = NULL, dim_device = NULL, dim_level2 = NULL, segment = NULL, pagination=1){

  ATpassword <- as.character(ATpassword)

  httr::set_config( httr::config( ssl_verifypeer = 0L ) )

  result <- data.frame(matrix(nrow=0, ncol=12))


  #MONTHLY
  cnames <- c("d_time_month","d_time_year","d_l2","d_source","d_device_type",
              "m_visits","m_visitors","m_conversions1_visits","m_conversions1_rate",
              "m_v_sales","m_time_spent_per_pages", "m_bounce_rate"
  )

  colnames(result) <- cnames


  api_request <- paste0("https://apirest.atinternet-solutions.com/data/v2/json/getData?",


                         "&columns={d_time_month,d_time_year,d_l2,d_source,d_device_type,m_visits,m_visitors,m_conversions1_visits,m_conversions1_rate,m_v_sales,m_time_spent_per_pages,m_bounce_rate}",
                         "&sort={d_time_month}",
                         "&space={s:'", space,"'}",
                         "&period={M:{start:'",start_date,"',end:'",end_date,"'}}",

                        #OPTIONAL
                        "&filter={",

                        #Dim_Source
                        ifelse(!is.null(dim_source),
                               paste0("d_source:{$eq:'",dim_source,"'}"),""),

                        #First Comma
                        ifelse(
                          ((!is.null(dim_source) & !is.null(dim_device)) | (!is.null(dim_source) & !is.null(dim_level2))),
                          paste0(","),
                          ""),

                        #Dim_Device
                        ifelse(!is.null(dim_device),
                               paste0("d_device_type:{$eq:'",dim_device,"'}"),""),

                        #Second Comma
                        ifelse(
                          ((!is.null(dim_device) & !is.null(dim_level2)) | (!is.null(dim_source) & !is.null(dim_device) & !is.null(dim_level2))),
                          paste0(","),
                          ""),

                        #Dim_level2
                        ifelse(!is.null(dim_level2),
                               paste0("d_l2:{$lk:'",dim_level2,"'}"),""),

                        "}",


                        #"&segment=",segment,
                        ifelse(!is.null(segment),
                               paste0("&segment=",segment),""),


                        "&max-results=10000&page-num=",pagination
  )


    result = tryCatch({

      api_request <- URLencode(api_request, repeated = TRUE)#pour encoder en UTF 8
      req <- httr::GET(api_request, httr::authenticate(user = ATuser, password = ATpassword))
      if (req$status_code != 200) {
        print("***** HTTP QUERY FAILED with code ")
        print (req$status_code)
        print (httr::content(req))
        stop("HTTP query failed")
      }


      result_doing <- httr::content(req)
      list <- result_doing$DataFeed[[1]]$Rows
      result_doing <- data.table::rbindlist(list, fill=TRUE)

      df <- rbind(result, result_doing)
    })

  df$space <- space
  df$segment <- segment

  df$date <- paste0(df$d_time_year,"-",df$d_time_month)

  #MONTHLY
  df <- df[order(df$date),]

 return(df)

}


