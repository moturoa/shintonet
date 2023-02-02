
configurationObject <- R6Class(
  public = list(

    filename = NULL,
    conf = NULL,

    initialize = function(filename = NULL, tenant = NULL){

      if(is.null(filename)){
        filename <- glue::glue("config_site/{tenant}/config_site.yml")
      }

      self$filename <- filename

      self$conf <- yaml::read_yaml(self$filename)


    },

    get = function(what, separator = "/", default = FALSE, groups = NULL){

      x <- strsplit(what, separator)[[1]]
      out <- self$conf

      tm <- try({
        for(i in seq_along(x))out <- out[[x[i]]]
      }, silent = TRUE)

      if(inherits(tm, "try-error")){
        warning(paste("Not found in config_site: "), what)
        return(default)
      }


      if(is.null(out)){
        return(default)
      } else if(!is.null(groups)){

        if(!is.null(self$get("global/use_groups", default = NULL))){
          g <- sapply(out, "[[", "group")
          keep_col <- vapply(g, function(x)is.null(x) || x %in% groups,  # als null, geen 'group' in de config, dan altijd tonen
                 FUN.VALUE=logical(1), USE.NAMES=FALSE)
          out <- out[keep_col]
        }

      }

      return(out)

    }

  )
)


