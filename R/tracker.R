trackerOptions = R6::R6Class(
  "trackerOptions",

  private = list(
    tracker_types = c("file", "gtag", "custom"),

    active_trackers = list(),

    gTagCallback = function() {

    }
  ),

  public = list(
    get_tracker = function(id) {
      private$active_trackers[[id]]
    },

    add_tracker = function(id, type, ...) {
      if (!type %in% private$tracker_types) {
        stop("Type is not supported. Supported types: ", paste0(private$tracker_types, collapse = ", "), ".")
      }

      private$active_trackers <- modifyList(
        private$active_trackers,
        list(id = list(type = type, ...))
      )

      invisible(self)
    },

    remove_tracker = function(id) {
      private$active_trackers[[id]] <- NULL

      invisible(self)
    },

    get_active_trackers = function() {
      private$active_trackers
    }
  )
)

#' @export
tracker_options = trackerOptions$new()

#' Allows collecting usage metrics on diferent events.
#'
#' @param session Current session object
#' @param category The category of the event
#' @param action The event action name
#' @param label The event description
#'
#' @export
trackEvent <- function(session, category, action, label) {
  session$sendCustomMessage(
    "trackEvent",
    list(
      category = category,
      action = action,
      label = label
    )
  )
}
