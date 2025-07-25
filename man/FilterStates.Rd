% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FilterStates.R
\docType{class}
\name{FilterStates}
\alias{FilterStates}
\title{\code{FilterStates} \code{R6} class}
\description{
Abstract class that manages adding and removing \code{FilterState} objects
and builds a \emph{subset expression}.

A \code{FilterStates} object tracks all condition calls
(logical predicates that limit observations) associated with a given dataset
and composes them into a single reproducible \code{R} expression
that will assign a subset of the original data to a new variable.
This expression is hereafter referred to as \emph{subset expression}.

The \emph{subset expression} is constructed differently for different
classes of the underlying data object and \code{FilterStates} sub-classes.
Currently implemented for \code{data.frame}, \code{matrix},
\code{SummarizedExperiment}, and \code{MultiAssayExperiment}.
}
\keyword{internal}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-FilterStates-new}{\code{FilterStates$new()}}
\item \href{#method-FilterStates-destroy}{\code{FilterStates$destroy()}}
\item \href{#method-FilterStates-format}{\code{FilterStates$format()}}
\item \href{#method-FilterStates-get_call}{\code{FilterStates$get_call()}}
\item \href{#method-FilterStates-print}{\code{FilterStates$print()}}
\item \href{#method-FilterStates-remove_filter_state}{\code{FilterStates$remove_filter_state()}}
\item \href{#method-FilterStates-get_filter_state}{\code{FilterStates$get_filter_state()}}
\item \href{#method-FilterStates-set_filter_state}{\code{FilterStates$set_filter_state()}}
\item \href{#method-FilterStates-clear_filter_states}{\code{FilterStates$clear_filter_states()}}
\item \href{#method-FilterStates-ui_active}{\code{FilterStates$ui_active()}}
\item \href{#method-FilterStates-srv_active}{\code{FilterStates$srv_active()}}
\item \href{#method-FilterStates-ui_add}{\code{FilterStates$ui_add()}}
\item \href{#method-FilterStates-srv_add}{\code{FilterStates$srv_add()}}
\item \href{#method-FilterStates-clone}{\code{FilterStates$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FilterStates-new"></a>}}
\if{latex}{\out{\hypertarget{method-FilterStates-new}{}}}
\subsection{Method \code{new()}}{
Initializes \code{FilterStates} object by setting
\code{dataname}, and \code{datalabel}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FilterStates$new(
  data,
  data_reactive = function(sid = "") NULL,
  dataname,
  datalabel = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{(\code{data.frame} or \code{MultiAssayExperiment} or \code{SummarizedExperiment} or \code{matrix})
the \code{R} object which \code{subset} function is applied on.}

\item{\code{data_reactive}}{(\verb{function(sid)})
should return an object of the same type as \code{data} object or \code{NULL}.
This object is needed for the \code{FilterState} counts being updated
on a change in filters. If function returns \code{NULL} then filtered counts are not shown.
Function has to have \code{sid} argument being a character.}

\item{\code{dataname}}{(\code{character(1)})
name of the dataset, used in the subset expression.
Passed to the function argument attached to this \code{FilterStates}.}

\item{\code{datalabel}}{(\code{character(1)}) optional
text label.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Object of class \code{FilterStates}, invisibly.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FilterStates-destroy"></a>}}
\if{latex}{\out{\hypertarget{method-FilterStates-destroy}{}}}
\subsection{Method \code{destroy()}}{
Destroys a \code{FilterStates} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FilterStates$destroy()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FilterStates-format"></a>}}
\if{latex}{\out{\hypertarget{method-FilterStates-format}{}}}
\subsection{Method \code{format()}}{
Returns a formatted string representing this \code{FilterStates} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FilterStates$format(show_all = FALSE, trim_lines = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{show_all}}{(\code{logical(1)}) passed to \code{format.teal_slices}}

\item{\code{trim_lines}}{(\code{logical(1)}) passed to \code{format.teal_slices}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{character(1)} the formatted string
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FilterStates-get_call"></a>}}
\if{latex}{\out{\hypertarget{method-FilterStates-get_call}{}}}
\subsection{Method \code{get_call()}}{
Filter call

Builds \emph{subset expression} from condition calls generated by \code{FilterState}.
The \code{lhs} of the expression is a \code{dataname_prefixed}, where word prefixed refers to
situation when call is evaluated on elements of the original data, for example \code{dataname[[x]]}.
By default \code{dataname_prefixed = dataname} and it's not alterable through class methods.
Customization of \code{private$dataname_prefixed} is done through inheriting classes.

The \code{rhs} is a call to \code{private$fun} with following arguments:
\itemize{
\item \code{dataname_prefixed}
\item list of logical expressions generated by \code{FilterState} objects
stored in \code{private$state_list}. Each logical predicate is combined with \code{&} operator.
Variables in these logical expressions by default are not prefixed but this can be changed
by setting \code{private$extract_type} (change in the similar way as \code{dataname_prefixed})
Possible call outputs depending on a custom fields/options:
}

\if{html}{\out{<div class="sourceCode">}}\preformatted{# default
dataname <- subset(dataname, col == "x")

# fun = dplyr::filter
dataname <- dplyr::filter(dataname, col == "x")

# fun = MultiAssayExperiment::subsetByColData; extract_type = "list"
dataname <- MultiAssayExperiment::subsetByColData(dataname, dataname$col == "x")

# teal_slice objects having `arg = "subset"` and `arg = "select"`
dataname <- subset(dataname, subset = row_col == "x", select = col_col == "x")

# dataname = dataname[[element]]
dataname[[element]] <- subset(dataname[[element]], subset = col == "x")
}\if{html}{\out{</div>}}

If no filters are applied, \code{NULL} is returned to avoid no-op calls such as \code{dataname <- dataname}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FilterStates$get_call(sid = "")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sid}}{(\code{character})
when specified then method returns code containing condition calls (logical predicates) of
\code{FilterState} objects which \code{"sid"} attribute is different than this \code{sid} argument.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{call} or \code{NULL}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FilterStates-print"></a>}}
\if{latex}{\out{\hypertarget{method-FilterStates-print}{}}}
\subsection{Method \code{print()}}{
Prints this \code{FilterStates} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FilterStates$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{additional arguments passed to \code{format}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FilterStates-remove_filter_state"></a>}}
\if{latex}{\out{\hypertarget{method-FilterStates-remove_filter_state}{}}}
\subsection{Method \code{remove_filter_state()}}{
Remove one or more \code{FilterState}s from the \code{state_list} along with their UI elements.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FilterStates$remove_filter_state(state)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{state}}{(\code{teal_slices})
specifying \code{FilterState} objects to remove;
\code{teal_slice}s may contain only \code{dataname} and \code{varname}, other elements are ignored}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{NULL}, invisibly.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FilterStates-get_filter_state"></a>}}
\if{latex}{\out{\hypertarget{method-FilterStates-get_filter_state}{}}}
\subsection{Method \code{get_filter_state()}}{
Gets reactive values from active \code{FilterState} objects.

Get active filter state from \code{FilterState} objects stored in \code{state_list}(s).
The output is a list compatible with input to \code{self$set_filter_state}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FilterStates$get_filter_state()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Object of class \code{teal_slices}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FilterStates-set_filter_state"></a>}}
\if{latex}{\out{\hypertarget{method-FilterStates-set_filter_state}{}}}
\subsection{Method \code{set_filter_state()}}{
Sets active \code{FilterState} objects.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FilterStates$set_filter_state(state)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{state}}{(\code{teal_slices})}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Function that raises an error.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FilterStates-clear_filter_states"></a>}}
\if{latex}{\out{\hypertarget{method-FilterStates-clear_filter_states}{}}}
\subsection{Method \code{clear_filter_states()}}{
Remove all \code{FilterState} objects from this \code{FilterStates} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FilterStates$clear_filter_states(force = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{force}}{(\code{logical(1)})
flag specifying whether to include anchored filter states.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{NULL}, invisibly.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FilterStates-ui_active"></a>}}
\if{latex}{\out{\hypertarget{method-FilterStates-ui_active}{}}}
\subsection{Method \code{ui_active()}}{
\code{shiny} UI definition that stores \code{FilterState} UI elements.
Populated with elements created with \code{renderUI} in the module server.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FilterStates$ui_active(id)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(\code{character(1)})
\code{shiny} module instance id.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{shiny.tag}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FilterStates-srv_active"></a>}}
\if{latex}{\out{\hypertarget{method-FilterStates-srv_active}{}}}
\subsection{Method \code{srv_active()}}{
\code{shiny} server module.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FilterStates$srv_active(id)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(\code{character(1)})
\code{shiny} module instance id.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{NULL}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FilterStates-ui_add"></a>}}
\if{latex}{\out{\hypertarget{method-FilterStates-ui_add}{}}}
\subsection{Method \code{ui_add()}}{
\code{shiny} UI module to add filter variable.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FilterStates$ui_add(id)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(\code{character(1)})
\code{shiny} module instance id.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{shiny.tag}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FilterStates-srv_add"></a>}}
\if{latex}{\out{\hypertarget{method-FilterStates-srv_add}{}}}
\subsection{Method \code{srv_add()}}{
\code{shiny} server module to add filter variable.

This module controls available choices to select as a filter variable.
Once selected, a variable is removed from available choices.
Removing a filter variable adds it back to available choices.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FilterStates$srv_add(id)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(\code{character(1)})
\code{shiny} module instance id.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{NULL}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FilterStates-clone"></a>}}
\if{latex}{\out{\hypertarget{method-FilterStates-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FilterStates$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
