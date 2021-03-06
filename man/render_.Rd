% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2_render.r
\name{render_}
\alias{render_}
\title{Prepare the ggplot object before passing to either animation package.}
\usage{
render_(
  frames,
  axes = "center",
  manip_col = "blue",
  line_size = 1L,
  text_size = 5L,
  aes_args = list(),
  identity_args = list(),
  ggproto = list(theme_spinifex())
)
}
\arguments{
\item{frames}{The result of \code{array2df()}, a long df of the projected frames.}

\item{axes}{Position of the axes, expects one of:
"center", "left", "right", "bottomleft", "topright", "off", or a
map_absolute() call. Defaults to "center".}

\item{manip_col}{String of the color to highlight the \code{manip_var}, if used.
Defaults to "blue".}

\item{line_size}{The size of the lines of the unit circle and variable
contributions of the basis. Defaults to 1.}

\item{text_size}{The size of the text labels of the variable
contributions of the basis. Defaults to 5.}

\item{aes_args}{A list of aesthetic arguments to passed to
\verb{geom_point(aes(X)}. Any mapping of the data to an aesthetic,
for example, \code{geom_point(aes(color = myCol, shape = myCol))} becomes
\code{aes_args = list(color = myCol, shape = myCol)}.}

\item{identity_args}{A list of static, identity arguments passed into
\code{geom_point()}, but outside of \code{aes()}; \code{geom_point(aes(), X)}.
Typically a single numeric for point size, alpha, or similar.
For example, \code{geom_point(aes(), size = 2, alpha = .7)} becomes
\code{identity_args = list(size = 2, alpha = .7)}.}

\item{ggproto}{A list of ggplot2 function calls.
Anything that would be "added" to ggplot(); in the case of applying a theme,
\code{ggplot() + theme_bw()} becomes \code{ggproto = list(theme_bw())}.
Intended for aesthetic ggplot2 functions (not geom_* family).}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}, see \code{\link{ggtour}}.
Typically called by \code{render_plotly()} or \code{render_gganimate()}. Takes the
result of \code{array2df()}, and renders them into a ggplot2 object.
}
\examples{
## Setup
dat_std <- scale_sd(wine[, 2:6])
clas <- wine$Type
bas <- basis_pca(dat_std)
mv <- manip_var_of(bas)

manual_array <- manual_tour(basis = bas, manip_var = mv)
manual_df <- array2df(array = manual_array, data = dat_std)

## Required arguments
render_(frames = manual_df)

## Full arguments
require("ggplot2")
render_(frames = manual_df, axes = "left", manip_col = "purple",
        aes_args = list(color = clas, shape = clas),
        identity_args = list(size = 1.5, alpha = .7),
        ggproto = list(theme_minimal(),
                       ggtitle("My title"),
                       scale_color_brewer(palette = "Set2")))
}
