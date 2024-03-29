% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2_ggproto_visualize.r
\name{proto_text}
\alias{proto_text}
\title{Tour proto for data, text labels}
\usage{
proto_text(
  aes_args = list(vjust = "outward", hjust = "outward"),
  identity_args = list(nudge_x = 0.05),
  row_index = TRUE
)
}
\arguments{
\item{aes_args}{A list of arguments to call inside of aes().
aesthetic mapping of the primary geom. For example,
\code{geom_point(aes(color = my_fct, shape = my_fct))} becomes
\code{aes_args = list(color = my_fct, shape = my_fct)}.}

\item{identity_args}{A list of static, identity arguments passed into
the primary geom. For instance,
\code{geom_point(size = 2, alpha = .7)} becomes
\code{identity_args = list(size = 2, alpha = .7)}.
Also passes more foundational arguments such as stat and position, though
these have been tested less.}

\item{row_index}{A numeric or logical index of rows to subset to.
Defaults to NULL, all observations.}
}
\description{
Adds \code{geom_text()} of the projected data.
}
\examples{
library(spinifex)
dat     <- scale_sd(penguins_na.rm[, 1:4])
clas    <- penguins_na.rm$species
bas     <- basis_pca(dat)
mv      <- manip_var_of(bas)
gt_path <- save_history(dat, grand_tour(), max_bases = 5)

ggt <- ggtour(gt_path, dat, angle = .2) +
  proto_text(list(color = clas))
\donttest{
animate_plotly(ggt)
}

## Custom labels, subset of points
ggt2 <- ggtour(gt_path, dat) +
  proto_text(list(color = clas, size = as.integer(clas)),
             list(alpha = .7),
             row_index = 1:15)
\donttest{
animate_plotly(ggt2)
}
}
\seealso{
Other ggtour proto functions: 
\code{\link{append_fixed_y}()},
\code{\link{facet_wrap_tour}()},
\code{\link{ggtour}()},
\code{\link{proto_basis}()},
\code{\link{proto_default}()},
\code{\link{proto_density}()},
\code{\link{proto_density2d}()},
\code{\link{proto_hex}()},
\code{\link{proto_highlight}()},
\code{\link{proto_hline0}()},
\code{\link{proto_origin}()},
\code{\link{proto_point}()}
}
\concept{ggtour proto functions}
