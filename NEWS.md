
# spinifex v0.3

- New ggproto api for visuals, see `?ggplot_tour()` to get started!!
-- 10x new `ggproto_*` functions
-- 2x animation funcs; `animate_plotly()` and `aniamte_gganimate()`.
-- 2x internal utility functions
- New vignette demonstrating this api `vignette("ggproto_api", "spinifex")`
- Shiny app now imports .csv & .rds,  code improved, uses new ggproto api
-- `run
- Fixed a format issue with the `BeastCancer` dataset

# spinifex v0.2.8

- Vectorized all `for` loops
- `manip_var_of(basis)` suggests a variable to used based on the rank of the contributions of the basis.

Basis_* functions to find features of interest, powered by `{Rdimtools}`.
- `basis_olda()`
- `basis_odp()`
- `basis_odp()`
- `basis_odp()`

New util functions:
- `scale_sd()` center and scale each variable by it's standard deviation
- `scale_01()` center and scale each variable to be between [0, 1]
- `basis_half_circle()`, variable agnostic basis with minimal variable dependence.
- `as_history_array()`, coerces an array of bases into the same attributes and class as returns of `tourr::save_history()`


# spinifex v0.2.7

- Gives deprecated warning when using `view_basis()` and `oblique_basis()`, no longer listed in `view_frame()` documentation
- Fixed "radial_tour" shiny app to reflect argument changes
- Fixed example run time for `render_plotly()` going longer than 10 seconds


# spinifex v0.2.6

- New argument, `ggproto`, accepts a list of `{ggplot2}` objects for more control over visual output
- New argument, `aes_args`, accepts a list of arguments to pass into the `aes()` call within `geom_point()`. This is used for variable mappings, such as color, but needs to be qualified as this is standard evaluation
- New argument, `identity_args`, accepts a list of arguments to pass call within `geom_point()`, but outside of `aes()`. This is used for scalar options or manually mapped vector such as point size or alpha
- New datasets, `PimaIndiansDiabetes_long` & `PimaIndiansDiabetes_wide`
- Fixed the centering of axes across frames
- Fixed `scale_axes()` to automatically scale to data rather than assume the data is scaled to [0, 1]
- Changed notation from "slide(s)" to "frame(s)" through-out
- Deprecated `view_basis()` and `oblique_basis()` in favor of `view_frame()`
- Fixed theta labeling on `view_manip_space()`
- Examples improved (simple case, adding more and more complexity) through-out 
- Minor code, code comments, spelling, and grammar clean up through-out


# spinifex v0.2.5

## New utility

- theme_spinifex(), a ggplot theme, slightly lighter than theme_minimal().

## Other changes

- Aesthetic arguments, which are now passed directly into `geom_point(aes(...))` instead of hard-coded arguments
- Removed 'col' (color), 'pch' (point character), 'cex' (point size) and 'alpha' (transparency) from rendering functions. These options are now passed directly into the elipsis, `ggplot2::geom_point(...)`
- Added wrapper for `gganimate::anim_save()` in `render_gganimate()` 
- Added wrapper for `htmlwidgets::saveWidget()` in `render_plotly()`
- Clarified and more consistent documentation


# spinifex v0.2.0

## Demo shiny app

- `run_app("radial_tour")`

## New functions
To accommodate interactive use, rather than predefined paths:

- `oblique_basis()`
- `oblique_frame()`
- `run_app()` New shiny app!

## New utility functions

- `map_absolute()`
- `is_orthonormal()` slightly more general than `tourr::is_orthonormal()`

## Other changes

- Added 'cex' (point size) and 'alpha' transparency arguments to rendering functions
- Argument naming and order consistency
- Clarified and more consistent documentation
- More defensive coding


# spinifex v0.1.0

Initial submission to CRAN. Vignette: "spinifex"

## Primary functions

-` play_tour_path()`
- `play_manual_tour()`

## Primary utility functions

- `view_basis()`
- `view_manip_space()`
