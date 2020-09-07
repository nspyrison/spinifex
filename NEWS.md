# spinifex 0.2.6

- New argument `ggproto` acepts a list of `ggplot2` objects for more control over visual output
- New datasets: PimaIndiansDiabetes_long & PimaIndiansDiabetes_wide
- changed notation from slide(s) to frame(s) through-out
-
- Depricating `view_basis()` in favor of `view_frame/oblique_frame`
- Fixed theta labeling on `view_manip_space()`
- Minor code clean-up
- Minor example clean-up
- Minor export fixes


# spinifex 0.2.5

## New utility

- theme_spinifex(), a ggplot theme, slightly lighter than theme_minimal().

## Other changes

- Aesthetic arguments, which are now passed directly into `geom_point(aes(...))` instead of hard-coded arguments
- Removed 'col' (color), 'pch' (point character), 'cex' (point size) and 'alpha' (transparency) from rendering functions. These options are now passed directly into `ggplot2::geom_point(...)`
- Added wrapper for `gganimate::anim_save()` in `render_gganimate()` 
- Added wrapper for ``htmlwidgets::saveWidget()` in `render_plotly()`
- Clarified and more consistent documentation


# spinifex 0.2.0

## Demo shiny app

- run_app("intro")

## New functions
To accommodate interactive use, rather than predefined paths:

- oblique_basis()
- oblique_frame()
- run_app()

## New utility & internally-aimed functions

- pan_zoom
- is_orthonormal()

## Other changes

- Added 'cex' (point size) and 'alpha' transparency arguments to rendering functions
- argument naming and order consistency
- clarified and more consistent documentation
- more defensive coding
- temporary use of spinifex::is_orthonormal(), more general than tourr 0.5.6 is_orthonormal()


# spinifex 0.1.0

Initial submission to CRAN. Vignette: "spinifex"

## Primary functions

- play_tour_path()
- play_manual_tour()

## Primary utility functions

- view_basis()
- view_manip_space()
