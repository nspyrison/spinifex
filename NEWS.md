# spinifex 0.2.6

- New argument, `ggproto`, accepts a list of `ggplot2` objects for more control over visual output
- New argument, `aes_args`, accepts a list of arguments to pass into the aes() call within geom_point(). This is used for variable mappings, such as color, but needs to be qualified as this is standard evaluation
- New argument, `identity_args`, accepts a list of arguments to pass call within geom_point(), but outside of aes(). This is used for scalar options or manually mapped vector such as point size or alpha
- New datasets: PimaIndiansDiabetes_long & PimaIndiansDiabetes_wide
- Fixed the centering of axes across frames
- Fixed `scale_axes()` to automatically scale to data rather than assume the data is scaled to [0, 1]
- Changed notation from "slide(s)" to "frame(s)" through-out
- Renamed `oblique_frame()` to `view_frame()`, `oblique_frame()` will be phased out later
- Renamed `oblique_basis()` to `print_basis()`, `oblique_basis()` will be phased out later
- Depricating `view_basis()` in favor of `view_frame/oblique_frame`
- Fixed theta labeling on `view_manip_space()`
- Examples improved (simple case, adding more and more complexity) through-out 
- Minor code, code comments, spelling, and grammar clean up through-out


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
