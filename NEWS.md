# spinifex v0.3.4

- Cleaned up both vignettes; include plotly output & removed scroll bars from wide code chunks
- Removed forced garbage collection `gc()` calls, too expensive for the minor issues it tried to mitigate
- `proto_density()` now changes the aspect ratio to 1/2 (y/x), twice as wide
- `animate_plotly()` will change the x scaleratio to 2 (for non `plotly::subplot` animations)
- Changed defaults to `map_relative()`; position = "left" or "right" is fully off outside of the data and slightly smaller
- Adjust text-related `proto_*()` default; decreasing the text size (5 -> 4) 
- Adjust line segment-related `proto_*()` default; decreasing the line size a (1 -> .6)
- Adjust `filmstrip & facet_wrap_tour()`: adds theme for borders to help distinguish facets
- Adjust `theme_spinifex()` to include shading and strip outline consistent with the above point


# spinifex v0.3.3

- Add do_center_frame = TRUE argument to `ggtour()`, such that tours don't want too far to the sides as they were in cheem radial tours
- Adjust `theme_spinifex()`: tightening margins, facet strip outlines, and better separating theme elements from ggtour default settings


# spinifex v0.3.2

- New function: `facet_wrap_tour()`, for faceting tours
- New function: `proto_frame_cor2()`, adds text for the within-frame correlation squared
- New function: `append_fixed_y()`, add/overwrite data y column to fixed values, such as for the height of predicted values or residuals of a model
- New function: `draw_basis()` static ggplot2 variant of `proto_basis`, that accepts a basis directly without requiring ggtour initialization
- Most data-oriented `proto_*` functions have `row_index` argument; allowing for subsetting that is compatible with faceting and appending a fixed y! By default, `proto_point()` will plot non-selected points in faint grey, behind selected points
- `theme_spinifex()` changed removing the duplicate legend display, fewer warnings, and less frame oddities (geom existence issues) with `animate_plotly`
- Better examples for setting dimensions, resolution, and renders in `animate_*` functions
- Refreshed readme


# spinifex v0.3.1

- Fixed 'phi issue' where manual tours were not consistent in their initial direction, now always move toward full contribution before 0 contribution.
- Interpolation of manual tours now handled in `ggtour()` synchronizes `angle` usage with `{tourr}` tours
- `manual_tour()` and related functions now handle 1D projections
- New functions: `proto_highlight/1d()` for highlighting specific points in ggtours
- New function: `filmstrip()`, creates a ggplot faceting the frames of a ggtour for a static output
- New dataset: `penguins_na.rm`, from `palmerpenguins::penguins`, removed NA
rows & reordered columns
- Added argument `rownum_index` to `proto_text`, for labeling subsets
- Experimental wrapper function `spinifex::save_history`, muting the noisy execution of `tourr::save_history`


# spinifex v0.3.0

- New ggproto api for visuals, see `?ggplot_tour()` to get started!!
-- 10x new `proto_*` functions, essentially `geoms_*` for animated tours: 
basis/1d, point, origin/1d, density, text, hex, default/1d.
-- 2x animation funcs; `animate_plotly()` and `aniamte_gganimate()`
-- 2x internal utility functions
- New vignette demonstrating this api `vignette("ggproto_api", "spinifex")`
- Shiny app now imports .csv & .rds,  code improved, uses new ggproto api
-- `run
- Fixed a format issue with the `BeastCancer_na.rm` dataset


# spinifex v0.2.8

- Vectorized all `for` loops
- `manip_var_of(basis)` suggests a variable to used based on the rank of the contributions of the basis.

Basis_* functions to find features of interest, powered by `{Rdimtools}`.
- `basis_olda()`
- `basis_odp()`
- `basis_onpp()`
- `basis_olpp()`

New util functions:
- `scale_sd()` center and scale each variable by it's standard deviation
- `scale_01()` center and scale each variable to be between [0, 1]
- `basis_half_circle()`, variable agnostic basis with minimal variable dependence
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


# spinifex v0.2.0!

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
