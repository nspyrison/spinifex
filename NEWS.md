# spinifex 0.2.0

## Demo shiny app: 

- run_app("intro")

## New functions:
To accommodate interactive use, rather than predefined paths.

- oblique_basis()
- oblique_frame()
- run_app()

Minor util/internally aimed functions:

- pan_zoom
- is_orthonormal()

## Other changes:

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

## Primary util functions

- view_basis()
- view_manip_space()
