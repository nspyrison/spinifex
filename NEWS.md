# !!TODO:!! 

- ADD size, height, width, and resolution, show_legend arguments to play_manual_tour
- add gganimate::anim_save() to example code

# !!TODO end!!



# spinifex 0.1.5

## Intoduce shiny apps: 

- run_app("intro")
- run_app("primary")

## New functions to accomadate interactive use, rather than predefined paths:

- oblique_basis()
- oblique_frame()

## Other changes:

- argument naming and order consistency
- address issue with vignette erroring on some opperating systems
- ggrepel for gganimate and shiny displays (not supported in plotly)
- more defensive coding
- local is_orthonormal() overwrite that works correctly on identity matrices


# spinifex 0.1.0

Initial submission to CRAN. Vignette: "spinifex"

## Primary functions

- play_tour_path()
- play_manual_tour()

## Primary util functions

- view_basis()
- view_manip_space()
