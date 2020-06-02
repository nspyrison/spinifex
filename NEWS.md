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

- adress issue with vignette erroring on some opperating systems
- argument name and order consistency
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
