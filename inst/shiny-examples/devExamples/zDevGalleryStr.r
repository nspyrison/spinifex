# as.matrix(gallery_disp()[4:(4 + 2*p() - 1)], ncol= 2, byrow=F)
# foo <- data.frame("type A", "class B", 1,2,3,4,5,6)
# foo2 <- gather(foo, key = "var", value = "X", -c(X.type.A.,X.class.B., X4, X5,X6))
# foo3 <- gather(foo2, key = "var", value = "Y", -c(X.type.A.,X.class.B., var, X))
# ## Cartisian product,  go back to generation and store basis in each row.

mat <- matrix(1:4, ncol=2)
foo <- data.frame(manip_type=c("type A", "type C"), manip_var=c(4,6))
foo2 <- data.frame(id=seq.int(nrow(foo)), foo, 
                   basis = I(replicate(nrow(foo), mat, simplify=FALSE)))
foo2

### icons
# f <- foo2[, -4]
# oo2 <- foo2[, 4]
foo3 <- NULL
for (i in 1:nrow(foo2)){
  rows <- cbind(foo2[i, 1:3], foo2[,4][[i]])
  foo3 <- rbind(foo3, rows)
}
colnames(foo3) <- c("id", "manip_type", "manip_var", "x", "y")

angle <- seq(0, 2 * pi, length = 360)
circ  <- data.frame(x = cos(angle), y = sin(angle))
.colnames <- colnames(flea[,1:6])

ggplot2::ggplot() + 
  ggplot2::scale_color_brewer(palette = "Dark2") +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_fixed(ratio = 1 / length(unique(foo3$id))) +
  ## Cirle path
  ggplot2::geom_path(data = circ, 
                     mapping = ggplot2::aes(x = x, y = y),
                     color = "grey80", size = .3, inherit.aes = F) +
  ## Basis axes line segments
  ggplot2::geom_segment(data = foo3, 
                        mapping = ggplot2::aes(x = x, y = y, xend = 0, yend = 0)) +
  ## Basis variable text labels
  ggplot2::geom_text(data = foo3, 
                     mapping = ggplot2::aes(x = x, y = y, label = .colnames[manip_var]),
                     size = 4, hjust = 0, vjust = 0, colour = "black") +
  ## manip_type label
  ggplot2::geom_text(data = foo3, 
                     mapping = ggplot2::aes(x = -1, y = -1, label = manip_type),
                     size = 4, hjust = 0, vjust = 0, colour = "black") + 
  ## facet
  ggplot2::facet_grid(rows = foo3$id)
