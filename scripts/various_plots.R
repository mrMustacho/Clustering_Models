### 3d plots
library(rgl)


data(volcano)

z <- 2 * volcano        # Exaggerate the relief

x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)

zlim <- range(z)
zlen <- zlim[2] - zlim[1] + 1

colorlut <- terrain.colors(zlen) # height color lookup table

col <- colorlut[ z - zlim[1] + 1 ] # assign colors to heights for each point

open3d()
surface3d(x, y, z, color = col, back = "lines")

## density 3d plot
library(MASS)
library(plotly)
x <- iris[,1]
y <- iris[,2]
den3d <- kde2d(x, y)
plot_ly(x=den3d$x, y=den3d$y, z=den3d$z) %>% add_surface()

### with centour
library(MASS)
library(plotly)
fig <- plot_ly(z = ~volcano) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)
fig <- fig %>% layout(
  scene = list(
    camera=list(
      eye = list(x=1.87, y=0.88, z=-0.64)
    )
  )
)

fig
