# 5 geometry operations----
pacman::p_load(sf, terra, dplyr, spData)

# * 5.1 Introduction----
# simplify and convert vector geometries
# unary operations work on a single geometry:
# simplification of lines, buffers, centroids, scaling geometries
# binary operations modify one geometry based on another, like clipping
# or geometry unions
# 
# 