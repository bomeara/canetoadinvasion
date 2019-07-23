my_plan <- drake_plan(
  locations = GetCaneToadLocations(),
  elevations = GetAustraliaElevations(),
  my_plot = RenderAustraliaMap(elevations),
  my_plot_with_points = AddLocations(elevations, locations)
)
