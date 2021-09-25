program weather_stats_parallel
   use mod_parallel, only: tile_indices
   implicit none
   print *, tile_indices(15)
end program weather_stats_parallel
