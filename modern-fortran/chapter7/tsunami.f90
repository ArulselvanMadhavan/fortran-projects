program tsunami
   use mod_parallel, only: tile_neighbors, tile_indices
   use iso_fortran_env, only: int32, real32
   use mod_diff, only: diff => diff_centered
   implicit none
   integer(int32), parameter :: grid_size = 100
   integer(int32), parameter :: num_time_steps = 5000
   real(real32), parameter :: dt = 0.02 !time step
   real(real32), parameter :: dx = 1    !grid spacing
   real(real32), parameter :: g = 9.8   !gravitational acc
   real(real32), allocatable :: h(:) [:], u(:) [:], gather(:) [:], hmean(:)
   integer(int32), parameter :: ipos = 25
   real(real32), parameter :: decay = 0.02
   integer(int32) :: indices(2), neighbors(2)
   integer(int32) :: left, right, is, ie, ils, ile, ims, ime, tile_size
   integer(int32) :: i,n
   ! Validations
   if (mod(grid_size, num_images()) /= 0) then
      error stop 'grid size must be divisible by num_images'
   end if

   print *, 'Image:', this_image()
   neighbors = tile_neighbors()
   left = neighbors(1)
   right = neighbors(2)
   
   indices = tile_indices(grid_size)
   is = indices(1)
   ie = indices(2)

   tile_size = grid_size/num_images()
   ils = 1
   ile = tile_size

   ims = ils - 1
   ime = ile + 1

   allocate (h(ims:ime) [*])
   allocate (u(ims:ime) [*])
   allocate (hmean(ims:ime))
   allocate (gather(grid_size) [*])
   print *, is, ie, ils, ile, ims, ime

   ! setup data
   do i = is - 1, ie + 1
      h(i - is + 1) = exp(-decay*(i - ipos)**2)
   end do

   u = 0
   hmean = 10

   gather(is:ie) [1] = h(ils:ile)
   sync all
   if(this_image() == 1) then
      print *, gather
   end if

   time_loop: do n = 1, num_time_steps
      h(ime)[left] = h(ils)
      h(ims)[right] = h(ile)
      sync all

      ! Compute u
      u = u - (u * diff(u) + g * diff(h)) / dx * dt
      sync all

      u(ime)[left] = u(ils)
      u(ims)[right] = u(ile)
      sync all
      h = h - diff(u * (hmean +h))/dx * dt

      gather(is:ie)[1] = h(ils:ile)
      sync all
      if(this_image()==1) print *, n, gather
      end do time_loop
   
end program tsunami

