module mod_parallel
   implicit none
   private
   public :: tile_indices, tile_neighbors
   ! use iso_fortran_env, only: int32
contains
   pure function tile_indices(n)
      integer, intent(in) :: n
      integer :: tile_indices(2)
      integer :: quotient, rem, e, s, idx, num, offset

      rem = mod(n, num_images())
      quotient = n/num_images()
      idx = this_image()

      s = (idx - 1)*quotient
      e = idx*quotient
      tile_indices(1) = s + 1
      tile_indices(2) = e

      offset = num_images() - rem

      if (idx > offset) then
         tile_indices(1) = s + idx - offset
         tile_indices(2) = e + idx - offset
      end if

   end function tile_indices

   pure function tile_neighbors()
      integer :: tile_neighbors(2)
      if (this_image() == num_images()) then
         tile_neighbors(1) = this_image() - 1
         tile_neighbors(2) = 1
      else if (this_image() == 1) then
         tile_neighbors(1) = num_images()
         tile_neighbors(2) = this_image() + 1
      else
         tile_neighbors(1) = this_image() - 1
         tile_neighbors(2) = this_image() + 1
      end if

   end function tile_neighbors

end module mod_parallel
