module mod_parallel
   implicit none
   private
   public :: tile_indices

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
end module mod_parallel
