module mod_arrays
   implicit none
   private
   public :: denan, mean
contains
   pure function denan(x)
      ! Exclude Nans
      use ieee_arithmetic, only: ieee_is_nan
      real, allocatable, intent(in) :: x(:)
      real, allocatable :: denan(:)
      denan = pack(x,.not. ieee_is_nan(x))
   end function denan

   pure real function mean(x)
      real, allocatable, intent(in)::x(:)
      mean = sum(x)/size(x)
   end function mean

end module mod_arrays

