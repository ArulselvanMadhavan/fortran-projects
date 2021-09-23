module mod_array
   implicit none
   private
   public :: reverse

contains
  pure function reverse(x)
    real,intent(in)::x(:)
    real :: reverse(size(x))
    reverse = x(size(x):1:-1)
   end function reverse
end module mod_array

