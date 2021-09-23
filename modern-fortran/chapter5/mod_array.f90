module mod_array
   use mod_alloc, only: alloc
   implicit none
   private
   public :: reverse, average, moving_average, std, moving_std

contains
   pure function reverse(x)
      real, intent(in)::x(:)
      real :: reverse(size(x))
      reverse = x(size(x):1:-1)
   end function reverse

   pure function average(x)
      real, intent(in)::x(:)
      real :: average
      average = sum(x)/size(x)
   end function average

   pure function std(x)
      real, intent(in)::x(:)
      real :: std
      std = sqrt(average((x - average(x))**2))
   end function std

   pure function moving_average(x, w)
      real, intent(in) :: x(:)
      real, allocatable :: moving_average(:)
      integer, intent(in) :: w
      integer :: i, n
      n = size(x)
      call alloc(moving_average, n)
      do i = 1, n
         moving_average(i) = average(x(max(i - w, 1):i))
      end do

   end function moving_average

   pure function moving_std(x, w)
      real, intent(in) :: x(:)
      real, allocatable :: moving_std(:)
      integer, intent(in) :: w
      integer :: i, n, i1
      n = size(x)
      call alloc(moving_std, n)
      do i = 1, n
         i1 = max(i - w, 1)
         moving_std(i) = std(x(i1:i))
      end do

   end function moving_std

end module mod_array

