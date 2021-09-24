module mod_array
   use mod_alloc, only: alloc
   implicit none
   private
   public :: reverse, average, moving_average, std, moving_std, crosspos, crossneg

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

    pure function crosspos(x, w) result(res)
      real, intent(in) :: x(:)
      integer, intent(in) :: w
      integer, allocatable :: res(:)
      real, allocatable :: xavg(:)
      logical, allocatable :: greater(:), smaller(:)
      integer :: i
      res = [(i, i = 2, size(x))]
      xavg = moving_average(x, w)
      greater = x > xavg
      smaller = x < xavg
      res = pack(res, greater(2:) .and. smaller(:size(x)-1))
    end function crosspos

    pure function crossneg(x, w)
      real, intent(in) :: x(:)
      integer, intent(in) :: w
      integer, allocatable :: crossneg(:)
      real, allocatable :: xavg(:)
      logical, allocatable :: greater(:), smaller(:)
      integer::i
      xavg = moving_average(x, w)
      greater = x > xavg
      smaller = x < xavg
      crossneg = [(i, i = 2, size(x))]
      crossneg = pack(crossneg, greater(:size(x)-1) .and. smaller(2:))
    end function crossneg
    

end module mod_array

