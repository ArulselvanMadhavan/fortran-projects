module mod_alloc
   implicit none
contains

   pure subroutine alloc(a, size)
      real, allocatable, intent(in out)::a(:)
      integer, intent(in)::size
      integer :: stat
      character(100) err

      if (allocated(a)) call free(a)
      allocate (a(size), stat=stat, errmsg=err)
      if (stat > 0) error stop err
   end subroutine alloc

   pure subroutine free(a)
      real, allocatable, intent(in out)::a(:)
      integer :: stat
      character(100) err
      if (allocated(a)) deallocate (a, stat=stat, errmsg=err)
      if (stat > 0) error stop err
   end subroutine free
end module mod_alloc

