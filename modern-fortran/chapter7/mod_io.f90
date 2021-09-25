module mod_io
   implicit none
   private
   public :: read_buoy

contains

   integer function num_records(filename)
      ! Return the number of records (lines) of a text file.
      character(len=*), intent(in) :: filename
      integer :: fileunit
      open (newunit=fileunit, file=filename)
      num_records = 0
      do
         read (unit=fileunit, fmt=*, end=1)
         num_records = num_records + 1
      end do
1     continue
      close (unit=fileunit)
   end function num_records

   subroutine read_buoy(fname, time, wspeed)
      character(len=*), intent(in) :: fname
      character(len=20), allocatable, intent(in out) :: time(:)
      real, allocatable, intent(in out) :: wspeed(:)
      integer :: fileunit
      integer :: n, nm
      if (allocated(time)) deallocate (time)
      if (allocated(wspeed)) deallocate (wspeed)
      nm = num_records(fname)
      allocate (time(nm), wspeed(nm))
      open (newunit=fileunit, file=fname)
      do n = 1, nm
         read (unit=fileunit, fmt=*, end=1) time(n), wspeed(n)
      end do
1     continue
      close (fileunit)
   end subroutine read_buoy

end module mod_io

