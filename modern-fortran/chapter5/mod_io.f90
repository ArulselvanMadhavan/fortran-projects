module mod_io
  use mod_alloc, only:alloc
  use mod_array, only:reverse
   implicit none
   private
   public :: num_records, read_stock, write_stock

contains

   integer function num_records(filename)
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

   subroutine read_stock(file_name, time, open, high, low, close, adjclose, volume)
      character(*), intent(in) :: file_name
      character(:), allocatable, intent(in out) :: time(:)
      real, allocatable, intent(in out)::open (:), high(:), low(:), close (:), adjclose(:), volume(:)
      integer :: n, nm, fileunit

      nm = num_records(file_name) - 1

      if (allocated(time)) deallocate (time)
      allocate (character(10) :: time(nm))
      call alloc(open, nm)
      call alloc(high, nm)
      call alloc(low, nm)
      call alloc(close, nm)
      call alloc(adjclose, nm)
      call alloc(volume, nm)

      open (newunit=fileunit, file=file_name)
      read (fileunit, fmt=*, end=1)
      do n = 1, nm
         read (fileunit, fmt=*, end=1) time(n), open (n), high(n), low(n), close (n), adjclose(n), volume(n)
      end do

1     close (fileunit)
   end subroutine read_stock

  subroutine write_stock(filename, time, price, mvavg, mvstd)
    ! Write derived stock data to file.
    character(len=*), intent(in) :: filename
    character(len=:), allocatable, intent(in) :: time(:)
    real, intent(in) :: price(:), mvavg(:), mvstd(:)
    integer :: fileunit, n
    open(newunit=fileunit, file=filename)
    do n = 1, size(time)
      write(fileunit, fmt=*) time(n), price(n), mvavg(n), mvstd(n)
    end do
    close(fileunit)
  end subroutine write_stock    
end module mod_io

