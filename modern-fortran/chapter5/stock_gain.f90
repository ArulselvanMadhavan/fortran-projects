program stock_gain
  use mod_io, only:num_records
  use mod_array, only:reverse
   implicit none

   character(len=4), allocatable :: symbols(:)
   character(len=:), allocatable :: time(:)
   real, allocatable :: open (:), high(:), low(:), close (:), adjclose(:), volume(:)
   integer::n
   real :: gain
   symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ', 'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']
   print *, "---------------------------------------"
   do n = 1, size(symbols)
      call read_stock('data/'//trim(symbols(n))//'.csv', time, open, high, low, close, adjclose, volume, gain)
      print *, symbols(n), gain, nint(gain / adjclose(1) * 100)
   end do

contains
   subroutine read_stock(file_name, time, open, high, low, close, adjclose, volume, gain)
      character(*), intent(in) :: file_name
      character(:), allocatable, intent(in out) :: time(:)
      real, allocatable, intent(in out)::open (:), high(:),low(:), close (:), adjclose(:), volume(:)
      integer :: n,nm,fileunit
      real, intent(in out) :: gain
      nm = num_records(file_name) - 1

      if(allocated(time)) deallocate(time)
      allocate(character(10) :: time(nm))
      call alloc(open,nm)
      call alloc(high, nm)
      call alloc(low, nm)
      call alloc(close, nm)
      call alloc(adjclose, nm)
      call alloc(volume, nm)

      open(newunit=fileunit,file = file_name)
      read(fileunit, fmt=*, end=1)
      do n = 1, nm
         read(fileunit, fmt=*,end=1) time(n), open(n), high(n), low(n), close(n), adjclose(n), volume(n)
      end do
      
      adjclose = reverse(adjclose)
      gain = (adjclose(size(adjclose)) - adjclose(1))

      1 close(fileunit)
   end subroutine read_stock

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
end program stock_gain

