program stock_gain
   use mod_io, only: num_records, read_stock
   use mod_array, only: reverse
   use mod_alloc, only: alloc, free
   implicit none

   character(len=4), allocatable :: symbols(:)
   character(len=:), allocatable :: time(:)
   real, allocatable :: open (:), high(:), low(:), close (:), adjclose(:), volume(:)
   integer::n
   real :: gain
   symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ', 'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']
   print *, "---------------------------------------"
   do n = 1, size(symbols)
      call read_stock('data/'//trim(symbols(n))//'.csv', time, open, high, low, close, adjclose, volume)
            adjclose = reverse(adjclose)
      gain = (adjclose(size(adjclose)) - adjclose(1))

      print *, symbols(n), gain, nint(gain/adjclose(1)*100)
   end do
end program stock_gain

