program stock_crossover
  use mod_array, only: crosspos, crossneg,reverse
    implicit none

  integer, allocatable :: buy(:), sell(:)  
  character(len=4), allocatable :: symbols(:)
  character(len=:), allocatable :: time(:)
  real, allocatable :: open(:), high(:), low(:), close(:), adjclose(:), volume(:)
  integer :: i, im, n, fileunit
  
  symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ', 'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']


  do n = 1, size(symbols)
     ! time = time(size(time):1, -1)
     adjclose = reverse(adjclose)

     buy = crosspos(adjclose, 30)
     sell = crossneg(adjclose, 30)

     open(newunit = fileunit, file=trim(symbols(n)) // '_crossover.txt')

     do i = 1, size(buy)
        write(fileunit, fmt=*) 'Buy ', time(buy(i))
     end do
     
     do i = 1, size(sell)
        write(fileunit,fmt=*) 'Sell ', time(sell(i))
     end do

     close(fileunit)
  end do
  
end program stock_crossover
