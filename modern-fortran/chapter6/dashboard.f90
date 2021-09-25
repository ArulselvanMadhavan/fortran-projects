program dashboard
   use iso_fortran_env, only: dash => output_unit
   implicit none
   real, parameter :: lat = 59.123456, lon = 18.123456, alt = 11000.1
   integer, parameter :: eng(4) = [95, 96, 95, 97]
   logical, parameter :: airborne = .true.
   character(len=:), allocatable :: dashfmt

   dashfmt = '(2(f9.5, 2x), f7.1, 2x, 4(i3.3, 2x), l)'
   write (dash, dashfmt) lat, lon, alt, eng, airborne
end program dashboard

