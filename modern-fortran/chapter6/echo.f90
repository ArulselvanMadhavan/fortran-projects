program echo_robot
   implicit none
   character(len=1000) :: text
   read *, text
   print *, trim(text)
end program echo_robot
