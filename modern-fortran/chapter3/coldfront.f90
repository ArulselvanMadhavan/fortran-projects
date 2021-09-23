! program cold_front
!    implicit none

!    real :: temp1 = 12, temp2 = 24
!    real :: dx = 960, c = 20, dt = 24
!    real :: res ! result in deg.C

!    res = temp2 - c*(temp2 - temp1)/dx*dt

!    print *, 'Temperature after ', dt, &
!         'hours is ', res, 'degrees'
   
! end program cold_front

program cold_front
  implicit none
  real :: nhours
  integer :: n
  do n = 6,48,6                 ! start with 6. end with 48. step with 6
     nhours = real(n)
     print *, 'Temperature after ', &
          nhours, ' hours is ', &
          cf_temp(12., 24.,20.,960.,nhours), ' degrees'
  end do

contains
  real function cf_temp(temp1, temp2, c, dx, dt) result(res)
    real,intent(in)::temp1,temp2,c,dx,dt
    res = temp2 - c * (temp2 - temp1) / dx * dt
  end function cf_temp
  
end program cold_front
