program coarrays
   implicit none

   integer :: a[*]

   if (num_images() /= 2) &
      error stop 'Need to be run on two images'

   a = 0

   if (this_image() == 1) then
      a = 1
      print *, 'image#1 has value', a
      a[2] = 2*a
   end if

   sync all

   if (this_image() == 2) then
      print *, 'image#2 has value', a
      a[1] = 2*a
   end if

   sync all

   if (this_image() == 2) then
      print *, 'image 2 sees that image 1 has value', a[1]
   end if

end program coarrays
