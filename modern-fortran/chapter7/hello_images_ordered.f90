program hello_images_ordered
   implicit none
   integer :: n

   do n = 1, num_images()
      if (this_image() == n) then
         print *, 'Image:', n
      end if
      sync all
   end do

end program hello_images_ordered
