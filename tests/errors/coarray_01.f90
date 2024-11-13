program test
    integer :: val
    val = this_image ()
    call co_sum (val, result_image=1)
    if (this_image() == 1) then
      write(*,*) "The sum is ", val ! prints (n**2 + n)/2,
                                    ! with n = num_images()
    end if
    print *, "Hello, World!"
end program test