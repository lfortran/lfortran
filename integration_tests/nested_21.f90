! Check sync statements in do while loops 
! at cycle statements for scalar and arrays
module temp

contains
  subroutine demo()
    implicit none
    integer :: a,c
    integer :: arr(2)
    a = 1
    ! Variable c is used to not run loops indefinitely
    c = 1
    ! Scalar checks
    do while(f())
        if (c>10) error stop
        c = c + 1
        if (a < 5) then 
          a = a + 1
          cycle
        end if
        if (a > 4) error stop
    end do
    print *, a
    if (a < 4) error stop

    ! Array Checks
    c = 1
    arr(2) = 1
    do while(g())
        if (c>10) error stop
        c = c + 1
        if (arr(2) < 5) then 
          arr(2) = arr(2) + 1
          cycle
        end if
        if (arr(2) > 4) error stop
    end do
    print *, arr(2)
    if (arr(2) < 4) error stop

  contains
    logical function f()
      print *, a
      f = (a < 4) 
    end function f

    logical function g()
      print *, arr(2)
      g = (arr(2) < 4) 
    end function g
    
  end subroutine demo 
end module

program nested_21
  use temp
  implicit none
  call demo()
end program nested_21