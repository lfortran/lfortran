! Test while's test statement and how we handle any expression
! inside that needs a temporary

program while_05
    integer :: x = 2
    integer :: hit = 0

    ! Predicate with some array operations in it
    do while(boo(foo(x) == ["Hello","World"])) 
      print *, "Hi"
      x = 1
      hit = hit + 1
    end do
    
    if(hit /= 1) error stop

    contains 
    function foo(i) result (r)
        integer :: I
        character(5):: r(2)
        if(i == 1) then
            r = ["bla","bla"]
        else 
            r = ["Hello","World"]
        end  if
    end function 

    function boo(arr) result (r_logical)
        logical ::r_logical
        logical :: arr(2)
        if(all(arr.eqv. [.true.,.true.])) then
            r_logical = .true.
        else 
            r_logical = .false.
        end if
    end function


end program