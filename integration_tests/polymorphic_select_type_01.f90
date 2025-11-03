program main
    type :: type_a
       integer :: a
    end type type_a

    class(type_a), allocatable :: x
    character(100) :: ss

    allocate(x)
    x%a = 10

    select type(x)
     type is (type_a)
       print *, x
       write(ss, "(i0)") x
       if (trim(ss) /= "10") error stop "FAILED: wrong value from write"
     class default
       error stop "FAILED: wrong type"
    end select
end program