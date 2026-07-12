program implied_do_loops44
    implicit none
    integer :: i
    character(len=256), allocatable :: manual(:)
    character(len=100) :: out_str
    
    allocate(manual(3))
    manual = ['a', 'b', 'c']
    
    write(out_str, '(3(g0))') ( [character(len=80/3) :: manual(i)], i=1, size(manual) )
    
    if (trim(out_str) /= "a                         b                         c") then
        print *, "Expected: 'a                         b                         c'"
        print *, "Got: '", trim(out_str), "'"
        error stop
    end if
end program