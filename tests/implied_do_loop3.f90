! Test implied do loops + call to intrinsic
module implied_do_loops3_mod
    implicit none

    contains 

    subroutine test_1
        character(len=20) :: tmp_line(3)
        integer :: iii, ii
        tmp_line = "Hello"
        ii = 3
        write(*, '(g0)') (trim(tmp_line(iii)), iii=1, ii)
    end subroutine
    
    subroutine test_2
        implicit none
        character, allocatable :: tmp_line
        integer :: i
        tmp_line = "Hello"
        print *, (tmp_line, i=1, 3)
    end subroutine
    
    subroutine test_3
        character(len=20) :: tmp_line
        integer :: iii, ii
        tmp_line = "Hello"
        ii = 3
        write(*, '(g0)') (trim(tmp_line), iii=1, ii)
    end subroutine
    
end module


program implied_do_loops3
    use implied_do_loops3_mod
    implicit none
    call test_1()   
    call test_2()   
    call test_3()   
end program 