program string_103
! Test that a temporary variable created for a function call result
! whose type depends on another variable (e.g., character(len=len(x)))
! has the correct dependency list in ASR.
implicit none
character(len=:), allocatable :: temp
character(len=:), allocatable :: rev
integer :: i
temp = "hello/world"
rev = reverse(temp)
i = find_char(rev, "/")
if (i /= 6) error stop
! The following nested call triggered an ASR verify error before the fix:
! "Variable lfortran_tmp depends on temp but isn't found in its dependency list"
i = find_char(reverse(temp), "/")
print *, "ok"
contains
    function reverse(string) result(reverse_string)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: reverse_string
        integer :: i, n
        n = len(string)
        do i = 1, n
            reverse_string(n-i+1:n-i+1) = string(i:i)
        end do
    end function

    function find_char(string, pattern) result(res)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: pattern
        integer :: res
        integer :: i
        res = 0
        do i = 1, len(string)
            if (string(i:i) == pattern(1:1)) then
                res = i
                return
            end if
        end do
    end function
end program
