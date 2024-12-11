module external_10_module
    implicit none
    integer :: g = 2
end module

subroutine sub1_external_10
    use external_10_module
    implicit none
    call sub2_external_10 (g)
    print *, g
    if (g /= 10) error stop
end subroutine

subroutine sub2_external_10 (x)
    implicit none
    integer, intent(inout) :: x
    print *, x
    if (x /= 2) error stop
    x = 10
end subroutine

program external_10
    implicit none
    call sub1_external_10
end program
