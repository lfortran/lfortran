! TODO: Add assert and print something
module module_1
    implicit none
contains
subroutine sub_1(a, b, c, d, e, f)
    integer, intent(in) :: a
    integer, intent(in) :: b
    integer, intent(in) :: c
    integer, intent(in), optional :: d
    integer, intent(in), optional :: e
    integer, intent(in) :: f
end subroutine sub_1

end module module_1

module module_2
    use module_1, only: sub_1
contains

    subroutine sub_2()
        call sub_1(1, 2, 3, d=1, e=1, f=2)
        ! call sub_1(1, d=2)
    end subroutine sub_2
end module module_2

