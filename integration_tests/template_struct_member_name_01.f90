! Members of a template-local derived type (type-bound procedures and
! components) must not be replaced by a same-named template procedure that
! was instantiated earlier (#13525).
module template_struct_member_name_01_m
    implicit none
    template tm {}
        type :: pair
            integer :: k = 0
        contains
            procedure :: store
            procedure :: n
        end type
        type :: counter
            integer :: n = 0
        contains
            procedure :: store => count_steps
        end type
    contains
        subroutine store(this)
            class(pair), intent(inout) :: this
            this%k = 5
        end subroutine
        subroutine n(this)
            class(pair), intent(inout) :: this
            this%k = 9
        end subroutine
        subroutine count_steps(this)
            class(counter), intent(inout) :: this
            this%n = this%n + 17
        end subroutine
        subroutine run(c, p)
            type(counter), intent(inout) :: c
            type(pair), intent(inout) :: p
            call c%store()
            c%n = c%n + 1
            call p%store()
        end subroutine
    end template
end module

program template_struct_member_name_01
    use template_struct_member_name_01_m
    implicit none
    instantiate tm {}, only: pair, counter, run
    type(counter) :: c
    type(pair) :: p
    call run(c, p)
    if (c%n /= 18) error stop
    if (p%k /= 5) error stop
    call p%n()
    if (p%k /= 9) error stop
    print *, c%n, p%k
end program
