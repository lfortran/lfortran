module template_array_m

    implicit none
    private

    requirement r(t)
        type :: t; end type
    end requirement

    template array_tmpl(t)
        requires r(t)
        private
        public :: insert_t
    contains
        function insert_t(lst, i) result(r)
            type(t), intent(in) :: lst(:), i
            type(t) :: r
            !lst(1) = i
            !r = lst(1)
        end function
    end template

contains

    !subroutine test_template()
    !    instantiate array_tmpl(integer), only: insert_int => insert_t
    !    integer :: a(1), i
    !    a(1) = 0
    !    i = 1
    !    print *, a(1)
    !    call insert_int(a, i)
    !    print *, a(1)
    !end subroutine

end module

program template_array

    use template_array_m
    implicit none

    !call test_template()

end