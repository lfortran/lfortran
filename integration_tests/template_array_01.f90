module template_array_01_m

    implicit none
    private
    public :: test_template

    requirement r(t)
        type, deferred :: t
    end requirement

    template array_tmpl(t)
        require :: r(t)
        private
        public :: insert_t
    contains
        function insert_t(lst, i) result(r)
            type(t), intent(in) :: lst(:), i
            type(t) :: r
            lst(1) = i
            r = lst(1)
        end function

        function insert_t_n(n, lst, i) result(r)
            integer, intent(in) :: n
            type(t), intent(in) :: lst(n), i
            type(t) :: r
            lst(1) = i
            r = lst(1) 
        end function
    end template

contains

    subroutine test_template()
        instantiate array_tmpl(integer), only: insert_int => insert_t, insert_int_n => insert_t_n
        integer :: a(1), i, r
        a(1) = 0
        i = 1
        print *, a(1)
        r = insert_int(a, i)
        print *, a(1)

        a(1) = 0
        print *, a(1)
        r = insert_int_n(size(a), a, i)
        print *, a(1)
    end subroutine

end module

program template_array_01

    use template_array_01_m
    implicit none

    call test_template()

end
