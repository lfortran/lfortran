module template_class_procedure_nopass_01_m
    implicit none
    template tm {t}
        deferred type :: t
        ! The binding keeps its NOPASS attribute.
        type :: b_t
            type(t) :: v
        contains
            procedure, nopass :: get => bget
        end type
    contains
        function bget(x) result(r)
            type(t), intent(in) :: x
            type(t) :: r
            r = x
        end function
    end template
end module

program template_class_procedure_nopass_01
    use template_class_procedure_nopass_01_m
    implicit none
    instantiate tm {integer}
    type(b_t) :: b
    b%v = 3
    if (b%get(4) /= 4) error stop
    if (b%get(b%v) /= 3) error stop
    print *, "ok"
end program
