! Tests the optional construct name on END TEMPLATE (R1603) and
! END REQUIREMENT (R1635).
module template_end_name_01_m
    implicit none

    requirement addable_r {t, add}
        deferred type :: t
        function add(lhs, rhs) result(res)
            type(t), intent(in) :: lhs, rhs
            type(t) :: res
        end function
    end requirement addable_r

    template sum_tmpl(t, add)
        require :: addable_r {t, add}
        private
        public :: sum_t
    contains
        function sum_t(vec) result(res)
            type(t), intent(in) :: vec(:)
            type(t) :: res
            integer :: i
            res = vec(1)
            do i = 2, size(vec)
                res = add(res, vec(i))
            end do
        end function
    end template sum_tmpl

end module

program template_end_name_01
    use template_end_name_01_m
    implicit none
    instantiate sum_tmpl {integer, operator(+)}, only: sum_int => sum_t
    instantiate sum_tmpl {real, operator(+)}, only: sum_real => sum_t
    integer :: si
    real :: sr

    si = sum_int([1, 2, 3, 4])
    if (si /= 10) error stop

    sr = sum_real([1.0, 2.0, 3.0, 4.5])
    if (abs(sr - 10.5) > 1e-6) error stop

    print *, si, sr
end program
