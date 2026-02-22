module submodule_16_string
    implicit none

    type :: string_t
        character(len=:), allocatable :: s
    end type

    interface operator(//)
        module procedure cat_char_string
    end interface

contains

    pure function cat_char_string(lhs, rhs) result(out)
        character(len=*), intent(in) :: lhs
        type(string_t), intent(in) :: rhs
        character(len=:), allocatable :: out

        out = lhs // rhs%s
    end function

end module

module submodule_16_m
    use submodule_16_string, only: string_t, operator(//)
    implicit none

    type :: test_diagnosis_t
        logical :: test_passed_ = .false.
        character(len=:), allocatable :: diagnostics_string_
    end type

    interface operator(//)
        elemental module function append_string_if_test_failed(lhs, rhs) result(lhs_cat_rhs)
            class(test_diagnosis_t), intent(in) :: lhs
            type(string_t), intent(in) :: rhs
            type(test_diagnosis_t) lhs_cat_rhs
        end function
    end interface

contains

    subroutine run_demo()
        type(test_diagnosis_t) :: d
        type(string_t) :: s

        d%test_passed_ = .false.
        d%diagnostics_string_ = "prefix: "
        s%s = "payload"

        d = d // s

        if (d%diagnostics_string_ /= "prefix: payload") error stop 1
    end subroutine

end module

submodule(submodule_16_m) submodule_16_s
    implicit none

contains

    module procedure append_string_if_test_failed
        if (lhs%test_passed_) then
            lhs_cat_rhs = lhs
        else
            lhs_cat_rhs = test_diagnosis_t(lhs%test_passed_, lhs%diagnostics_string_ // rhs)
        end if
    end procedure

end submodule

program submodule_16
    use submodule_16_m, only: run_demo
    implicit none

    call run_demo()
end program
