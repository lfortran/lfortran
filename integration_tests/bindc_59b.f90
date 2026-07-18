! External wrapper (implicit interface) whose dummy is an assumed-size
! CHARACTER(KIND=C_CHAR) array. It forwards the argument unchanged to the
! BIND(C) procedure write_member_value. Because get_member_value is a top-level
! external procedure, its assumed-size character dummy is received as a bare
! data pointer (storage association) and must be forwarded directly to the
! BIND(C) callee.
subroutine get_member_value(value)
    use iso_c_binding, only: c_char
    implicit none
    character(kind=c_char), intent(out) :: value(*)
    interface
        subroutine write_member_value(value) bind(c)
            use iso_c_binding, only: c_char
            character(kind=c_char), intent(out) :: value(*)
        end subroutine
    end interface
    call write_member_value(value)
end subroutine get_member_value
