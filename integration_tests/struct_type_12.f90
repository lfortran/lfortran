module struct_type_12_int_module
    implicit none
    integer, parameter :: k = 4
    type ty_int
        integer(k) :: k = 42_k
    end type
end module

module struct_type_12_complex_module
    implicit none
    integer, parameter :: q = selected_real_kind(10)
    type ty_complex
        complex(q) :: q = (1.0_q, 2.0_q)
    end type
end module

module struct_type_12_char_module
    implicit none
    integer, parameter :: c = selected_char_kind('ASCII')
    type ty_char
        character(kind=c, len=5) :: c = 'hello'
    end type
end module

module struct_type_12_logical_module
    implicit none
    integer, parameter :: l = 4
    type ty_logical
        logical(l) :: l = .true.
    end type
end module

module struct_type_12_real_module
    implicit none
    integer, parameter :: q = selected_real_kind(10)
    type ty_real
        real(q) :: q = 1._q
    end type
end module


program struct_type_12
    use struct_type_12_int_module
    use struct_type_12_complex_module,  ty_complex => ty_complex
    use struct_type_12_char_module,     ty_char    => ty_char
    use struct_type_12_logical_module,  ty_logical => ty_logical
    use struct_type_12_real_module,     ty_real    => ty_real
    implicit none

    type(ty_int)     :: t_int
    type(ty_complex) :: t_complex
    type(ty_char)    :: t_char
    type(ty_logical) :: t_logical
    type(ty_real)    :: t_real

    if (t_int%k /= 42) error stop "integer kind shadow: value incorrect"
    if (real(t_complex%q) /= 1.0 .or. aimag(t_complex%q) /= 2.0) &
        error stop "complex kind shadow: value incorrect"
    if (t_char%c /= 'hello') error stop "char kind shadow: value incorrect"
    if (.not. t_logical%l) error stop "logical kind shadow: value incorrect"
    if (t_real%q /= 1.0) error stop "real kind shadow: value incorrect"
    print *, "all tests passed"
end program struct_type_12