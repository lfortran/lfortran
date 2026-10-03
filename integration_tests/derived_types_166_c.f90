module derived_types_166_c
    use derived_types_166_b, only: u => t, z
    implicit none
    type :: h
        type(u) :: c = z
    end type
    type :: h2
        type(u) :: d = z
    end type
    type(u) :: mv = z
contains
    integer function g()
        use derived_types_166_b, only: z
        type(u) :: x = z
        g = x%i
    end function
end module
