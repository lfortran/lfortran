module template_struct_dependency_01_m
    implicit none
    template tm {t}
        deferred type :: t
        type :: inner
            type(t) :: a
        end type
        type :: mid
            type(inner) :: i
        end type
        type :: outer
            type(mid) :: m
            type(inner) :: j
        end type
    end template
    ! `outer` depends on `mid` and `inner`, which are listed after it
    instantiate tm {integer}, only: o_int => outer, m_int => mid, inner
end module

program template_struct_dependency_01
    use template_struct_dependency_01_m
    implicit none
    template tp {t}
        deferred type :: t
        type :: inner
            type(t) :: a
        end type
        type :: outer
            type(inner) :: i
        end type
    end template
    instantiate tp {real}, only: o => outer, i => inner
    instantiate tp {integer}, only: i2 => inner, o2 => outer
    type(o) :: s
    type(i) :: x
    type(o2) :: s2
    type(i2) :: x2
    type(o_int) :: s3
    type(m_int) :: y3
    type(inner) :: x3

    x%a = 5.0
    s%i = x
    print *, s%i%a
    if (abs(s%i%a - 5.0) > 1e-6) error stop

    x2%a = 6
    s2%i = x2
    print *, s2%i%a
    if (s2%i%a /= 6) error stop

    x3%a = 7
    y3%i = x3
    s3%m = y3
    s3%j = x3
    print *, s3%m%i%a, s3%j%a
    if (s3%m%i%a /= 7) error stop
    if (s3%j%a /= 7) error stop
end program
