#ifdef __GFORTRAN__
#define MACRO_SAME(A) A
#define MACRO_CAT2(A,B) MACRO_SAME(A)B
#define MACRO_CAT3(A,B,C) MACRO_CAT2(A,B)C
#define MACRO_STRINGIFY(A) "A"
#else
#define MACRO_CAT2(A,B) A ## B
#define MACRO_CAT3(A,B,C) A ## B ## C
#define MACRO_STRINGIFY(A) #A
#endif
#define ADD_ARGUMENT(n) res = MACRO_CAT2(a,n)

module mre_method
    implicit none
contains

    function method_create_1(a1) result(res)
        integer, intent(in) :: a1
        integer             :: res

        ADD_ARGUMENT(1)
    end function

end module mre_method

program cpp_pre_09
    use mre_method, only: method_create_1
    implicit none
    integer :: x

    x = method_create_1(42)
    if (x /= 42) error stop
    print *, "Tested cpp_pre_09: ", x
end program cpp_pre_09
