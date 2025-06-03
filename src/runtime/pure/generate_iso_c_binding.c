#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <complex.h>

#include <stdlib.h>

// reference: https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html

// integer types
const size_t c_int = sizeof(int);

const size_t c_short = sizeof(short int);

const size_t c_long = sizeof(long int);

const size_t c_long_long = sizeof(long long int);

const size_t c_signed_char = sizeof(signed char);

const size_t c_size_t = sizeof(size_t);

const size_t c_int8_t = sizeof(int8_t);

const size_t c_int16_t = sizeof(int16_t);

const size_t c_int32_t = sizeof(int32_t);

const size_t c_int64_t = sizeof(int64_t);

const size_t c_int_least8_t = sizeof(int_least8_t);

const size_t c_int_least16_t = sizeof(int_least16_t);

const size_t c_int_least32_t = sizeof(int_least32_t);

const size_t c_int_least64_t = sizeof(int_least64_t);

const size_t c_int_fast8_t = sizeof(int_fast8_t);

const size_t c_int_fast16_t = sizeof(int_fast16_t);

const size_t c_int_fast32_t = sizeof(int_fast32_t);

const size_t c_int_fast64_t = sizeof(int_fast64_t);

const size_t c_intmax_t = sizeof(intmax_t);

const size_t c_intptr_t = sizeof(intptr_t);

const size_t c_ptrdiff_t = sizeof(ptrdiff_t);

// real types
const size_t c_float = sizeof(float);

const size_t c_double = sizeof(double);

const size_t c_long_double = sizeof(long double);

// complex types
const size_t c_float_complex = sizeof(float);

const size_t c_double_complex = sizeof(double);

const size_t c_long_double_complex = sizeof(long double);

// logical type
const size_t c_bool = sizeof(_Bool);

// character type
const size_t c_char = sizeof(char);

// write to file 'lfortran_intrinsic_iso_c_binding.f90'
int main() {
    FILE *f = fopen("src/runtime/pure/lfortran_intrinsic_iso_c_binding.f90", "w");
    

    fprintf(f, "! AUTO_GENERATED FILE, DO NOT EDIT BY HAND.\n");
    fprintf(f, "! This file was generated using `generate_iso_c_binding.c`.\n\n");

    fprintf(f, "module lfortran_intrinsic_iso_c_binding\n");

    fprintf(f, "    implicit none\n\n");

    fprintf(f, "    type :: c_ptr\n");
    fprintf(f, "        integer :: ptr\n");
    fprintf(f, "    end type c_ptr\n\n");

    fprintf(f, "    type :: c_funptr\n");
    fprintf(f, "        integer :: ptr\n");
    fprintf(f, "    end type c_funptr\n\n");

    fprintf(f, "    ! Constants for bind(c) variable types\n");
    fprintf(f, "    integer, parameter :: c_int = %zu\n", c_int);
    fprintf(f, "    integer, parameter :: c_short = %zu\n", c_short);
    fprintf(f, "    integer, parameter :: c_long = %zu\n", c_long);
    fprintf(f, "    integer, parameter :: c_long_long = %zu\n", c_long_long);
    fprintf(f, "    integer, parameter :: c_signed_char = %zu\n", c_signed_char);
    fprintf(f, "    integer, parameter :: c_size_t = %zu\n", c_size_t);
    fprintf(f, "    integer, parameter :: c_int8_t = %zu\n", c_int8_t);
    fprintf(f, "    integer, parameter :: c_int16_t = %zu\n", c_int16_t);
    fprintf(f, "    integer, parameter :: c_int32_t = %zu\n", c_int32_t);
    fprintf(f, "    integer, parameter :: c_int64_t = %zu\n", c_int64_t);
    fprintf(f, "    integer, parameter :: c_int_least8_t = %zu\n", c_int_least8_t);
    fprintf(f, "    integer, parameter :: c_int_least16_t = %zu\n", c_int_least16_t);
    fprintf(f, "    integer, parameter :: c_int_least32_t = %zu\n", c_int_least32_t);
    fprintf(f, "    integer, parameter :: c_int_least64_t = %zu\n", c_int_least64_t);
    fprintf(f, "    integer, parameter :: c_int_fast8_t = %zu\n", c_int_fast8_t);
    fprintf(f, "    integer, parameter :: c_int_fast16_t = %zu\n", c_int_fast16_t);
    fprintf(f, "    integer, parameter :: c_int_fast32_t = %zu\n", c_int_fast32_t);
    fprintf(f, "    integer, parameter :: c_int_fast64_t = %zu\n", c_int_fast64_t);
    fprintf(f, "    integer, parameter :: c_intmax_t = %zu\n", c_intmax_t);
    fprintf(f, "    integer, parameter :: c_intptr_t = %zu\n", c_intptr_t);
    fprintf(f, "    integer, parameter :: c_ptrdiff_t = %zu\n", c_ptrdiff_t);
    fprintf(f, "    integer, parameter :: c_float = %zu\n", c_float);
    fprintf(f, "    integer, parameter :: c_double = %zu\n", c_double);
    fprintf(f, "    integer, parameter :: c_long_double = %zu\n", c_long_double);
    fprintf(f, "    integer, parameter :: c_float_complex = %zu\n", c_float_complex);
    fprintf(f, "    integer, parameter :: c_double_complex = %zu\n", c_double_complex);
    fprintf(f, "    integer, parameter :: c_long_double_complex = %zu\n", c_long_double_complex);
    fprintf(f, "    integer, parameter :: c_bool = %zu\n", c_bool);
    fprintf(f, "    integer, parameter :: c_char = %zu\n\n", c_char);

    fprintf(f, "    ! Constants for some null types.\n");
    fprintf(f, "    character(len=1), parameter :: c_null_char = char(0)\n");
    fprintf(f, "    type(c_ptr), parameter :: c_null_ptr = c_ptr(0)\n");
    fprintf(f, "    type(c_funptr), parameter :: c_null_funptr = c_funptr(0)\n\n");

    fprintf(f, "    ! Intrinsic procedures provided by the module.\n");
    fprintf(f, "    ! TODO: Implement intrinsics `c_f_procpointer` and `c_sizeof`.\n");
    fprintf(f, "    interface\n\n");

    fprintf(f, "        logical function c_associated(c_ptr_var)\n");
    fprintf(f, "            import c_ptr\n");
    fprintf(f, "            type(c_ptr), intent(in) :: c_ptr_var\n");
    fprintf(f, "        end function\n\n");

    fprintf(f, "        subroutine c_f_pointer(cptr, fptr, shape)\n");
    fprintf(f, "            import c_ptr\n");
    fprintf(f, "            type(c_ptr), intent(in) :: cptr\n");
    fprintf(f, "            !type(*), pointer, intent(out) :: fptr\n");
    fprintf(f, "            integer, pointer, intent(out) :: fptr\n");
    fprintf(f, "            integer, intent(in), optional :: shape(:)\n");
    fprintf(f, "        end subroutine\n\n");

    fprintf(f, "        !type(c_ptr) function c_loc(x)\n");
    fprintf(f, "        integer function c_loc(x)\n");
    fprintf(f, "            import c_ptr\n");
    fprintf(f, "            !type(*), intent(in) :: x\n");
    fprintf(f, "            integer, intent(in) :: x\n");
    fprintf(f, "        end function\n\n");

    fprintf(f, "        !type(c_funptr) function c_funloc(x)\n");
    fprintf(f, "        integer function c_funloc(x)\n");
    fprintf(f, "            import c_funptr\n");
    fprintf(f, "            !type(*), intent(in) :: x\n");
    fprintf(f, "            integer, intent(in) :: x\n");
    fprintf(f, "        end function\n\n");

    fprintf(f, "    end interface\n\n");
   
    fprintf(f, "end module lfortran_intrinsic_iso_c_binding\n");

    fclose(f);
}