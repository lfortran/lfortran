! Assumed-shape array arguments of runtime trait messages follow the ordinary
! rules of their INTENT. Each procedure whose name starts with from_ has
! exactly one invalid actual argument: an intent(inout) or intent(out) array
! argument of a message called through a view needs a definable actual, also
! in a function message. Mismatched implements scale with an intent(in) array.
module traits_array_inout_negative_m
    implicit none
    real(8), parameter :: fixed(3) = [1, 2, 3]
    abstract interface :: IEstimates
    end interface
    abstract interface :: ICoeffs
        subroutine update(self, estimates, beta)
            type(itself), pass, intent(inout) :: self
            class(IEstimates), intent(inout) :: estimates
            real(8), intent(inout) :: beta(:)
        end subroutine update
        subroutine fill(self, x)
            type(itself), pass, intent(in) :: self
            real(8), intent(out) :: x(0:)
        end subroutine fill
        integer function clear(self, k)
            type(itself), pass, intent(in) :: self
            integer, intent(inout) :: k(:)
        end function clear
    end interface
    abstract interface :: IScale
        subroutine scale(self, x)
            type(itself), pass, intent(in) :: self
            real(8), intent(inout) :: x(:)
        end subroutine scale
    end interface
    type :: Prior
    end type
    implements IEstimates :: Prior
    end implements
    type :: Coeffs
    end type
    implements ICoeffs :: Coeffs
        procedure, pass :: update => coeffs_update
        procedure, pass :: fill => coeffs_fill
        procedure, pass :: clear => coeffs_clear
    end implements
    type :: Mismatched
    end type
    implements IScale :: Mismatched
        procedure, pass :: scale => mismatched_scale
    end implements
contains
    subroutine coeffs_update(self, estimates, beta)
        class(Coeffs), intent(inout) :: self
        class(IEstimates), intent(inout) :: estimates
        real(8), intent(inout) :: beta(:)
        beta = 2*beta
    end subroutine
    subroutine coeffs_fill(self, x)
        class(Coeffs), intent(in) :: self
        real(8), intent(out) :: x(0:)
        x = 0
    end subroutine
    integer function coeffs_clear(self, k)
        class(Coeffs), intent(in) :: self
        integer, intent(inout) :: k(:)
        coeffs_clear = size(k)
        k = 0
    end function
    subroutine mismatched_scale(self, x)
        class(Mismatched), intent(in) :: self
        real(8), intent(in) :: x(:)
    end subroutine
    function values() result(r)
        real(8) :: r(3)
        r = 1
    end function

    subroutine from_expression(c, e, b)
        class(ICoeffs), intent(inout) :: c
        class(IEstimates), intent(inout) :: e
        real(8), intent(inout) :: b(:)
        call c%update(e, b + 1)
    end subroutine
    subroutine from_constant(c, e)
        class(ICoeffs), intent(inout) :: c
        class(IEstimates), intent(inout) :: e
        call c%update(e, fixed)
    end subroutine
    subroutine from_constructor(c, e)
        class(ICoeffs), intent(inout) :: c
        class(IEstimates), intent(inout) :: e
        call c%update(e, [1.0d0, 2.0d0])
    end subroutine
    subroutine from_function(c, e)
        class(ICoeffs), intent(inout) :: c
        class(IEstimates), intent(inout) :: e
        call c%update(estimates=e, beta=values())
    end subroutine
    subroutine from_readonly(c, e, b)
        class(ICoeffs), intent(inout) :: c
        class(IEstimates), intent(inout) :: e
        real(8), intent(in) :: b(:)
        call c%update(e, b)
    end subroutine
    subroutine from_readonly_out(c, b)
        class(ICoeffs), intent(in) :: c
        real(8), intent(in) :: b(:)
        call c%fill(b)
    end subroutine
    subroutine from_function_message(c, k)
        class(ICoeffs), intent(in) :: c
        integer, intent(inout) :: k(:)
        integer :: n
        n = c%clear(2*k)
    end subroutine
end module

! The actual of an intent(inout) or intent(out) array dummy must be definable
! in a call through a view, an ordinary call and a static binding alike. Each
! procedure whose name starts with from_ has exactly one invalid actual: a
! section of a named constant or of an intent(in) array, an intent(in) array,
! a parenthesized variable, or a section with a vector subscript.
! valid_actuals passes pointer targets, also through intent(in) pointers and
! components of intent(in) objects, allocatable arrays, strided and reversed
! sections, and any array to an intent(in) array dummy.
module traits_array_definability_negative_m
    implicit none
    real(8), parameter :: fixed(3) = [1, 2, 3]
    type :: Holder
        real(8), pointer :: p(:) => null()
    end type
    abstract interface :: IMarker
    end interface
    abstract interface :: IArrays
        subroutine update(self, marker, beta)
            type(itself), pass, intent(inout) :: self
            class(IMarker), intent(inout) :: marker
            real(8), intent(inout) :: beta(:)
        end subroutine update
        subroutine fill(self, x)
            type(itself), pass, intent(in) :: self
            real(8), intent(out) :: x(0:)
        end subroutine fill
        integer function clear(self, k)
            type(itself), pass, intent(in) :: self
            integer, intent(inout) :: k(:)
        end function clear
        real(8) function total(self, x)
            type(itself), pass, intent(in) :: self
            real(8), intent(in) :: x(:)
        end function total
    end interface
    type :: Marker
    end type
    implements IMarker :: Marker
    end implements
    type :: Arrays
    end type
    implements IArrays :: Arrays
        procedure, pass :: update => arrays_update
        procedure, pass :: fill => arrays_fill
        procedure, pass :: clear => arrays_clear
        procedure, pass :: total => arrays_total
    end implements
contains
    subroutine arrays_update(self, marker, beta)
        class(Arrays), intent(inout) :: self
        class(IMarker), intent(inout) :: marker
        real(8), intent(inout) :: beta(:)
        beta = 2*beta
    end subroutine
    subroutine arrays_fill(self, x)
        class(Arrays), intent(in) :: self
        real(8), intent(out) :: x(0:)
        x = 0
    end subroutine
    integer function arrays_clear(self, k)
        class(Arrays), intent(in) :: self
        integer, intent(inout) :: k(:)
        arrays_clear = size(k)
        k = 0
    end function
    real(8) function arrays_total(self, x)
        class(Arrays), intent(in) :: self
        real(8), intent(in) :: x(:)
        arrays_total = sum(x)
    end function
    subroutine ordinary_update(beta)
        real(8), intent(inout) :: beta(:)
        beta = 2*beta
    end subroutine

    subroutine from_constant_section(a, m)
        class(IArrays), intent(inout) :: a
        class(IMarker), intent(inout) :: m
        call a%update(m, fixed(1:2))
    end subroutine
    subroutine from_constant_section_out(a)
        class(IArrays), intent(in) :: a
        call a%fill(fixed(2:3))
    end subroutine
    subroutine from_readonly_section(a, m, b)
        class(IArrays), intent(inout) :: a
        class(IMarker), intent(inout) :: m
        real(8), intent(in) :: b(:)
        call a%update(m, b(1:2))
    end subroutine
    subroutine from_readonly_section_out(a, b)
        class(IArrays), intent(in) :: a
        real(8), intent(in) :: b(:)
        call a%fill(b(2:))
    end subroutine
    subroutine from_parenthesized(a, m, b)
        class(IArrays), intent(inout) :: a
        class(IMarker), intent(inout) :: m
        real(8), intent(inout) :: b(:)
        call a%update(m, (b))
    end subroutine
    subroutine from_parenthesized_out(a, b)
        class(IArrays), intent(in) :: a
        real(8), intent(inout) :: b(:)
        call a%fill((b))
    end subroutine
    subroutine from_vector_subscript(a, m, b)
        class(IArrays), intent(inout) :: a
        class(IMarker), intent(inout) :: m
        real(8), intent(inout) :: b(:)
        call a%update(m, b([1, 3]))
    end subroutine
    subroutine from_vector_subscript_out(a, b)
        class(IArrays), intent(in) :: a
        real(8), intent(inout) :: b(:)
        call a%fill(b([3, 1]))
    end subroutine
    subroutine from_vector_subscript_function(a, k)
        class(IArrays), intent(in) :: a
        integer, intent(inout) :: k(:)
        integer :: n
        n = a%clear(k([2, 1]))
    end subroutine
    subroutine from_ordinary_constant_section()
        call ordinary_update(fixed(1:2))
    end subroutine
    subroutine from_ordinary_readonly_section(b)
        real(8), intent(in) :: b(:)
        call ordinary_update(b(1:2))
    end subroutine
    subroutine from_ordinary_parenthesized(b)
        real(8), intent(inout) :: b(:)
        call ordinary_update((b))
    end subroutine
    subroutine from_ordinary_vector_subscript(b)
        real(8), intent(inout) :: b(:)
        call ordinary_update(b([1, 3]))
    end subroutine
    subroutine from_static_readonly(a, m, b)
        type(Arrays), intent(inout) :: a
        type(Marker), intent(inout) :: m
        real(8), intent(in) :: b(:)
        call a%update(m, b)
    end subroutine
    subroutine from_static_vector_subscript(a, m, b)
        type(Arrays), intent(inout) :: a
        type(Marker), intent(inout) :: m
        real(8), intent(inout) :: b(:)
        call a%update(m, b([1, 3]))
    end subroutine

    subroutine valid_actuals(a, m, b, r, p, h, c)
        class(IArrays), intent(inout) :: a
        class(IMarker), intent(inout) :: m
        real(8), intent(inout) :: b(:)
        real(8), intent(in) :: r(:)
        real(8), pointer, intent(in) :: p(:)
        type(Holder), intent(in) :: h
        real(8), allocatable, intent(inout) :: c(:)
        real(8) :: s
        call a%update(m, b(3:1:-2))
        call a%update(m, p(2:))
        call a%fill(h%p)
        call a%fill(h%p(::2))
        call a%update(m, c)
        call ordinary_update(p(::2))
        call ordinary_update(h%p)
        s = a%total(r) + a%total(fixed(1:2)) + a%total((b)) + a%total(b([1, 3])) + a%total(r + 1)
    end subroutine
end module

! The associate name of an expression, also through another associate name,
! and a part of one are not definable (F2018 11.1.3.3), so none can be the
! actual of an intent(inout) or intent(out) array dummy. Each procedure whose
! name starts with from_ has exactly one such actual. valid_associates passes
! the associate names of a variable and of its section, and their parts.
module traits_array_associate_negative_m
    use traits_array_definability_negative_m, only: IArrays, IMarker, ordinary_update
    implicit none
contains
    subroutine from_associate_expression(a, m, b)
        class(IArrays), intent(inout) :: a
        class(IMarker), intent(inout) :: m
        real(8), intent(inout) :: b(:)
        associate (x => (b))
            call a%update(m, x)
        end associate
    end subroutine
    subroutine from_associate_expression_alias_out(a, b)
        class(IArrays), intent(in) :: a
        real(8), intent(inout) :: b(:)
        associate (x => b + 1)
            associate (y => x)
                call a%fill(y(2:))
            end associate
        end associate
    end subroutine
    subroutine from_ordinary_associate_expression(b)
        real(8), intent(inout) :: b(:)
        associate (x => (b))
            call ordinary_update(x)
        end associate
    end subroutine
    subroutine valid_associates(a, m, b)
        class(IArrays), intent(inout) :: a
        class(IMarker), intent(inout) :: m
        real(8), intent(inout) :: b(:)
        associate (x => b, y => b(2:))
            call a%update(m, x)
            call a%fill(y)
            associate (z => x)
                call ordinary_update(z(::2))
            end associate
        end associate
    end subroutine
end module
