module traits_runtime_initializers_01_contracts
    implicit none
    private
    public :: ICastable, IPrintable, IAdmissible, cast, show, show_admissible

    abstract interface :: ICastable
        initial(n)
            integer, intent(in) :: n
        end initial
    end interface ICastable

    abstract interface :: IPrintable
        subroutine output(self)
            type(itself), intent(in), pass :: self
        end subroutine output
    end interface IPrintable

    abstract interface, extends(ICastable + IPrintable) :: IAdmissible
        subroutine input(self, filename)
            type(itself), intent(in), pass :: self
            character(*), intent(in) :: filename
        end subroutine input
    end interface IAdmissible

contains

    function cast{ICastable :: T}(n) result(r)
        integer, intent(in) :: n
        type(T)             :: r
        r = T(n)
    end function cast

    subroutine show(printer)
        class(ICastable + IPrintable), intent(in) :: printer
        call printer%output()
    end subroutine show

    subroutine show_admissible(item, filename)
        class(IAdmissible), intent(in) :: item
        character(*), intent(in)       :: filename
        call item%output()
        call item%input(filename)
    end subroutine show_admissible
end module traits_runtime_initializers_01_contracts
