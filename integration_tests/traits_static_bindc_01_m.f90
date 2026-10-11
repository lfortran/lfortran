! Static trait syntax is an LFortran extension.
module traits_static_bindc_01_m
    implicit none
    abstract interface :: ICValue
        function value(n) result(r) bind(c)
            integer, value, intent(in) :: n
            integer :: r
        end function
    end interface
    type :: SourcePayload
        integer :: n
    end type
    implements ICValue :: SourcePayload
        procedure, nopass :: value => source_value
    end implements
contains
    function source_value(n) result(r)
        integer, value, intent(in) :: n
        integer :: r
        r = n + 19
    end function
end module
