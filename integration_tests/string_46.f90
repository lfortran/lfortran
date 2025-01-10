program string_46

type :: string
    character(:), allocatable :: s
end type

type(string), allocatable :: x(:)
allocate(x(2))
allocate(character(len("fix")) :: x(1)%s)
x(1)%s = "fix"
allocate(character(len("lfortran")) :: x(2)%s)
x(2)%s = "lfortran"
x = g(x)
print *, x(1)%s, x(2)%s
if( x(1)%s /= "fix" ) error stop
if( x(2)%s /= "lfortran" ) error stop


contains

    function g(intokens) result(tokens)
    type(string), intent(in) :: intokens(:)
    type(string), allocatable :: tokens(:)
    allocate(tokens(size(intokens)))
    tokens = intokens
    end function

end program
