 subroutine foo(a,b,f)
    complex, intent(out) :: a
    complex, intent(in)  :: b
    complex, external    :: f
    a = f(b)
 end subroutine foo
