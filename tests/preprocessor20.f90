#if !defined pi_constant
#define PI 3.14
#endif /* multi-line comment on a two separate lines
*/

#if !defined negative_constant
#define negative_one -1
#endif /* multi-line comment on a single line */


program preprocessor20
    implicit none
    print *, "PI: ", PI
    print *, "negative one: ", negative_one
end program preprocessor20
