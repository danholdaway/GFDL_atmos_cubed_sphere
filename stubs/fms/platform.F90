

module platform_mod

! #include <fms_platform.h> 
!Set type kinds.
!These values are not necessarily portable.
!DEC$ MESSAGE:'Using 8-byte addressing'


!Control "pure" functions.
!DEC$ MESSAGE:'Using pure routines.'


!Control array members of derived types.
!DEC$ MESSAGE:'Using allocatable derived type array members.'


!Control use of cray pointers.
!DEC$ MESSAGE:'Using cray pointers.'


!Control size of integers that will hold address values.
!Appears for legacy reasons, but seems rather dangerous.


!If you do not want to use 64-bit integers.


!If you do not want to use 32-bit floats.


!If you want to use quad-precision.
! The NO_QUAD_PRECISION macro will be deprecated and removed at some future time.
! Model code will rely solely upon the ENABLE_QUAD_PRECISION macro thereafer.


    integer, public, parameter :: r8_kind = 8
    integer, public, parameter :: r4_kind = 4
    integer, public, parameter :: i8_kind = 8
    
end module platform_mod
