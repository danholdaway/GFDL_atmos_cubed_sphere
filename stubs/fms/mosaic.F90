module mosaic_mod
    implicit none

    public :: get_mosaic_ntiles

    contains

    integer function get_mosaic_ntiles(mosaic_file)
        character(len=*), intent(in) :: mosaic_file
    end function get_mosaic_ntiles

end module mosaic_mod