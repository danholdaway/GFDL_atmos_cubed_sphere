module netcdf
    implicit none
    
    integer :: nf90_write, nf90_nowrite, nf90_noerr

    interface nf90_put_var
        module procedure nf90_put_var_r4_2d
        module procedure nf90_put_var_r4_3d
        module procedure nf90_put_var_r8_2d
        module procedure nf90_put_var_r8_3d
    end interface

    interface nf90_get_var
        module procedure nf90_get_var_r4_1d
        module procedure nf90_get_var_r4_2d
        module procedure nf90_get_var_r4_3d
        module procedure nf90_get_var_r8_1d
        module procedure nf90_get_var_r8_2d
        module procedure nf90_get_var_r8_3d
    end interface

contains

    function nf90_strerror(status)
        integer, intent(in) ::  status 
        character(len=50) :: nf90_strerror
    end function nf90_strerror

    integer function nf90_open(path ,mode, ncid)
        character(len=*), intent(in) :: path
        integer, intent(in) :: mode
        integer, intent(out) :: ncid
    end function nf90_open

    integer function nf90_close(ncid)
        integer, intent(in) :: ncid
    end function nf90_close

    integer function nf90_put_var_r4_2d(ncid,varid,var,start,count,stride)
        integer, intent(in) :: ncid, varid
        real(kind=4), intent(in) :: var(:,:)
        integer, optional, intent(in) :: start(:),  count(:), stride(:)
    end function nf90_put_var_r4_2d

    integer function nf90_put_var_r4_3d(ncid,varid,var,start,count,stride)
        integer, intent(in) :: ncid, varid
        real(kind=4), intent(in) :: var(:,:,:)
        integer, optional, intent(in) :: start(:),  count(:), stride(:)
    end function nf90_put_var_r4_3d

    integer function nf90_put_var_r8_2d(ncid,varid,var,start,count,stride)
        integer, intent(in) :: ncid, varid
        real(kind=8), intent(in) :: var(:,:)
        integer, optional, intent(in) :: start(:),  count(:), stride(:)
    end function nf90_put_var_r8_2d

    integer function nf90_put_var_r8_3d(ncid,varid,var,start,count,stride)
        integer, intent(in) :: ncid, varid
        real(kind=8), intent(in) :: var(:,:,:)
        integer, optional, intent(in) :: start(:),  count(:), stride(:)
    end function nf90_put_var_r8_3d

    integer function nf90_get_var_r4_1d(ncid,varid,var,start,count,stride)
        integer, intent(in) :: ncid, varid
        real(kind=4), intent(out) :: var(:)
        integer, optional, intent(in) :: start(:),  count(:), stride(:)
    end function nf90_get_var_r4_1d 

    integer function nf90_get_var_r4_2d(ncid,varid,var,start,count,stride)
        integer, intent(in) :: ncid, varid
        real(kind=4), intent(out) :: var(:,:)
        integer, optional, intent(in) :: start(:),  count(:), stride(:)
    end function nf90_get_var_r4_2d 

    integer function nf90_get_var_r4_3d(ncid,varid,var,start,count,stride)
        integer, intent(in) :: ncid, varid
        real(kind=4), intent(out) :: var(:,:,:)
        integer, optional, intent(in) :: start(:),  count(:), stride(:)
    end function nf90_get_var_r4_3d 

    integer function nf90_get_var_r8_1d(ncid,varid,var,start,count,stride)
        integer, intent(in) :: ncid, varid
        real(kind=8), intent(out) :: var(:)
        integer, optional, intent(in) :: start(:),  count(:), stride(:)
    end function nf90_get_var_r8_1d 

    integer function nf90_get_var_r8_2d(ncid,varid,var,start,count,stride)
        integer, intent(in) :: ncid, varid
        real(kind=8), intent(out) :: var(:,:)
        integer, optional, intent(in) :: start(:),  count(:), stride(:)
    end function nf90_get_var_r8_2d 

    integer function nf90_get_var_r8_3d(ncid,varid,var,start,count,stride)
        integer, intent(in) :: ncid, varid
        real(kind=8), intent(out) :: var(:,:,:)
        integer, optional, intent(in) :: start(:),  count(:), stride(:)
    end function nf90_get_var_r8_3d 

    integer function nf90_inquire(ncid,nvariables)
        integer, intent(in) :: ncid
        integer, intent(out) :: nvariables
    end function nf90_inquire 
    
    integer function nf90_inq_varid(ncid,name,varid)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer, intent(out) :: varid
    end function nf90_inq_varid

    integer function nf90_inquire_variable(ncid,varid,name)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(out) :: name
    end function nf90_inquire_variable

    integer function nf90_inq_dimid(ncid,name,dimid)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer, intent(out) :: dimid
    end function nf90_inq_dimid

    integer function nf90_inq_dimlen(ncid,dimid,dimlen)
        integer, intent(in) :: ncid, dimid
        integer, intent(out) :: dimlen
    end function nf90_inq_dimlen

    integer function nf90_inquire_dimension(ncid,dimid,len)
        integer, intent(in) :: ncid, dimid
        integer, intent(out) :: len
    end function nf90_inquire_dimension

end module netcdf