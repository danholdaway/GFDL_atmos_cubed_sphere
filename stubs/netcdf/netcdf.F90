module netcdf
    implicit none
    
    integer :: nf90_write, nf90_nowrite, nf90_noerr
    integer :: nf_nowrite, nf_noerr

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

    interface nf_open
        module procedure nf90_open
    end interface

    interface nf_close
        module procedure nf90_close
    end interface

    interface nf_strerror
        module procedure nf90_strerror
    end interface

    interface nf_inq_dimid
        module procedure nf90_inq_dimid
    end interface
    
    interface nf_inq_dimlen
        module procedure nf90_inq_dimlen
    end interface    
    
    interface nf_inq_varid
        module procedure nf90_inq_varid
    end interface    
                        
    interface nf_get_var_real
        module procedure nf90_get_var_r4_1d
        module procedure nf90_get_var_r4_2d
        module procedure nf90_get_var_r4_3d
    end interface

    interface nf_get_var_double
        module procedure nf90_get_var_r8_1d
        module procedure nf90_get_var_r8_2d
        module procedure nf90_get_var_r8_3d
    end interface

    interface nf_get_vara_real
        module procedure nf_get_vara_real_2d
        module procedure nf_get_vara_real_3d
        module procedure nf_get_vara_real_4d
    end interface

    interface nf_get_vara_double
        module procedure nf_get_vara_double_2d
        module procedure nf_get_vara_double_3d
        module procedure nf_get_vara_double_4d
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

    integer function nf_get_att(ncid,varid,att_name,att_val)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: att_name
        real(kind=8), intent(out) :: att_val
    end function nf_get_att

    integer function nf_get_att_text(ncid,varid,att_name,att_text)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: att_name
        character(len=*), intent(out) :: att_text
    end function nf_get_att_text

    integer function nf_get_vara_real_2d(ncid,varid,start,count,var)
        integer, intent(in) :: ncid, varid
        integer, optional, intent(in) :: start(:),  count(:)
        real(kind=4), intent(out) :: var(:,:)
    end function nf_get_vara_real_2d

    integer function nf_get_vara_real_3d(ncid,varid,start,count,var)
        integer, intent(in) :: ncid, varid
        integer, optional, intent(in) :: start(:),  count(:)
        real(kind=4), intent(out) :: var(:,:,:)
    end function nf_get_vara_real_3d

    integer function nf_get_vara_real_4d(ncid,varid,start,count,var)
        integer, intent(in) :: ncid, varid
        integer, optional, intent(in) :: start(:),  count(:)
        real(kind=4), intent(out) :: var(:,:,:,:)
    end function nf_get_vara_real_4d

    integer function nf_get_vara_double_2d(ncid,varid,start,count,var)
        integer, intent(in) :: ncid, varid
        integer, optional, intent(in) :: start(:),  count(:)
        real(kind=8), intent(out) :: var(:,:)
    end function nf_get_vara_double_2d

    integer function nf_get_vara_double_3d(ncid,varid,start,count,var)
        integer, intent(in) :: ncid, varid
        integer, optional, intent(in) :: start(:),  count(:)
        real(kind=8), intent(out) :: var(:,:,:)
    end function nf_get_vara_double_3d

    integer function nf_get_vara_double_4d(ncid,varid,start,count,var)
        integer, intent(in) :: ncid, varid
        integer, optional, intent(in) :: start(:),  count(:)
        real(kind=8), intent(out) :: var(:,:,:,:)
    end function nf_get_vara_double_4d

end module netcdf