module diag_manager_mod
    use mpp_domains_mod
    USE time_manager_mod, ONLY: time_type
    implicit none
INTERFACE register_diag_field
    MODULE PROCEDURE register_diag_field_scalar
    MODULE PROCEDURE register_diag_field_array
END INTERFACE

INTERFACE diag_field_add_attribute
    MODULE PROCEDURE diag_field_add_attribute_scalar_r
    MODULE PROCEDURE diag_field_add_attribute_scalar_i
    MODULE PROCEDURE diag_field_add_attribute_scalar_c
    MODULE PROCEDURE diag_field_add_attribute_r1d
    MODULE PROCEDURE diag_field_add_attribute_i1d
END INTERFACE 

INTERFACE send_data
    MODULE PROCEDURE send_data_0d
    MODULE PROCEDURE send_data_1d
    MODULE PROCEDURE send_data_2d
    MODULE PROCEDURE send_data_3d
END INTERFACE 

! Replaced CLASS(*) with interface, to avoid Tapenade error
interface diag_axis_init
    module procedure diag_axis_init_i_1d
    module procedure diag_axis_init_r_1d
end interface

    
contains

! TODO: remove because it isn't used and it cause and error in Tapenade
LOGICAL FUNCTION send_data_0d(diag_field_id, field, time, err_msg)
INTEGER, INTENT(in) :: diag_field_id
REAL, INTENT(in) :: field
TYPE(time_type), INTENT(in), OPTIONAL :: time
CHARACTER(len=*), INTENT(out), OPTIONAL :: err_msg
END FUNCTION send_data_0d

! TODO: remove because it isn't used and it cause and error in Tapenade
!> @return true if send is successful
LOGICAL FUNCTION send_data_1d(diag_field_id, field, time, is_in, mask, rmask, ie_in, weight, err_msg)
INTEGER, INTENT(in) :: diag_field_id
REAL, DIMENSION(:), INTENT(in) :: field
REAL, INTENT(in), OPTIONAL :: weight
TYPE (time_type), INTENT(in), OPTIONAL :: time
INTEGER, INTENT(in), OPTIONAL :: is_in, ie_in
LOGICAL, INTENT(in), DIMENSION(:), OPTIONAL :: mask
REAL, INTENT(in), DIMENSION(:), OPTIONAL :: rmask
CHARACTER(len=*), INTENT(out), OPTIONAL :: err_msg
END FUNCTION send_data_1d

! TODO: remove because it isn't used and it cause and error in Tapenade
!> @return true if send is successful
LOGICAL FUNCTION send_data_2d(diag_field_id, field, time, is_in, js_in, &
   & mask, rmask, ie_in, je_in, weight, err_msg)
INTEGER, INTENT(in) :: diag_field_id
REAL, INTENT(in), DIMENSION(:,:) :: field
REAL, INTENT(in), OPTIONAL :: weight
TYPE (time_type), INTENT(in), OPTIONAL :: time
INTEGER, INTENT(in), OPTIONAL :: is_in, js_in, ie_in, je_in
LOGICAL, INTENT(in), DIMENSION(:,:), OPTIONAL :: mask
REAL, INTENT(in), DIMENSION(:,:),OPTIONAL :: rmask
CHARACTER(len=*), INTENT(out), OPTIONAL :: err_msg
END FUNCTION send_data_2d

! TODO: remove because it isn't used and it cause and error in Tapenade
!> @return true if send is successful
LOGICAL FUNCTION send_data_3d(diag_field_id, field, time, is_in, js_in, ks_in, &
         & mask, rmask, ie_in, je_in, ke_in, weight, err_msg)
INTEGER, INTENT(in) :: diag_field_id
REAL, DIMENSION(:,:,:), INTENT(in) :: field
REAL, INTENT(in), OPTIONAL :: weight
TYPE (time_type), INTENT(in), OPTIONAL :: time
INTEGER, INTENT(in), OPTIONAL :: is_in, js_in, ks_in,ie_in,je_in, ke_in
LOGICAL, DIMENSION(:,:,:), INTENT(in), OPTIONAL :: mask
REAL, DIMENSION(:,:,:), INTENT(in), OPTIONAL :: rmask
CHARACTER(len=*), INTENT(out), OPTIONAL :: err_msg
END FUNCTION send_data_3d

INTEGER FUNCTION diag_axis_init_i_1d(name, DATA, units, cart_name, long_name, direction,&
& set_name, edges, Domain, Domain2, DomainU, aux, req, tile_count, domain_position )
CHARACTER(len=*), INTENT(in) :: name !< Short name for axis
real, DIMENSION(:), INTENT(in) :: DATA !< Array of coordinate values
CHARACTER(len=*), INTENT(in) :: units !< Units for the axis
CHARACTER(len=*), INTENT(in) :: cart_name !< Cartesian axis ("X", "Y", "Z", "T")
CHARACTER(len=*), INTENT(in), OPTIONAL :: long_name !< Long name for the axis.
CHARACTER(len=*), INTENT(in), OPTIONAL :: set_name
INTEGER, INTENT(in), OPTIONAL :: direction !< Indicates the direction of the axis
INTEGER, INTENT(in), OPTIONAL :: edges !< Axis ID for the previously defined "edges axis"
TYPE(domain1d), INTENT(in), OPTIONAL :: Domain
TYPE(domain2d), INTENT(in), OPTIONAL :: Domain2
TYPE(domainUG), INTENT(in), OPTIONAL :: DomainU
CHARACTER(len=*), INTENT(in), OPTIONAL :: aux !< Auxiliary name, can only be <TT>geolon_t</TT> or <TT>geolat_t</TT>
CHARACTER(len=*), INTENT(in), OPTIONAL :: req !< Required field names.
INTEGER, INTENT(in), OPTIONAL :: tile_count
INTEGER, INTENT(in), OPTIONAL :: domain_position
end function diag_axis_init_i_1d

INTEGER FUNCTION diag_axis_init_r_1d(name, DATA, units, cart_name, long_name, direction,&
& set_name, edges, Domain, Domain2, DomainU, aux, req, tile_count, domain_position )
CHARACTER(len=*), INTENT(in) :: name !< Short name for axis
integer, DIMENSION(:), INTENT(in) :: DATA !< Array of coordinate values
CHARACTER(len=*), INTENT(in) :: units !< Units for the axis
CHARACTER(len=*), INTENT(in) :: cart_name !< Cartesian axis ("X", "Y", "Z", "T")
CHARACTER(len=*), INTENT(in), OPTIONAL :: long_name !< Long name for the axis.
CHARACTER(len=*), INTENT(in), OPTIONAL :: set_name
INTEGER, INTENT(in), OPTIONAL :: direction !< Indicates the direction of the axis
INTEGER, INTENT(in), OPTIONAL :: edges !< Axis ID for the previously defined "edges axis"
TYPE(domain1d), INTENT(in), OPTIONAL :: Domain
TYPE(domain2d), INTENT(in), OPTIONAL :: Domain2
TYPE(domainUG), INTENT(in), OPTIONAL :: DomainU
CHARACTER(len=*), INTENT(in), OPTIONAL :: aux !< Auxiliary name, can only be <TT>geolon_t</TT> or <TT>geolat_t</TT>
CHARACTER(len=*), INTENT(in), OPTIONAL :: req !< Required field names.
INTEGER, INTENT(in), OPTIONAL :: tile_count
INTEGER, INTENT(in), OPTIONAL :: domain_position
end function diag_axis_init_r_1d

! TODO: removed class(*) to avoid error in Tapenade
SUBROUTINE diag_grid_init(domain, glo_lat, glo_lon, aglo_lat, aglo_lon)
    TYPE(domain2d), INTENT(in) :: domain !< The domain to which the grid data corresponds.
    real, INTENT(in), DIMENSION(:,:) :: glo_lat !< The latitude information for the grid tile.
    real, INTENT(in), DIMENSION(:,:) :: glo_lon !< The longitude information for the grid tile.
    real, INTENT(in), DIMENSION(:,:) :: aglo_lat !< The latitude information for the a-grid tile.
    real, INTENT(in), DIMENSION(:,:) :: aglo_lon !< The longitude information for the a-grid tile.
end SUBROUTINE diag_grid_init

SUBROUTINE diag_field_add_attribute_scalar_r(diag_field_id, att_name, att_value)
    INTEGER, INTENT(in) :: diag_field_id !< ID number for field to add attribute to
    CHARACTER(len=*), INTENT(in) :: att_name !< new attribute name
    REAL, INTENT(in) :: att_value !< new attribute value
  END SUBROUTINE diag_field_add_attribute_scalar_r

  !> @brief Add a scalar integer attribute to the diag field corresponding to a given id
  SUBROUTINE diag_field_add_attribute_scalar_i(diag_field_id, att_name, att_value)
    INTEGER, INTENT(in) :: diag_field_id !< ID number for field to add attribute to
    CHARACTER(len=*), INTENT(in) :: att_name !< new attribute name
    INTEGER, INTENT(in) :: att_value !< new attribute value
  END SUBROUTINE diag_field_add_attribute_scalar_i

  !> @brief Add a scalar character attribute to the diag field corresponding to a given id
  SUBROUTINE diag_field_add_attribute_scalar_c(diag_field_id, att_name, att_value)
    INTEGER, INTENT(in) :: diag_field_id !< ID number for field to add attribute to
    CHARACTER(len=*), INTENT(in) :: att_name !< new attribute name
    CHARACTER(len=*), INTENT(in) :: att_value !< new attribute value
  END SUBROUTINE diag_field_add_attribute_scalar_c

  !> @brief Add a real 1D array attribute to the diag field corresponding to a given id
  SUBROUTINE diag_field_add_attribute_r1d(diag_field_id, att_name, att_value)
    INTEGER, INTENT(in) :: diag_field_id !< ID number for field to add attribute to
    CHARACTER(len=*), INTENT(in) :: att_name !< new attribute name
    REAL, DIMENSION(:), INTENT(in) :: att_value !< new attribute value
  END SUBROUTINE diag_field_add_attribute_r1d

  !> @brief Add an integer 1D array attribute to the diag field corresponding to a given id
  SUBROUTINE diag_field_add_attribute_i1d(diag_field_id, att_name, att_value)
    INTEGER, INTENT(in) :: diag_field_id !< ID number for field to add attribute to
    CHARACTER(len=*), INTENT(in) :: att_name !< new attribute name
    INTEGER, DIMENSION(:), INTENT(in) :: att_value !< new attribute value
  END SUBROUTINE diag_field_add_attribute_i1d

INTEGER FUNCTION register_static_field(module_name, field_name, axes, long_name, units,&
& mask_variant, standard_name, DYNAMIC, do_not_log, interp_method,&
& tile_count, area, volume, realm)
CHARACTER(len=*), INTENT(in) :: module_name, field_name
INTEGER, DIMENSION(:), INTENT(in) :: axes
CHARACTER(len=*), OPTIONAL, INTENT(in) :: long_name, units, standard_name
! TODO: remove because it isn't used and it cause and error in Tapenade
! CLASS(*), OPTIONAL, INTENT(in) :: missing_value
! CLASS(*), DIMENSION(:), OPTIONAL, INTENT(in) :: range
LOGICAL, OPTIONAL, INTENT(in) :: mask_variant
LOGICAL, OPTIONAL, INTENT(in) :: DYNAMIC
LOGICAL, OPTIONAL, INTENT(in) :: do_not_log !< if TRUE, field information is not logged
CHARACTER(len=*), OPTIONAL, INTENT(in) :: interp_method !< The interp method to be used when
                                                     !! regridding the field in post-processing.
                                                     !! Valid options are "conserve_order1",
                                                     !! "conserve_order2", and "none".
INTEGER,          OPTIONAL, INTENT(in) :: tile_count
INTEGER,          OPTIONAL, INTENT(in) :: area !< Field ID for the area field associated with this field
INTEGER,          OPTIONAL, INTENT(in) :: volume !< Field ID for the volume field associated with this field
CHARACTER(len=*), OPTIONAL, INTENT(in) :: realm !< String to set as the value to the modeling_realm attribute
end function register_static_field

! TODO: remove because it isn't used and it cause and error in Tapenade
INTEGER FUNCTION register_diag_field_scalar(module_name, field_name, init_time, &
& long_name, units, standard_name, missing_value, range, do_not_log, err_msg,&
& area, volume, realm)
CHARACTER(len=*),           INTENT(in) :: module_name   !< Module where the field comes from
CHARACTER(len=*),           INTENT(in) :: field_name    !< Name of the field
TYPE(time_type),  OPTIONAL, INTENT(in) :: init_time     !< Time to start writing data from
CHARACTER(len=*), OPTIONAL, INTENT(in) :: long_name     !< Long_name to add as a variable attribute
CHARACTER(len=*), OPTIONAL, INTENT(in) :: units         !< Units to add as a variable_attribute
CHARACTER(len=*), OPTIONAL, INTENT(in) :: standard_name !< Standard_name to name the variable in the file
REAL,         OPTIONAL, INTENT(in) :: missing_value !< Missing value to add as a variable attribute
REAL,         OPTIONAL, INTENT(in) :: range(:)      !< Range to add a variable attribute
LOGICAL,          OPTIONAL, INTENT(in) :: do_not_log    !< If TRUE, field information is not logged
CHARACTER(len=*), OPTIONAL, INTENT(out):: err_msg       !< Error_msg from call
INTEGER,          OPTIONAL, INTENT(in) :: area          !< Id of the area field
INTEGER,          OPTIONAL, INTENT(in) :: volume        !< Id of the volume field
CHARACTER(len=*), OPTIONAL, INTENT(in) :: realm         !< String to set as the modeling_realm attribute
end function register_diag_field_scalar

! TODO: remove because it isn't used and it cause and error in Tapenade
INTEGER FUNCTION register_diag_field_array(module_name, field_name, axes, init_time, &
& long_name, units, missing_value, range, mask_variant, standard_name, verbose,&
& do_not_log, err_msg, interp_method, tile_count, area, volume, realm)
CHARACTER(len=*),           INTENT(in) :: module_name   !< Module where the field comes from
CHARACTER(len=*),           INTENT(in) :: field_name    !< Name of the field
INTEGER,                    INTENT(in) :: axes(:)       !< Ids corresponding to the variable axis
TYPE(time_type),  OPTIONAL, INTENT(in) :: init_time     !< Time to start writing data from
CHARACTER(len=*), OPTIONAL, INTENT(in) :: long_name     !< Long_name to add as a variable attribute
CHARACTER(len=*), OPTIONAL, INTENT(in) :: units         !< Units to add as a variable_attribute
REAL,         OPTIONAL, INTENT(in) :: missing_value !< Missing value to add as a variable attribute
REAL,         OPTIONAL, INTENT(in) :: range(:)      !< Range to add a variable attribute
LOGICAL,          OPTIONAL, INTENT(in) :: mask_variant  !< Mask variant
CHARACTER(len=*), OPTIONAL, INTENT(in) :: standard_name !< Standard_name to name the variable in the file
LOGICAL,          OPTIONAL, INTENT(in) :: verbose       !< Print more information
LOGICAL,          OPTIONAL, INTENT(in) :: do_not_log    !< If TRUE, field information is not logged
CHARACTER(len=*), OPTIONAL, INTENT(out):: err_msg       !< Error_msg from call
CHARACTER(len=*), OPTIONAL, INTENT(in) :: interp_method !< The interp method to be used when
                                                     !! regridding the field in post-processing.
                                                     !! Valid options are "conserve_order1",
                                                     !! "conserve_order2", and "none".
INTEGER,          OPTIONAL, INTENT(in) :: tile_count    !< The current tile number
INTEGER,          OPTIONAL, INTENT(in) :: area          !< Id of the area field
INTEGER,          OPTIONAL, INTENT(in) :: volume        !< Id of the volume field
CHARACTER(len=*), OPTIONAL, INTENT(in) :: realm         !< String to set as the modeling_realm attribute

INTEGER :: field, j, ind, file_num, freq
INTEGER :: output_units
INTEGER :: stdout_unit
LOGICAL :: mask_variant1, verbose1
CHARACTER(len=128) :: msg
end function register_diag_field_array

end module diag_manager_mod