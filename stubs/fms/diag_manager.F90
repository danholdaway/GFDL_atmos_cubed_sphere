module diag_manager_mod

    USE time_manager_mod, ONLY: time_type
    implicit none
    INTERFACE register_diag_field
    MODULE PROCEDURE register_diag_field_scalar
    MODULE PROCEDURE register_diag_field_array
 END INTERFACE
    
contains

INTEGER FUNCTION register_diag_field_scalar(module_name, field_name, init_time, &
& long_name, units, missing_value, range, standard_name, do_not_log, err_msg,&
& area, volume, realm)
CHARACTER(len=*),           INTENT(in) :: module_name   !< Module where the field comes from
CHARACTER(len=*),           INTENT(in) :: field_name    !< Name of the field
TYPE(time_type),  OPTIONAL, INTENT(in) :: init_time     !< Time to start writing data from
CHARACTER(len=*), OPTIONAL, INTENT(in) :: long_name     !< Long_name to add as a variable attribute
CHARACTER(len=*), OPTIONAL, INTENT(in) :: units         !< Units to add as a variable_attribute
CHARACTER(len=*), OPTIONAL, INTENT(in) :: standard_name !< Standard_name to name the variable in the file
CLASS(*),         OPTIONAL, INTENT(in) :: missing_value !< Missing value to add as a variable attribute
CLASS(*),         OPTIONAL, INTENT(in) :: range(:)      !< Range to add a variable attribute
LOGICAL,          OPTIONAL, INTENT(in) :: do_not_log    !< If TRUE, field information is not logged
CHARACTER(len=*), OPTIONAL, INTENT(out):: err_msg       !< Error_msg from call
INTEGER,          OPTIONAL, INTENT(in) :: area          !< Id of the area field
INTEGER,          OPTIONAL, INTENT(in) :: volume        !< Id of the volume field
CHARACTER(len=*), OPTIONAL, INTENT(in) :: realm         !< String to set as the modeling_realm attribute
end function register_diag_field_scalar

INTEGER FUNCTION register_diag_field_array(module_name, field_name, axes, init_time, &
& long_name, units, missing_value, range, mask_variant, standard_name, verbose,&
& do_not_log, err_msg, interp_method, tile_count, area, volume, realm)
CHARACTER(len=*),           INTENT(in) :: module_name   !< Module where the field comes from
CHARACTER(len=*),           INTENT(in) :: field_name    !< Name of the field
INTEGER,                    INTENT(in) :: axes(:)       !< Ids corresponding to the variable axis
TYPE(time_type),  OPTIONAL, INTENT(in) :: init_time     !< Time to start writing data from
CHARACTER(len=*), OPTIONAL, INTENT(in) :: long_name     !< Long_name to add as a variable attribute
CHARACTER(len=*), OPTIONAL, INTENT(in) :: units         !< Units to add as a variable_attribute
CLASS(*),         OPTIONAL, INTENT(in) :: missing_value !< Missing value to add as a variable attribute
CLASS(*),         OPTIONAL, INTENT(in) :: range(:)      !< Range to add a variable attribute
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