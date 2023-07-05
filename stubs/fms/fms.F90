! use fms_mod,                only: error_mesg, FATAL,                 &
!                                   check_nml_error, stdlog,           &
!                                   write_version_number,              &
!                                   mpp_clock_id, mpp_clock_begin,     &
!                                   mpp_clock_end, CLOCK_SUBCOMPONENT, &
!                                   clock_flag_default, open_namelist_file, 
!                                   close_file,lowercase, file_exist, fms_end, mpp_pe, mpp_root_pe,

module fms_mod
    
    use          mpp_mod, only:  FATAL, mpp_pe, mpp_npes, mpp_root_pe, &
                                 mpp_clock_begin, mpp_clock_end,     &
                                 mpp_clock_id,   &
                                 CLOCK_SUBCOMPONENT, CLOCK_ROUTINE,&
                                 lowercase, stdlog

    
    
    ! use  mpp_domains_mod, only:  domain2D, mpp_define_domains, &
    !                              mpp_update_domains, GLOBAL_DATA_DOMAIN, &
    !                              mpp_domains_init, mpp_domains_exit,     &
    !                              mpp_global_field, mpp_domains_set_stack_size,  &
    !                              mpp_get_compute_domain, mpp_get_global_domain, &
    !                              mpp_get_data_domain
    
    ! use       mpp_io_mod, only:  mpp_io_init, mpp_open, mpp_close,         &
    !                        MPP_ASCII, MPP_NATIVE, MPP_IEEE32, MPP_NETCDF,  &
    !                        MPP_RDONLY, MPP_WRONLY, MPP_APPEND, MPP_OVERWR, &
    !                        MPP_SEQUENTIAL, MPP_DIRECT,                     &
    !                        MPP_SINGLE, MPP_MULTI, MPP_DELETE, mpp_io_exit, &
    !                        fieldtype, mpp_get_atts, mpp_get_info, mpp_get_fields, &
    !                        do_cf_compliance
  
    use fms_io_mod, only: open_namelist_file, file_exist, field_exist, &
                          get_mosaic_tile_grid
    ! use fms_io_mod, only : fms_io_init, fms_io_exit, field_size, &
    !                        read_data, write_data, read_compressed, read_distributed, &
    !                        open_namelist_file, open_restart_file, open_ieee32_file, close_file, &
    !                        get_domain_decomp, &
    !                        open_file, open_direct_file, get_mosaic_tile_grid, &
    !                        get_mosaic_tile_file, get_global_att_value, file_exist, field_exist, &
    !                        set_domain, nullify_domain

    use fms2_io_mod, only: open_file, close_file
    ! use memutils_mod, only: print_memuse_stats, memutils_init
    ! use grid2_mod, only: grid_init, grid_end
    ! use fms_string_utils_mod, only: fms_c2f_string, fms_cstring2cpointer, string
    ! use platform_mod, only: r4_kind, r8_kind
    
    use, intrinsic :: iso_c_binding
    
    implicit none
    private
    
    public :: CLOCK_ROUTINE
    ! routines for initialization and termination of module
    public :: fms_init, fms_end
    
    ! routines for opening/closing specific types of file
    public :: open_namelist_file, &
              close_file, &
              open_file
    
    ! routines for reading/writing distributed data
    ! public :: read_data, write_data, read_compressed, read_distributed
    ! public :: get_domain_decomp, field_size
    ! public :: get_global_att_value
    
    ! routines for get mosaic information
    ! public :: get_mosaic_tile_grid, get_mosaic_tile_file
    
    ! miscellaneous i/o routines
    public :: file_exist, check_nml_error, field_exist,     &
              error_mesg
    ! version logging routine (originally from fms_io)
    public :: write_version_number
    
    ! miscellaneous utilities (non i/o)
    public :: lowercase
    
    ! public mpp interfaces
    public :: FATAL, &
              mpp_pe, mpp_npes, mpp_root_pe,   &
              stdlog
    public :: mpp_clock_id, mpp_clock_begin, mpp_clock_end
    public :: CLOCK_SUBCOMPONENT
    
    ! interface monotonic_array
    !   module procedure :: monotonic_array_r4, monotonic_array_r8
    ! end interface monotonic_array
    
    !Balaji
    !this is published by fms and applied to any initialized clocks
    !of course you can go and set the flag to SYNC or DETAILED by hand
    integer, public :: clock_flag_default
    !> @}
      !> Namelist read error values
      !> @ingroup fms_mod
      TYPE nml_errors_type
         INTEGER :: multipleNMLSinFile
         INTEGER :: badType1
         INTEGER :: badType2
         INTEGER :: missingVar
         INTEGER :: NotInFile
      END TYPE nml_errors_type
      TYPE(nml_errors_type), SAVE :: nml_errors
    !> @addtogroup fms_mod
    !> @{
    
    !------ namelist interface -------
    !------ adjustable severity level for warnings ------
    
      logical           :: read_all_pe   = .true. !< Read global data on all processors extracting local
                           !! part needed (TRUE) or read global data on PE0 and broadcast to all
                           !! PEs(FALSE).
      character(len=16) :: clock_grain = 'NONE' !< The level of clock granularity used for performance
                           !! timing sections of code. Possible values in order of increasing detail
                           !! are: 'NONE', 'COMPONENT', 'SUBCOMPONENT', 'MODULE_DRIVER', 'MODULE',
                           !! 'ROUTINE', 'LOOP', and 'INFRA'.  Code sections are defined using routines
                           !! in MPP module: mpp_clock_id, mpp_clock_begin, and mpp_clock_end. The fms
                           !! module makes these routines public. A list of timed code sections will be
                           !! printed to STDOUT. See the @ref mpp_mod module for more details.
      character(len=16) :: clock_flags='NONE' !< Possible values are 'NONE', 'SYNC', or 'DETAILED'.
                           !! SYNC will give accurate information on load balance of the clocked
                           !! portion of code. DETAILED also turns on detailed message-passing
                           !! performance diagnosis. Both SYNC and DETAILED will  work correctly on
                           !! innermost clock nest and distort outer clocks, and possibly the overall
                           !! code time. See the @ref mpp_mod module for more details.
      character(len=8)  :: warning_level = 'warning' !< Sets the termination condition for the WARNING
                           !! flag to interfaces error_mesg/mpp_error. set warning_level = 'fatal'
                           !! (program crashes for warning messages) or 'warning' (prints warning
                           !! message and continues).
      integer           :: stack_size = 0 !< The size in words of the MPP user stack. If stack_size > 0,
                           !! the following MPP routine is called: call mpp_set_stack_size (stack_size).
                           !! If stack_size = 0 (default) then the default size set by mpp_mod is used.
      integer           :: domains_stack_size = 0 !< The size in words of the MPP_DOMAINS user stack. If
                           !! domains_stack_size > 0, the following MPP_DOMAINS routine is called:
                           !! call mpp_domains_set_stack_size (domains_stack_size). If
                           !! domains_stack_size = 0 (default) then the default size set by
                           !! @ref mpp_domains_mod is used.
      logical, public   :: print_memory_usage = .FALSE. !< If set to .TRUE., memory usage statistics
                           !! will be printed at various points in the code. It is used to study memory
                           !! usage, e.g to detect memory leaks.
    
    !------ namelist interface -------
    
      namelist /fms_nml/  read_all_pe, clock_grain, clock_flags,         &
                          warning_level, stack_size, domains_stack_size, &
                          print_memory_usage
    
    !   ---- private data for check_nml_error ----
    
       integer, private :: num_nml_error_codes, nml_error_codes(20)
       logical, private :: do_nml_error_init = .true.
       private  nml_error_init
    
    
    !  ---- version number -----
    
    ! Include variable "version" to be written to log file.
    #include<file_version.h>
    
      logical :: module_is_initialized = .FALSE.
    
      logical, private :: fms_io_initialized = .FALSE.!> used to make sure fms_io version is only
                                                      !! written to log once
    
    !> @}
    
    !> @addtogroup fms_mod
    !> @{
    contains
    
    subroutine fms_init (localcomm, alt_input_nml_path)
    
    !--- needed to output the version number of constants_mod to the logfile ---
    !  use constants_mod, only: constants_version=>version !pjp: PI not computed
    !  use fms_io_mod,    only: fms_io_version
    
     integer, intent(in), optional :: localcomm
     character(len=*), intent(in), optional :: alt_input_nml_path
     integer :: ierr, io
     integer :: logunitnum
     integer :: stdout_unit !< Unit number for the stdout file
    
    end subroutine fms_init
    
    !#######################################################################
    
    !> @brief Calls the termination routines for all modules in the MPP package.
    !!
    !> Termination routine for the fms module. It also calls destructor routines
    !! for the mpp, mpp_domains, and mpp_io modules. If this routine is called
    !! more than once it will return silently. There are no arguments.
    subroutine fms_end ( )
    
    end subroutine fms_end
    
    !#######################################################################
    
     !> @brief Print notes, warnings and error messages; terminates program for warning
     !! and error messages. Usage of @ref mpp_error is preferable. (use error levels NOTE,WARNING,FATAL, see example below)
     !! @details Print notes, warnings and error messages; and terminates the program for
     !!     error messages. This routine is a wrapper around mpp_error, and is provided
     !!     for backward compatibility. This module also publishes mpp_error,
     !!      <B>users should try to use the mpp_error interface</B>.
     !!
     !! <br>Example usage:
     !! @code{.F90}
     !! use fms_mod, only: error_mesg, FATAL, NOTE
     !! call error_mesg ('fms_mod', 'initialization not called', FATAL)
     !! call error_mesg ('fms_mod', 'fms_mod message', NOTE)
     !! @endcode
     subroutine error_mesg (routine, message, level)
      character(len=*), intent(in) :: routine !< Routine name where the warning or error has occurred.
      character(len=*), intent(in) :: message !< Warning or error message to be printed.
      integer,          intent(in) :: level !< Level of severity; set to NOTE, WARNING, or FATAL Termination always occurs
                                            !! for FATAL, never for NOTE, and is settable for WARNING (see namelist).
    
     end subroutine error_mesg
    
    !#######################################################################
    
     !> @brief Facilitates the control of fatal error conditions
     !! @details When err_msg is present, message is copied into err_msg
     !!     and the function returns a value of .true.
     !!     Otherwise calls mpp_error to terminate execution.
     !!     The intended use is as shown below.
     !! @returns true when err_msg is present
     !! @code{.F90}
     !! if(fms_error_handler(routine, message, err_msg)) return
     !! @endcode
     function fms_error_handler(routine, message, err_msg)
    
     logical :: fms_error_handler
     character(len=*), intent(in) :: routine !< Routine name where the fatal error has occurred.
     character(len=*), intent(in) :: message !< fatal error message to be printed.
     character(len=*), intent(out), optional :: err_msg !< When err_msg is present: err_msg = message
    
     end function fms_error_handler
    
    ! used to check the iostat argument that is
    ! returned after reading a namelist
    ! see the online documentation for how this routine might be used
    
      !> @brief Checks the iostat argument that is returned after reading a namelist
      !!     and determines if the error code is valid.
      !! @return This function returns the input iostat value (integer) if it is an
      !!     allowable error code. If the iostat error code is not
      !!     allowable, an error message is printed and the program terminated.
      !! @details The FMS allows multiple namelist records to reside in the same file.
      !!     Use this interface to check the iostat argument that is returned after
      !!     reading a record from the namelist file. If an invalid iostat value
      !!     is detected this routine will produce a fatal error. See the NOTE below.
      !!
      !!     Some compilers will return non-zero iostat values when reading through
      !!     files with multiple namelist. This routine
      !!     will try skip these errors and only terminate for true namelist errors.
      !!
      !!     <br>Examples<br>
      !!
      !!       The following example checks if a file exists, reads a namelist input
      !!       from that file, and checks for errors in that
      !!       namelist. When the correct namelist is read and it has no errors the
      !!       routine check_nml_error will return zero and the while loop will exit.
      !!       This code segment should be used to read namelist files.
      !!       @code{.F90}
      !!         integer :: ierr, io
      !!
      !!         read (input_nml_file, fms_nml, iostat=io)
      !!         ierr = check_nml_error(io,'fms_nml')
      !!       @endcode
      !! @throws FATAL, Unknown error while reading namelist ...., (IOSTAT = ####)
      !! There was an error reading the namelist specified. Carefully examine all namelist and variables
      !! for anything incorrect (e.g. malformed, hidden characters).
      !!
      !! @throws FATAL, Unknown namelist, or mistyped namelist variable in namelist ...., (IOSTAT = ####)
      !! The name list given doesn't exist in the namelist file, or a variable in the namelist is
      !! mistyped or isn't a namelist variable.
      INTEGER FUNCTION check_nml_error(IOSTAT, NML_NAME)
        INTEGER, INTENT(in) :: IOSTAT !< The iostat value returned when reading a namelist record.
        CHARACTER(len=*), INTENT(in) :: NML_NAME !< The name of the namelist. This name will be printed if an error is
                                                 !! encountered, otherwise the name is not used.
    
        CHARACTER(len=256) :: err_str
    

      END FUNCTION check_nml_error
    
    !-----------------------------------------------------------------------
    !   private routine for initializing allowable error codes
    
      !> @brief Determines the IOSTAT error value for some common Namelist errors.
      !!   Also checks if the compiler returns a non-zero status if there are
      !!   multiple namelist records in a single file.
      SUBROUTINE nml_error_init
        ! Determines the IOSTAT error value for some common Namelist errors.
        ! Also checks if the compiler returns a non-zero status if there are
        ! multiple namelist records in a single file.
        INTEGER, PARAMETER :: unit_begin = 20, unit_end = 1024
        INTEGER :: fileunit, io_stat
        INTEGER, DIMENSION(5) :: nml_iostats
        LOGICAL :: opened
    
        ! Variables for sample namelists
        INTEGER :: i1 !< Variables for sample namelists
        INTEGER :: i2 !< Variables for sample namelists
        REAL :: r1, r2
        LOGICAL :: l1

    

      END SUBROUTINE nml_error_init
    
    !#######################################################################
    
    !> @brief match the input character string to a string
    !!     in an array/list of character strings
    !! @return If an exact match was found then true is returned, otherwise false is returned.
    !! @details Tries to find a match for a character string in a list of character strings.
    !!      The match is case sensitive and disregards blank characters to the right of
    !!      the string.
    !!
    !!      <br>Examples<br>
    !!      @code{.F90}
    !!       string = "def"
    !!       string_array = (/ "abcd", "def ", "fghi" /)
    !!
    !!       string_array_index ( string, string_array, index )
    !!      @endcode
    !!       Returns: TRUE, index = 2
    function string_array_index ( string, string_array, index ) result (found)
    character(len=*),  intent(in)  :: string !< Character string of arbitrary length.
    character(len=*),  intent(in)  :: string_array(:) !< Array/list of character strings.
    integer, optional, intent(out) :: index !< The index of string_array where the first match was found. If
                                            !! no match was found then index = 0.
    logical :: found !< If an exact match was found then TRUE is returned, otherwise FALSE is returned.
    integer :: i
    
    end function string_array_index
    
    !#######################################################################
    !> @brief Prints to the log file (or a specified unit) the version id string and
    !!  tag name.
    subroutine write_version_number (version, tag, unit)
      character(len=*), intent(in) :: version !> string that contains routine name
      character(len=*), intent(in), optional :: tag !> tag name that code was checked out with
      integer,          intent(in), optional :: unit !> alternate unit number to direct output,
                                                     !! defaults to stdlog
      integer :: logunit
    
    
    end subroutine write_version_number
    
    #include "fms_r4.fh"
    #include "fms_r8.fh"
    
    end module fms_mod