BEGIN_PROVIDER [integer, i_unit_x_two_body_dm_ab]
 implicit none
 integer :: getUnitAndOpen
 character*(128) :: file_name
 file_name = trim(trim(ezfio_filename)//'/properties/two_body_dm_x')
 i_unit_x_two_body_dm_ab = getUnitAndOpen(file_name,'w')

END_PROVIDER 

BEGIN_PROVIDER [integer, i_unit_y_two_body_dm_ab]
 implicit none
 integer :: getUnitAndOpen
 character*(128) :: file_name
 file_name = trim(trim(ezfio_filename)//'/properties/two_body_dm_y')
 i_unit_y_two_body_dm_ab = getUnitAndOpen(file_name,'w')

END_PROVIDER 

BEGIN_PROVIDER [integer, i_unit_z_two_body_extra_diag_dm_ab]
 implicit none
 integer :: getUnitAndOpen
 character*(128) :: file_name
 file_name = trim(trim(ezfio_filename)//'/properties/two_body_dm_extra_diag')
 i_unit_z_two_body_extra_diag_dm_ab = getUnitAndOpen(file_name,'w')

END_PROVIDER 

BEGIN_PROVIDER [integer, i_unit_z_two_body_diag_dm_ab]
 implicit none
 integer :: getUnitAndOpen
 character*(128) :: file_name
 file_name = trim(trim(ezfio_filename)//'/properties/two_body_dm_diag')
 i_unit_z_two_body_diag_dm_ab = getUnitAndOpen(file_name,'w')

END_PROVIDER 

BEGIN_PROVIDER [integer, i_unit_z_two_body_total_dm_ab]
 implicit none
 integer :: getUnitAndOpen
 character*(128) :: file_name
 file_name = trim(trim(ezfio_filename)//'/properties/two_body_dm_total')
 i_unit_z_two_body_total_dm_ab = getUnitAndOpen(file_name,'w')

END_PROVIDER 

