 BEGIN_PROVIDER [ integer, elec_alpha_num ]
&BEGIN_PROVIDER [ integer, elec_beta_num ]
&BEGIN_PROVIDER [ integer, elec_num ]
&BEGIN_PROVIDER [ integer, elec_num_tab, (2) ]

 implicit none
 BEGIN_DOC
 !  Numbers of alpha ("up") , beta ("down") and total electrons
 END_DOC
 PROVIDE ezfio_filename
 call ezfio_get_electrons_elec_alpha_num(elec_alpha_num)
 call ezfio_get_electrons_elec_beta_num(elec_beta_num)
 call ezfio_get_electrons_elec_num(elec_num)
 elec_num_tab(1) = elec_alpha_num
 elec_num_tab(2) = elec_beta_num
 ASSERT (elec_alpha_num > 0)
 ASSERT (elec_beta_num >= 0)
 call write_time(output_Electrons)
 call write_int(output_Electrons,elec_num,                           &
     'Number of electrons' )
 call write_int(output_Electrons,elec_alpha_num,                     &
     'Number of alpha electrons' )
 call write_int(output_Electrons,elec_beta_num,                      &
     'Number of beta electrons' )
 write(output_Electrons,*)
END_PROVIDER



