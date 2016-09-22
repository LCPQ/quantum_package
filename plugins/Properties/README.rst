=================
Properties Module
=================

Needed Modules
==============

.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.

.. image:: tree_dependency.png

* `Determinants <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants>`_

Needed Modules
==============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


.. image:: tree_dependency.png

* `Determinants <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants>`_

Documentation
=============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


`ao_integrated_delta_rho_all_points <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/delta_rho.irp.f#L61>`_
  array of the overlap in x,y between the AO function and integrated between [z,z+dz] in the z axis
  for all the z points that are given (N_z_pts)


`ao_integrated_delta_rho_one_point <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/delta_rho.irp.f#L130>`_
  array of the overlap in x,y between the AO function and integrated between [z,z+dz] in the z axis
  for one specific z point


`average_position <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/properties.irp.f#L1>`_
  average_position(1) = <psi_det|X|psi_det>
  average_position(2) = <psi_det|Y|psi_det>
  average_position(3) = <psi_det|Z|psi_det>


`average_spread <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/properties.irp.f#L27>`_
  average_spread(1) = <psi_det|X^2|psi_det>
  average_spread(2) = <psi_det|Y^2|psi_det>
  average_spread(3) = <psi_det|Z^2|psi_det>


`conversion_factor_cm_1_hcc <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/hyperfine_constants.irp.f#L85>`_
  Conversion factor for the calculation of the hcc, according to the nuclear charge


`conversion_factor_gauss_hcc <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/hyperfine_constants.irp.f#L84>`_
  Conversion factor for the calculation of the hcc, according to the nuclear charge


`conversion_factor_mhz_hcc <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/hyperfine_constants.irp.f#L83>`_
  Conversion factor for the calculation of the hcc, according to the nuclear charge


`delta_z <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/delta_rho.irp.f#L4>`_
  Undocumented


`diag_o1_mat_elem <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/slater_rules_mono_electronic.irp.f#L91>`_
  Computes <i|O1|i>


`diag_o1_mat_elem_alpha_beta <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/slater_rules_mono_electronic.irp.f#L210>`_
  Computes <i|O1(alpha) -O1(beta)|i>


`electronic_population_alpha <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/mulliken.irp.f#L57>`_
  spin population on the ao basis :
  spin_population(i,j) = rho_AO(alpha)(i,j) - rho_AO(beta)(i,j) * <AO_i|AO_j>


`electronic_population_beta <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/mulliken.irp.f#L58>`_
  spin population on the ao basis :
  spin_population(i,j) = rho_AO(alpha)(i,j) - rho_AO(beta)(i,j) * <AO_i|AO_j>


`filter_connected_mono <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/slater_rules_mono_electronic.irp.f#L240>`_
  Filters out the determinants that are not connected through PURE
  .br
  MONO EXCITATIONS OPERATORS (a^{\dagger}j a_i)
  .br
  returns the array idx which contains the index of the
  .br
  determinants in the array key1 that interact
  .br
  via some PURE MONO EXCITATIONS OPERATORS
  .br
  idx(0) is the number of determinants that interact with key1


`get_average <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/average.irp.f#L1>`_
  computes the average value of a pure MONO ELECTRONIC OPERATOR
  whom integrals on the MO basis are stored in "array"
  and with the density is stored in  "density"


`gross_orbital_product_alpha <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/mulliken.irp.f#L76>`_
  gross orbital product


`gross_orbital_product_beta <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/mulliken.irp.f#L77>`_
  gross orbital product


`i_o1_j <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/slater_rules_mono_electronic.irp.f#L1>`_
  Returns <i|O1|j> where i and j are determinants
  and O1 is a ONE BODY OPERATOR
  array  is the array of the mono electronic operator
  on the MO basis


`i_o1_j_alpha_beta <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/slater_rules_mono_electronic.irp.f#L158>`_
  Returns <i|O1(alpha) - O1(beta)|j> where i and j are determinants
  and O1 is a ONE BODY OPERATOR
  array  is the array of the mono electronic operator
  on the MO basis


`i_o1_psi <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/slater_rules_mono_electronic.irp.f#L52>`_
  <key|O1|psi> for the various Nstates
  and O1 is a ONE BODY OPERATOR
  array  is the array of the mono electronic operator
  on the MO basis


`i_o1_psi_alpha_beta <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/slater_rules_mono_electronic.irp.f#L119>`_
  <key|O1(alpha) - O1(beta)|psi> for the various Nstates
  and O1 is a ONE BODY OPERATOR
  array  is the array of the mono electronic operator
  on the MO basis


`i_unit_integrated_delta_rho <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/delta_rho.irp.f#L118>`_
  fortran unit for the writing of the integrated delta_rho


`integrated_delta_rho_all_points <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/delta_rho.irp.f#L15>`_
  .br
  integrated_rho(alpha,z) - integrated_rho(beta,z) for all the z points
  chosen
  .br


`integrated_delta_rho_one_point <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/delta_rho.irp.f#L212>`_
  .br
  integral (x,y) and (z,z+delta_z) of rho(alpha) - rho(beta)
  on the MO basis
  .br


`iso_hcc_cm_1 <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/hyperfine_constants.irp.f#L124>`_
  isotropic hyperfine coupling constants among the various atoms


`iso_hcc_gauss <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/hyperfine_constants.irp.f#L123>`_
  isotropic hyperfine coupling constants among the various atoms


`iso_hcc_mhz <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/hyperfine_constants.irp.f#L122>`_
  isotropic hyperfine coupling constants among the various atoms


`mo_integrated_delta_rho_one_point <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/delta_rho.irp.f#L184>`_
  .br
  array of the integrals needed of integrated_rho(alpha,z) - integrated_rho(beta,z) for z = z_one_point
  on the MO basis
  .br


`mulliken_densities_alpha <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/mulliken.irp.f#L93>`_
  .br


`mulliken_densities_beta <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/mulliken.irp.f#L94>`_
  .br


`mulliken_spin_densities <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/mulliken.irp.f#L44>`_
  ATOMIC SPIN POPULATION (ALPHA MINUS BETA)


`n_z_pts <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/delta_rho.irp.f#L1>`_
  Undocumented


`print_hcc <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/hyperfine_constants.irp.f#L138>`_
  Undocumented


`print_hcc_main <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/print_hcc.irp.f#L1>`_
  Undocumented


`print_mulliken <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/print_mulliken.irp.f#L1>`_
  Undocumented


`print_mulliken_sd <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/mulliken.irp.f#L110>`_
  Undocumented


`spin_density_at_nucleous <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/hyperfine_constants.irp.f#L1>`_
  value of the spin density at each nucleus


`spin_density_at_nucleous_contrib_mo <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/hyperfine_constants.irp.f#L53>`_
  value of the spin density at each nucleus


`spin_density_at_nucleous_contrib_mo_test <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/hyperfine_constants.irp.f#L54>`_
  value of the spin density at each nucleus


`spin_density_at_nucleous_contrib_per_mo <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/hyperfine_constants.irp.f#L22>`_
  value of the spin density at each nucleus


`spin_density_at_nucleous_from_mo <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/hyperfine_constants.irp.f#L21>`_
  value of the spin density at each nucleus


`spin_gross_orbital_product <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/mulliken.irp.f#L29>`_
  gross orbital product for the spin population


`spin_population <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/mulliken.irp.f#L2>`_
  spin population on the ao basis :
  spin_population(i,j) = rho_AO(alpha)(i,j) - rho_AO(beta)(i,j) * <AO_i|AO_j>


`spin_population_angular_momentum <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/mulliken.irp.f#L17>`_
  Undocumented


`test_average_value <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/routines_test.irp.f#L3>`_
  Undocumented


`test_average_value_alpha_beta <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/routines_test.irp.f#L25>`_
  Undocumented


`test_dm <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/routines_test.irp.f#L56>`_
  Undocumented


`z_max <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/delta_rho.irp.f#L3>`_
  Undocumented


`z_min <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/delta_rho.irp.f#L2>`_
  Undocumented


`z_one_point <http://github.com/LCPQ/quantum_package/tree/master/plugins/Properties/ezfio_interface.irp.f#L6>`_
  z point on which the integrated delta rho is calculated

