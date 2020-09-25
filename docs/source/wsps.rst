
.. _wsps:

WRF-SUEWS Pre-processor
=========================

.. TODO: #84 this WSPS, including text and code, needs to be revised to make the steps more general.
.. the current workflow is very London specific.

Here are typical preprocessing steps needed for `wrfinput` files to be ready for WRF-SUEWS runs:

Example: UK runs preprocessors from `here <https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/blob/2dcfb9bb5f208c3a0e39c1ad0d6bb3d283a88eee/input-processor/pre-processor-UK/WRF-SUEWS-preprocessor-UK.py#L11-L21>`_.

.. code-block:: python

    steps = {
        'clean_dirs': 1,
        'extract_params_cities': 1,
        'extract_params_vegs': 1,
        'extract_params_extra_lands': 1,
        'modify_trans': 1,
        'change_to_SUEWS': 1,
        'modify_London': 1,
        'parameters': 1,
        'timezone': 0,
        }


.. option:: clean_dirs

    cleaning directories in `output <https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/output>`_ folder before running the `main pre-processor code <https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/blob/master/input-processor/pre-processor-UK/WRF-SUEWS-preprocessor-UK.py>`_ .

.. option:: extract_params_cities

    spinning up SUEWS for cities  based on cities characteristics in the [runs folder](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/runs) - For all domains

.. option:: extract_params_vegs

    - spinning up SUEWS for pure vegetation grids (main land cover)
    - For all domains

.. option:: extract_params_extra_lands

    - spinning up SUEWS for other categories of land cover (combination of different land covers)
    - For all domains

.. option:: modify_trans

    - modifying transmissivity
    - For all domains

.. option:: change_to_SUEWS

    - modifying the variables in ``wrfinput`` files and adding SUEWS variables
    - For all domains

.. option:: modify_London

    - modifying ``wrfinput`` variables related to greater London area (land fraction, building, vegetation height, QF coefficients etc.)

    - For most inner domain

.. option:: parameters

    - modifying parameters related to non-urban areas such as albedo, LAI, conductances based on `Omidvar et al. (2020) <https://gmd.copernicus.org/preprints/gmd-2020-148/>`_
    - For all domains.

.. option:: timezone

    - modifying grids timezone (might not work correctly because of the python package problem)
    - For all domains
    .. note:: It is recommended to specify the time-zone in the SUEWS runs folder, so the `timezone` variable is assigned correctly in `wrf-input` files


Each of the above steps is related to a utility function located `here <https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/utility>`_.

The input files (`wrf-inputs`) as well as non-urban parameters, and templates for `namelist.suews` and `SUEWS_param.json` are located in the `input folder <https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/input>`_.

Data related to land cover and building and vegetation height is located in the `Data folder <https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/data>`_.

Different versions of `wrfinput` files are saved in the `output folder <https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/output>`_ during the run. The final output to be used for WRF-SUEWS runs will be written in the `final folder <https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/output/final>`_.