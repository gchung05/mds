`mds` 0.3.2
---------------------------------------
**UNDER DEVELOPMENT**


`mds` 0.3.1
---------------------------------------

**Implemented Updates**

*General*
- Explicit declarations of `stringsAsFactors=T` in all `data.frame()` calls for cross-version compatibility


`mds` 0.3.0
---------------------------------------

**Implemented Updates**

*General*
- Overall improvement of documentation & detailed descriptions of functions
- Input parameter `covariates` specified in `deviceevent()` must be either `factor` or `numeric`, no longer `character`.
- Input parameter `match_levels` specified in `time_series()` must be `factor`, no longer `character`.
- Add to output of `define_analyses()` a function for adding dates in the units specified by `date_level` and `date_level_n`
- Deprecated `use_hierarchy` parameter in `time_series()`
- Explicit carry-forward of define_analysis id

*Covariate Handling*
- Explicit handling of covariate level analysis in `deviceevent()`, `define_analyses()`, and `time_series()`
- Added analysis data frame to `time_series()` output when covariate analysis is requested
- Covariates can now be EITHER `factor` OR `numeric` types when defined in `deviceevent()`. Previously only characters (inferred as factors) werea allowed.
- Covariate analysis definitions include as a whole (marginal effect) or as a subset by a `factor` covariate level
- Covariate analysis is exempt if there is zero variation in the data of the covariate
- Restructured `exposure()` to handle factors only in `match_levels`.

*Implantable Devices / Time In-Vivo Handling*
- Explicit handling of time in-vivo in `deviceevent()`, `define_analyses()`, and `time_series()`
- Added analysis data frame to `time_series()` output when time in-vivo analysis is requested
- CHANGED PARAMETER `deviceevent(implant_days)` to `deviceevent(time_invivo)` and make all downstream changes.

**To-Do List**
- Feature allowing total events (concept of reporting fraction) as an exposure data frame
- Facility for handling multiple events on the same actual occurrence (such as stroke and bleeding during the same operation)
- Explicit implementation of the Unique Device Identifier (UDI)
- Implementation of IMDRF coding for product malfunctions, patient adverse events, and analysis findings

`mds` 0.2.1
---------------------------------------

**Implemented Updates**

- More descriptive `define_analyses()` and `time_series()` outputs, including explicit references to device, event, and covariate levels.
- Alongside more explicit references, corresponding updates to nLabels attribute of `time_series()`
- Outputs of `define_analyses()` and `time_series()` are clearer about the device and event one-up hierarchy levels
- Updated `plot()` method to support updates to `define_analyses()` and `time_series()`

# `mds` 0.2.0
---------------------------------------

**Implemented Updates**

- Allow for list variable type in `deviceevents()` descriptors argument
- Wrote `time_series()` examples in documentation & provided sample time series dataset called `mds_ts`
- Better cross-referenced and linked documentation

**Bugfixes**

- `exposure() total_events` parameter now accepts integer class
- `deviceevent()`: Drop records with missing `device_level`s at all hierarchy levels
- `deviceevent()`: Drop records with missing `event_level`s
- `deviceevent()`: Drop records with missing covariate levels
- `exposure()`: Drop records with missing `device_level`s at all hierarchy levels
- `exposure()`: Drop records with missing `event_level`s
- `exposure()`: Drop records with missing match levels

# `mds` 0.1.0
---------------------------------------

- Initial Release. Yay!
