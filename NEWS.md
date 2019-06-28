`mds` 0.3.0
---------------------------------------
UNDER DEVELOPMENT

*General*
- Add to output of `define_analyses()` a function for adding dates in the units specified by `date_level` and `date_level_n`
- Overall improvement of documentation & detailed descriptions of functions

*Covariate Handling*
- Explicit handling of covariate level analysis in `deviceevent()`, `define_analyses()`, and `time_series()`
- Restriction of covariates to `factor` and `numeric` types only in `deviceevent()`
- Covariate analysis definitions include as a whole (marginal effect) or as a subset by a `factor` covariate level
- Covariate analysis is exempt if there is zero variation in the data of the covariate
- Restructured `exposure()` to handle factors only in `match_levels`.

*Implantable Devices / Time In-Vivo Handling*
- More explicit handling of implantable devices (in-vivo time)
- More explicit handling of implant days by `define_analyses()` and `time_series()`
- CHANGED PARAMETER `deviceevent(implant_days)` to `deviceevent(time_invivo)` and make all downstream changes.

**Implemented Updates**

- Explicit carry-forward of define_analysis id

**Potential updates**

- Feature allowing total events (concept of reporting fraction) as an exposure data frame
- Facility for handling multiple events on the same actual occurrence (such as stroke and bleeding during the same operation)
- Explicit implementation of the Unique Device Identifier (UDI)

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
