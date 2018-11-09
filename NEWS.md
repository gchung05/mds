`mds` 0.2.1
---------------------------------------
UNDER DEVELOPMENT

**Potential updates**

- Feature allowing total events (concept of reporting fraction) as an exposure data frame
- Facility for handling multiple events on the same actual occurrence (such as stroke and bleeding during the same operation)
- Calculation of implant length (and improved handling of implantables overall)

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
