# Class definition for signal cache

This class is an internal abstract class

## Methods

### Public methods

- [`SignalDataCache$get_header()`](#method-FileCache-get_header)

- [`SignalDataCache$get_annotations()`](#method-FileCache-get_annotations)

- [`SignalDataCache$get_channel_table()`](#method-FileCache-get_channel_table)

- [`SignalDataCache$get_channel()`](#method-FileCache-get_channel)

- [`SignalDataCache$delete()`](#method-FileCache-delete)

------------------------------------------------------------------------

### Method `get_header()`

Get header information, often small list object

#### Usage

    SignalDataCache$get_header(...)

#### Arguments

- `...`:

  passed to child methods

------------------------------------------------------------------------

### Method `get_annotations()`

Get annotation information, often a large table

#### Usage

    SignalDataCache$get_annotations(...)

#### Arguments

- `...`:

  passed to child methods

------------------------------------------------------------------------

### Method `get_channel_table()`

Get channel table

#### Usage

    SignalDataCache$get_channel_table(...)

#### Arguments

- `...`:

  passed to child methods

------------------------------------------------------------------------

### Method `get_channel()`

Get channel data

#### Usage

    SignalDataCache$get_channel(x, ...)

#### Arguments

- `x`:

  channel order or label

- `...`:

  passed to child methods

#### Returns

Channel signal with time-stamps inheriting class `'ieegio_get_channel'`

------------------------------------------------------------------------

### Method `delete()`

Delete file cache

#### Usage

    SignalDataCache$delete(...)

#### Arguments

- `...`:

  passed to child methods
