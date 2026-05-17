# Class definition for signal cache

This class is an internal abstract class

## Methods

### Public methods

- [`FileCache$get_header()`](#method-FileCache-get_header)

- [`FileCache$get_annotations()`](#method-FileCache-get_annotations)

- [`FileCache$get_channel_table()`](#method-FileCache-get_channel_table)

- [`FileCache$get_channel()`](#method-FileCache-get_channel)

- [`FileCache$delete()`](#method-FileCache-delete)

------------------------------------------------------------------------

### `FileCache$get_header()`

Get header information, often small list object

#### Usage

    FileCache$get_header(...)

#### Arguments

- `...`:

  passed to child methods

------------------------------------------------------------------------

### `FileCache$get_annotations()`

Get annotation information, often a large table

#### Usage

    FileCache$get_annotations(...)

#### Arguments

- `...`:

  passed to child methods

------------------------------------------------------------------------

### `FileCache$get_channel_table()`

Get channel table

#### Usage

    FileCache$get_channel_table(...)

#### Arguments

- `...`:

  passed to child methods

------------------------------------------------------------------------

### `FileCache$get_channel()`

Get channel data

#### Usage

    FileCache$get_channel(x, ...)

#### Arguments

- `x`:

  channel order or label

- `...`:

  passed to child methods

#### Returns

Channel signal with time-stamps inheriting class `'ieegio_get_channel'`

------------------------------------------------------------------------

### `FileCache$delete()`

Delete file cache

#### Usage

    FileCache$delete(...)

#### Arguments

- `...`:

  passed to child methods
