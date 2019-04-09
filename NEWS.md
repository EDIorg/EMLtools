# EMLassemblyline 2.99.0

#### 2019-03-31
* __Enhancement:__ Release `read_files()`, a function to read metadata templates and data files into an R list structure as an alternative input to `EMLassemblyline` functions (i.e. rather than supplying the files themselves). This function is primarily used for testing and demonstration purposes however, this approach is generally useful for interfacing upstream sources with `EMLassemblyline` (e.g. a metabase or database).
* __Enhancement:__ Release `create_directory()`, a function to create a commonly used directory structure for EMLassemblyline inputs and outputs.
* __Bug fix:__ `import_templates()` no longer fails to import template to empty directories.
* __Bug fix:__ `import_templates()` can now be run on datasets that don't have data tables.

#### 2019-03-14
* __Bug fix:__ Add `//otherEntity/physical/distribution/online/url`. This element was missing though documentation implied it existed.

#### 2019-03-11
* __Refactor:__ Rename arguments to align with EML element names.
    * `data.files` -> `data.table`
    * `data.files.description` -> `data.table.description`
    * `data.files.quote.character` -> `data.table.quote.character`
    * `data.files.url` -> `data.url`

#### 2019-03-02
* __Enhancement:__ Use arguments `data.table.description` and `other.entity.description` for `//dataTable/entityName` and `//otherEntity/entityName`, respectively. This provides a more meaningful file description than the file name it self.
* __Refactor:__ Rename arguments to align with EML element names.
    * `zip.dir` -> `other.entity`
    * `zip.dir.description` -> `other.entity.description`
* __Enhancement:__ Add change log!