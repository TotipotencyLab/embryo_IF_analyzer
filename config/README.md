# config/

Templates for the tables you fill in per analysis project. Copy one out, edit
it, and point the CLIs at your copy — nothing here is read automatically.

- `sample_sheet_template.tsv` — the sample sheet. Only `prefix` is required;
  every other column is your own metadata and is carried through onto the
  outputs, where `--group_by` can use it. `Make_SampleSheet.groovy` can
  generate one instead of you writing it by hand.
- `files_template.tsv` — the *file* table that `Make_SampleSheet.groovy` reads:
  one row per image file, with the `alias` that makes every sample prefix
  unique across files. Metadata you put here is seeded onto every series of
  that file, so it is typed once rather than per series.

The column list both tables are checked against lives in
[`../schema/sheet_columns.tsv`](../schema/sheet_columns.tsv), which is internal
and not meant to be edited per project.

The exact rules are in [`../note/data_formats.md`](../note/data_formats.md).

`tests/testthat/test-data_formats.R` loads the template through the same reader
the CLIs use, so it cannot drift away from the code without a test failing.
