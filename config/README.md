# config/

Templates for the tables you fill in per analysis project. Copy one out, edit
it, and point the CLIs at your copy — nothing here is read automatically.

- `sample_sheet_template.tsv` — the sample sheet. Only `prefix` is required;
  every other column is your own metadata and is carried through onto the
  outputs, where `--group_by` can use it.

The exact rules are in [`../note/data_formats.md`](../note/data_formats.md).

`tests/testthat/test-data_formats.R` loads the template through the same reader
the CLIs use, so it cannot drift away from the code without a test failing.
