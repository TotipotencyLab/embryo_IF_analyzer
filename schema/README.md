# schema/

**Internal.** Files the scripts read at run time to agree with each other.

Not `config/` — that holds templates you copy out and edit, and its README
promises nothing there is read automatically. These are the opposite: never
edited per project, always read.

- `sheet_columns.tsv` — the columns of `files.tsv` and `samples.tsv`, who owns
  each one, and its type. Both the Groovy and the R side read it, which is the
  reason it exists: the same list living in two languages would drift, and the
  failure would be a silently renamed or dropped column.

The rules these encode are described in
[`../note/data_formats.md`](../note/data_formats.md).
