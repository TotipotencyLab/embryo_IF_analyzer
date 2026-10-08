# Luxendo TruLive3D output — what is in it and what can read it

Knowledge note. Describes an *external* format this repo consumes; it is not a
description of anything this repo writes (that is `note/data_formats.md`).

Vendor spelling is **Luxendo** (Bruker Luxendo). Every acquisition JSON in the
dataset self-identifies as `"Luxendo TruLive3D, Embedded v3.17.3"`. `luxando`
is a typo worth not propagating into filenames.

Everything below was measured on
`/Volumes/toti-ssd-SR/imaging/luxendo/2026-09-10_184731` (33 GB) on 2026-09-29,
with Fiji's own jars (Bio-Formats 8.1.1, JHDF5 19.04.1).

## 1. Directory layout

```
2026-09-10_184731/
  raw/                     33 GB   the ONLY real pixels
    stack_<N>-<label> pos<M>_channel_<C>-<name>_obj_bottom/
      Cam_long_0000<T>.lux.h5      one file per timepoint
      Cam_long_0000<T>.json        its metadata, same content as /metadata inside
  main_raw.lux.h5         195 KB   link-only index over raw/
  bdv.h5 + bdv.xml        407 KB   BigDataViewer, link-only
  ims/stacks/imaris_<N>.ims  59 KB each, one per position, link-only
  ims/sizes/imaris_tp-0_ch-0_st-<N>_....ims
```

**The single most important structural fact: everything except `raw/` is built
from HDF5 external links.** `main_raw.lux.h5`, `bdv.h5` and every `.ims` are a
few hundred KB of pointers into `raw/`. They are virtual containers, so they are
free to create and worthless to copy without `raw/` beside them, and a move that
breaks the relative path silently empties them.

Link targets are *relative*:

```
/t00000/s00/0/cells  -> raw/stack_0-L26A pos1_channel_0-BF_obj_bottom/Cam_long_00000.lux.h5 :: Data
imaris_0.ims  DataSet/ResolutionLevel 0/TimePoint 0/Channel 0/Data
             -> ../../raw/stack_0-L26A pos1_channel_1-GFP_obj_bottom/Cam_long_00000.lux.h5 :: Data
```

Note the `.ims` channel order is **not** the Luxendo channel order: Imaris
`Channel 0` is Luxendo `channel_1-GFP`, `Channel 1` is `channel_2`, `Channel 2`
is `channel_0-BF`. Anything reading the `.ims` and assuming channel index means
what the directory name says will be wrong without looking wrong.

## 2. Inside one `.lux.h5`

Two objects at the root, and nothing else:

| path | shape | notes |
|---|---|---|
| `/Data` | `[z, y, x]` | uint16, **unsigned**, chunked `[nz, 64, 64]`, uncompressed |
| `/metadata` | scalar string | the full acquisition JSON, byte-identical to the sidecar `.json` |

`/Data` carries one attribute, `element_size_um`, a 3-vector in **`[z, y, x]`
order** — the ImageJ HDF5 convention, not `[x, y, z]`:

```
element_size_um = [5.0, 0.208, 0.208]
```

File size is `nz * ny * nx * 2` plus ~94 KB of HDF5 overhead, confirming no
compression. Whole-dataset read of a 312 MiB stack: 465 ms. Plane-by-plane is
~5x slower (2.5 s for 39 planes), not the 39x the chunk shape suggests — HDF5
serves partial chunks cheaply because the data are uncompressed.

`LuxendoFile.volume()` reads the whole stack in strips one chunk tall across
every slice, so each chunk is read once; the batch reads its frames that way.
**Over the samba mount the method does not matter** (measured 2026-10-04 on
2026-09-10_203030 position 1): one frame — three channels, 981 MB — took 84–88 s
in strips and 89 s plane by plane, ~11.5 MB/s either way. The network is the
cost there, ~85 s a frame or ~2.3 h of reading for a 96-frame position, so a
position to be run more than once is worth copying to local disk first.

## 3. The JSON

Four top-level keys: `processingInformation`, `metaData`, `cameraData`,
`imagingBranch`. Everything a gatherer needs is in the first:

```
processingInformation:
  image_id             "2026-09-10T17:47:37.268Z-<uuid>"
  stack                "0"            stack_description  "L26A pos1"
  channel              "1"            channel_description "GFP"
  time_point           "0"
  objective            "bottom"       camera              "long"
  voxel_size_um        {width: 0.208, height: 0.208, depth: 5.0}
  image_size_vx        {width: 2048,  height: 2048,  depth: 39}
  affine_to_sample     3x4 transform to sample space
```

`channel_description` is **absent for some channels** — in this dataset
`channel_2` has none. Channel identity therefore cannot be taken from the JSON
alone for every channel.

## 4. The 2026-09-10 dataset, as an example of the shape

14 positions x 3 channels x 4 timepoints = 168 `.lux.h5` files.

- positions: `L26A pos1-5`, `D17 pos1-4`, `fucci pos1-5`
- channels: `channel_0-BF`, `channel_1-GFP`, `channel_2` (unnamed)
- 2048 x 2048, uint16, 0.208 x 0.208 x 5.0 um, 150 ms exposure
- **z differs per position**: 39, 29, 20, 16, 23, 24, **1**, 25, 21, 29, 31, 27, 38, 21

`L26A pos3` has **z = 1**. A single plane, so `pixel_depth` must be written
blank, per the rule in `note/data_formats.md`. Any gatherer that assumes a
uniform z across positions truncates or crashes here.

## 5. What Bio-Formats can read — measured, not assumed

Bio-Formats 8.1.1 as shipped in Fiji:

| input | result |
|---|---|
| `raw/**/Cam_long_0000N.lux.h5` | `UnknownFormatException` — **no Luxendo reader exists** |
| `main_raw.lux.h5` | `UnknownFormatException` |
| `ims/stacks/imaris_N.ims` | `ImarisHDFReader` throws `ArrayIndexOutOfBoundsException: 3` |
| `bdv.xml` | opens, and **returns the wrong pixels** — see below |

So `.lux.h5` must be read with JHDF5 directly. `ch.systemsx.cisd.hdf5` is
already in Fiji and reads these files without any added dependency.

**By hand, in the GUI:** drag-and-drop goes to Bio-Formats and fails as above.
Fiji's bundled HDF5 plugin (`HDF5_Vibez`) does open one — **File › Import ›
HDF5…**, dataset `/Data` — because `.lux.h5` is laid out in that plugin's own
convention: `/Data` in `[z, y, x]` with `element_size_um`. Verified 2026-10-02
through its scriptable twin (`Scriptable load HDF5...`, `datasetnames=/Data`):
2048×2048, 39 slices, 16-bit, calibration 0.208 × 0.208 × 5.0 µm, and a sampled
pixel equal to `LuxendoFile`'s. One file is one channel at one time point; for
all channels of a series, `Make_LuxendoTiff` with `frames`.
BigDataViewer (*Plugins › BigDataViewer › Open XML/HDF5* on `bdv.xml`) may browse
the whole acquisition — **untested**: the wrong-specimen bug below is Bio-Formats'
`BDVReader`, not BigDataViewer, but nothing here has checked BigDataViewer either.

### ⚠️ `BDVReader` is unusable on this data, and fails silently

`bdv.xml` looks like the answer: it opens, reports **14 series** (one per
position), with correct per-series dimensions and calibration:

```
[s0] name=P_t00000, W_s00_0  2048x2048 z=39 c=3 t=4  px=0.208um  pz=5.0um
```

Every one of those series returns the pixels of `stack_9-fucci pos1`. Verified
to the exact mean and max, with a freshly constructed reader per series to rule
out reader-state reuse:

```
series  claims z  actually returns     real z  readable?
0       39        stack_9-fucci pos1   21      only z 0..20
4       23        stack_9-fucci pos1   21      only z 0..20
8       21        stack_9-fucci pos1   21      all
13      21        stack_9-fucci pos1   21      all
```

Mechanism: it resolves a plane by **channel alone**, ignoring the setup, so
`c=0,1,2` always land on setups `s39/s40/s41` — the last triple, which is
stack_9. Confirmed for all three channels.

**The links in `bdv.h5` are correct** (`/t00000/s00/0/cells` -> stack_0 BF,
`/t00000/s39/0/cells` -> stack_9 BF), so this is a Bio-Formats bug, not a bad
export. Not reported upstream yet.

The failure mode is the one this repo exists to guard against: for z below
stack_9's depth it hands back plausible, correctly calibrated, wrong-specimen
data, and only throws once you read past z=21.

## 6. Writers available for the converted output

Probed in the same Fiji:

| | status |
|---|---|
| `loci.formats.out.OMETiffWriter` | present, with `setBigTiff` **and** `getCompanion` |
| `loci.formats.out.TiffWriter` | present, `setBigTiff` |
| `loci.formats.in.ZarrReader` | **missing** — listed in `readers.txt` as `[type=external]`, jar not shipped |
| N5 / N5Zarr writers (saalfeldlab) | present, but nothing here can read them back |

Zarr and N5 are therefore write-only dead ends in this install. Classic TIFF
caps at 4 GB; one position x 4 timepoints at full resolution is 3.93 GB, so the
ceiling is real and close.

## 7. `bdv.h5` + `bdv.xml` as a file list — readable, and not a source of identity

Bio-Formats cannot read *pixels* through `bdv.xml` (§5), but the two files are
still worth reading directly, as an **index**: `bdv.h5` holds one HDF5 external
link per (time point, setup) at `/t<NNNNN>/s<NN>/0/cells`, pointing at the raw
file, and `bdv.xml` holds each setup's size and voxel size. Read without
dereferencing a link, that is the complete file list of the acquisition without
listing `raw/`. `LuxendoIndex.groovy` reads it; `LuxendoScan` uses it by default
(`listing=auto`).

Measured on both acquisitions (33 GB and 800 GB, over samba, 2026-10-02):

| | 168 files | 4032 files |
|---|---|---|
| links, every setup at every time point, none missing | 168 = 42 × 4 | 4032 = 42 × 96 |
| sidecar `time_point` == the link's `t` | 42 / 42 dirs | 42 / 42 |
| `bdv.xml` channel, size, voxel == sidecar | 42 / 42 | 42 / 42 |
| stack in the setup **name** (`st:N`) == sidecar `stack` | 42 / 42 | 42 / 42 |
| **`<tile>` == sidecar `stack`** | **6 / 42** | **6 / 42** |

**What it holds:** which file is which (time point, setup), sizes, voxel sizes,
channel index. **What it does not:** the position label (`L26A pos1`), the
channel name, and the stack number anywhere but inside a setup *name*. So the
index is a **file list and a cross-check**; identity still comes from one
sidecar per directory.

⚠️ **`<tile>` is not the stack.** Setups are numbered in *text* order of the
stack — `st:0, st:1, st:10, st:11, st:12, st:13, st:2, …` — and `tile` is that
ordinal, so it matches the stack only for stacks 0 and 1. Keying on it would
give `fucci pos2` (stack 10) the identity of `L26A pos3` (stack 2), and nothing
would look wrong.

⚠️ **It is written at the end of the acquisition.** All three index files
(`bdv.xml`, `bdv.h5`, `main_raw.lux.h5`) carry one timestamp, 83 s after the
last raw file. A crashed or still-running acquisition may have no index or a
stale one, and the index route cannot see a file it does not list — which is
what `listing=walk` is for.

⚠️ **The sidecar's planned count is not the actual one.** `metaData.triggers`
records `repeats: 96` for the 33 GB acquisition, which holds 4 time points: it
was stopped early. Do not use it as a completeness check. The same block holds
`interval_s` (1800 here) — the frame interval, which the time-axis work will
want as the time calibration.

**Cost, one session, the 4032-file acquisition over samba:**

| step | time |
|---|---|
| parse `bdv.h5` in place over samba | 5.7–6.2 s |
| copy `bdv.h5` + `bdv.xml` locally, then parse | 0.7 s |
| read one sidecar per directory (42) | 1.8–4.5 s |
| `length()` of each of the 4032 files | **51–89 s** |
| v0.6.0 directory walk, `quickScan` | 175–231 s |
| `Make_LuxendoSheets`, index route, end to end | 77–79 s |

The walk took 175–231 s here, not the ~480 s recorded in
`note/time_series_plan.md` §5.6 — network conditions vary; treat both as a
range. Which of the two routes ran first changed the other's time by a factor of
up to ~1.7, so compare within a session only.

**The per-file stat is nearly all of what the index route costs**, and it is
kept on purpose (option A in the plan): it is what gives `source_bytes`, what
proves each listed file is there, and what feeds `quickScan`'s
size-differs-from-its-siblings check. Dropping it would make the scan ~3–6 s and
move all three elsewhere — a missing file would surface when the batch opens it,
`source_bytes` would become optional, and a time point of a different depth would
rest on `bdv.xml`'s one size per setup. Recorded as option B in
`note/time_series_plan.md`, not built.
