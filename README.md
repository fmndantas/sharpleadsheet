## sharpleadsheet

SharpLeadsheet is a **CLI (Command Line Interface)** tool for rapidly converting plain text musical notation into **MusicXML** files.
It aims to provide a simple and intuitive input syntax, combined with clear and helpful user feedback.

### Workflow

```
---------------    ------------------    -----------------------
| `.sls` file | -> | sharpleadsheet | -> | (music) `.xml` file |
---------------    ------------------    -----------------------
```

Once generated, the `.xml` file can be imported into MuseScore (or any other music notation software) for further editing.

## Features

### ✅ Phase 1: Completed

| Feature | Status |
| :--- | :---: |
| Single part and staff support | ✅ |
| G and F clefs | ✅ |
| Arbitrary time signatures | ✅ |
| Arbitrary key signatures | ✅ |
| Half, quarter, eighth, and sixteenth notes (no tuplets) | ✅ |
| Sharp and flat notes | ✅ |
| Tied notes | ✅ |
| Final barline | ✅ |

### ⏳ Phase 2: In Progress

| Feature | Status |
| :--- | :---: |
| Chord symbols | ✅ |
| Text attached to voice entry | ✅ |
| Rhythmic notation | ✅ |
| Simple repeats | ✅ |
| Repeats with endings | ⏳ In Progress |
| Slash notation | ⏳ In Progress |
| Double barlines | ⏳ In Progress |
| Rehearsal marks | ⏳ In Progress |

## How to Use

Currently, you can run the tool using the `dotnet run` command:

```bash
dotnet run -- your_music.sls -o /tmp/
```

Detailed documentation for the input syntax will be added soon.

In the meantime, please check the **`Samples`** folder for example input files.

## Why SharpLeadsheet?

While other text-based music notation tools like [LilyPond](http://lilypond.org/), [ABC Notation](https://abcnotation.com/), and [MusicXML](https://www.w3.org/2021/06/musicxml40/) are powerful and feature-rich, they can be complex to learn and use.

**SharpLeadsheet focuses on simplicity!**

## Contributing

SharpLeadsheet is an **open source** project, and contributions are welcome!

You can contribute by:

1.  Opening an **Issue** to report bugs or suggest new features.
2.  Opening a **Pull Request** to address existing issues.
