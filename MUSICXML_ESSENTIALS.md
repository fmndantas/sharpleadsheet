# MusicXML Essentials for MuseScore Rendering

This document describes the **minimum required elements** to generate MusicXML files that render correctly in MuseScore, without worrying about beams, stems, or complex graphics.

## Core Principle

**MuseScore auto-generates visual elements from semantic data.** You only need to provide:
- What notes to play (pitch + duration)
- Musical context (time signature, key, clef)

MuseScore handles beams, stems, layout automatically.

---

## Required Structure

### 1. Document Root
```xml
<?xml version="1.0" encoding="UTF-8"?>
<score-partwise version="4.0">
  <!-- content here -->
</score-partwise>
```

**Why:** Defines the MusicXML format version and structure type.

---

### 2. Part List (Header)
```xml
<part-list>
  <score-part id="P1">
    <part-name>Piano</part-name>
  </score-part>
</part-list>
```

**Required elements:**
- `id`: Unique identifier (e.g., "P1", "P2")
- `part-name`: Display name for the instrument

**Why:** Declares all instruments/voices in the score.

---

### 3. Part Content

Each `<part>` contains measures:
```xml
<part id="P1">
  <measure number="1">
    <!-- measure content -->
  </measure>
  <measure number="2">
    <!-- measure content -->
  </measure>
</part>
```

**Required:**
- `id` matches `score-part` id
- `number` attribute on each measure (sequential)

---

### 4. Measure Attributes (First Measure)

```xml
<attributes>
  <divisions>1</divisions>
  <key>
    <fifths>0</fifths>
  </key>
  <time>
    <beats>4</beats>
    <beat-type>4</beat-type>
  </time>
  <clef>
    <sign>G</sign>
    <line>2</line>
  </clef>
</attributes>
```

#### 4.1 Divisions
**What:** How many "ticks" per quarter note.
**Examples:**
- `1` = if smallest note is quarter
- `2` = if you have eighth notes  
- `4` = if you have sixteenth notes

**Formula:** Set divisions to make all your durations integers.

#### 4.2 Key Signature
```xml
<key>
  <fifths>0</fifths>
</key>
```

**Fifths value:**
- `0` = C major / A minor
- `1` = G major (1 sharp)
- `2` = D major (2 sharps)
- `-1` = F major (1 flat)
- `-2` = Bb major (2 flats)

#### 4.3 Time Signature
```xml
<time>
  <beats>4</beats>
  <beat-type>4</beat-type>
</time>
```

**Examples:**
- `4/4`: beats=4, beat-type=4
- `3/4`: beats=3, beat-type=4
- `6/8`: beats=6, beat-type=8

#### 4.4 Clef
```xml
<clef>
  <sign>G</sign>
  <line>2</line>
</clef>
```

**Common clefs:**
- Treble: sign=G, line=2
- Bass: sign=F, line=4
- Alto: sign=C, line=3

---

### 5. Notes

```xml
<note>
  <pitch>
    <step>C</step>
    <alter>1</alter>  <!-- optional: 1=sharp, -1=flat -->
    <octave>4</octave>
  </pitch>
  <duration>1</duration>
  <type>quarter</type>
  <dot/>  <!-- optional: for dotted notes -->
</note>
```

#### 5.1 Pitch
**step:** Letter name (C, D, E, F, G, A, B)
**alter:** Chromatic alteration (1=sharp, -1=flat, 0=natural)
**octave:** Octave number (4 = middle C octave)

#### 5.2 Duration
**Integer value** based on divisions.

**Example (divisions=2):**
- Quarter note = 2
- Eighth note = 1
- Half note = 4
- Dotted quarter = 3

**Formula:** `duration = (note_length / quarter_note_length) × divisions`

**Question: Is it ok to consider that division is always 32?**

**Answer: No, don't hardcode divisions to 32.** While it would work, it's inefficient and creates unnecessarily large numbers in your XML.

**Better approach:** Calculate divisions dynamically based on the smallest note duration in each measure (exactly what your `Measure.defineDivisions` function already does!):

- If smallest note is **quarter** → divisions = 1
- If smallest note is **eighth** → divisions = 2  
- If smallest note is **sixteenth** → divisions = 4
- If you have **dotted notes**, multiply by 2 or 3 to avoid fractions

**Why this matters:**
- ✅ Smaller numbers = cleaner, more readable XML
- ✅ Standard practice (MuseScore, Finale, Sibelius all do this)
- ✅ Better compatibility with other software
- ✅ Your code already calculates the optimal value!

**Example:** A measure with only quarter notes:
- divisions=1: durations are 1, 1, 1, 1 ✅ clean
- divisions=32: durations are 32, 32, 32, 32 ❌ unnecessarily complex

#### 5.3 Type
Visual note appearance:
- `whole`, `half`, `quarter`, `eighth`, `16th`, `32nd`

**Important:** This is visual only. The actual duration comes from `<duration>`.

**Is this field obligatory?** 
**No**, but it's **highly recommended**. If you omit `<type>`, MuseScore will calculate the visual appearance from the `<duration>` value, but it may not always render as you expect, especially with complex rhythms or tuplets. Always include it for predictable rendering.

#### 5.4 Dot
```xml
<dot/>
```
Add for dotted notes (appears once per dot).

---

### 6. Rests

```xml
<note>
  <rest/>
  <duration>2</duration>
  <type>quarter</type>
</note>
```

**Difference from notes:** Replace `<pitch>` with `<rest/>`.

---

### 7. Ties

```xml
<!-- First note -->
<note>
  <pitch>...</pitch>
  <duration>2</duration>
  <type>quarter</type>
  <tie type="start"/>
  <notations>
    <tied type="start"/>
  </notations>
</note>

<!-- Second note -->
<note>
  <pitch>...</pitch>
  <duration>2</duration>
  <type>quarter</type>
  <tie type="stop"/>
  <notations>
    <tied type="stop"/>
  </notations>
</note>
```

**Both required:**
- `<tie>`: Playback (MIDI)
- `<tied>`: Visual rendering

---

## What MuseScore Handles Automatically

✅ **Beaming** - Groups eighth/sixteenth notes by beat
✅ **Stem direction** - Up/down based on pitch
✅ **Note spacing** - Proportional layout
✅ **Accidentals** - Shows sharps/flats when needed
✅ **Barlines** - Added between measures
✅ **Staff lines** - Standard 5-line staff

---

## Updating Mid-Score

Only include `<attributes>` when something changes:

```xml
<measure number="1">
  <attributes>
    <!-- initial setup -->
  </attributes>
  <note>...</note>
</measure>

<measure number="2">
  <!-- no attributes = use previous measure's settings -->
  <note>...</note>
</measure>

<measure number="3">
  <attributes>
    <key>
      <fifths>2</fifths>  <!-- key change only -->
    </key>
  </attributes>
  <note>...</note>
</measure>
```

---

## Mapping Your Domain Types

### From `Duration.T` to MusicXML

```
Your Type         -> type        duration (div=4)
-------------------------------------------------
Whole             -> "whole"     16
Half              -> "half"      8
Quarter           -> "quarter"   4
Eighth            -> "eighth"    2
Sixteenth         -> "16th"      1
QuarterDotted     -> "quarter"   6 (+ <dot/>)
```

### From `NoteName.T` to MusicXML

```
Your Type    -> step   alter
-----------------------------
C            -> "C"    0 (omit)
CSharp       -> "C"    1
DFlat        -> "D"    -1
D            -> "D"    0
E            -> "E"    0
F            -> "F"    0
FSharp       -> "F"    1
G            -> "G"    0
AFlat        -> "A"    -1
A            -> "A"    0
BFlat        -> "B"    -1
B            -> "B"    0
```

### From `Clef` to MusicXML

```
Your Type    -> sign   line
---------------------------
G            -> "G"    2
F            -> "F"    4
```

---

## Testing Your Output

### 1. Create a test file
Save as `test.musicxml`

### 2. Open in MuseScore
```bash
musescore test.musicxml
```

### 3. What to check
- ✅ Notes appear at correct pitches
- ✅ Rhythms are correct
- ✅ Beams group logically (auto-generated)
- ✅ Measure durations match time signature

### 4. Common issues

**Problem:** Notes don't align to beats
**Fix:** Check your `duration` values sum to time signature

**Problem:** Wrong note heads
**Fix:** `<type>` should match duration meaning (quarter, eighth, etc.)

**Problem:** Accidentals missing
**Fix:** Add `<alter>` in `<pitch>`

---

## Minimal Complete Example

```xml
<?xml version="1.0" encoding="UTF-8"?>
<score-partwise version="4.0">
  <part-list>
    <score-part id="P1">
      <part-name>Piano</part-name>
    </score-part>
  </part-list>
  <part id="P1">
    <measure number="1">
      <attributes>
        <divisions>2</divisions>
        <key>
          <fifths>0</fifths>
        </key>
        <time>
          <beats>4</beats>
          <beat-type>4</beat-type>
        </time>
        <clef>
          <sign>G</sign>
          <line>2</line>
        </clef>
      </attributes>
      <note>
        <pitch>
          <step>C</step>
          <octave>4</octave>
        </pitch>
        <duration>2</duration>
        <type>quarter</type>
      </note>
      <note>
        <pitch>
          <step>D</step>
          <octave>4</octave>
        </pitch>
        <duration>1</duration>
        <type>eighth</type>
      </note>
      <note>
        <pitch>
          <step>E</step>
          <octave>4</octave>
        </pitch>
        <duration>1</duration>
        <type>eighth</type>
      </note>
      <note>
        <rest/>
        <duration>2</duration>
        <type>quarter</type>
      </note>
      <note>
        <pitch>
          <step>G</step>
          <octave>4</octave>
        </pitch>
        <duration>2</duration>
        <type>quarter</type>
      </note>
    </measure>
  </part>
</score-partwise>
```

**Result:** One measure of 4/4 time with: quarter C, eighth D-E (beamed), quarter rest, quarter G.

---

## Summary: What You Need to Implement

1. **Header generation** - part-list with IDs and names
2. **Attributes** - divisions, key (fifths), time, clef
3. **Note conversion** - pitch (step/alter/octave), duration, type
4. **Rest conversion** - same as notes but with `<rest/>`
5. **Tie support** - add `<tie>` and `<tied>` elements
6. **Dotted notes** - add `<dot/>` element

**That's it.** MuseScore renders everything else beautifully.
