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

Ties connect two notes of the **same pitch** to create a longer duration. Each note keeps its individual duration, but they sound as one continuous note.

#### 7.1 Basic Tie (Two Notes)

```xml
<!-- First note (tie start) -->
<note>
  <pitch>
    <step>C</step>
    <octave>4</octave>
  </pitch>
  <duration>2</duration>
  <type>quarter</type>
  <tie type="start"/>
  <notations>
    <tied type="start"/>
  </notations>
</note>

<!-- Second note (tie stop) -->
<note>
  <pitch>
    <step>C</step>
    <octave>4</octave>
  </pitch>
  <duration>2</duration>
  <type>quarter</type>
  <tie type="stop"/>
  <notations>
    <tied type="stop"/>
  </notations>
</note>
```

**Both required:**
- `<tie>`: Affects playback (MIDI) - notes sound as one
- `<tied>`: Affects visual rendering - curved line appears

**Result:** Two quarter notes tied together = sounds like a half note.

#### 7.2 Sequence of Tied Notes (3+ Notes)

For ties spanning multiple notes, middle notes need **both** start and stop:

```xml
<!-- First note: start only -->
<note>
  <pitch>
    <step>D</step>
    <octave>4</octave>
  </pitch>
  <duration>4</duration>
  <type>quarter</type>
  <tie type="start"/>
  <notations>
    <tied type="start"/>
  </notations>
</note>

<!-- Middle note: stop previous, start next -->
<note>
  <pitch>
    <step>D</step>
    <octave>4</octave>
  </pitch>
  <duration>4</duration>
  <type>quarter</type>
  <tie type="stop"/>
  <tie type="start"/>
  <notations>
    <tied type="stop"/>
    <tied type="start"/>
  </notations>
</note>

<!-- Last note: stop only -->
<note>
  <pitch>
    <step>D</step>
    <octave>4</octave>
  </pitch>
  <duration>4</duration>
  <type>quarter</type>
  <tie type="stop"/>
  <notations>
    <tied type="stop"/>
  </notations>
</note>
```

**Result:** Three quarter D notes tied together = sounds like a dotted half note.

#### 7.3 Mapping Pattern

```
Note Position    <tie> elements           <tied> elements
----------------------------------------------------------------
First            type="start"             type="start"
Middle (any)     type="stop" + "start"    type="stop" + "start"
Last             type="stop"              type="stop"
```

#### 7.4 Complete Example with Ties Across Measures

```xml
<measure number="1">
  <attributes>...</attributes>
  <!-- Regular note -->
  <note>
    <pitch><step>C</step><octave>4</octave></pitch>
    <duration>4</duration>
    <type>quarter</type>
  </note>
  
  <!-- Start tie at end of measure -->
  <note>
    <pitch><step>D</step><octave>4</octave></pitch>
    <duration>4</duration>
    <type>quarter</type>
    <tie type="start"/>
    <notations>
      <tied type="start"/>
    </notations>
  </note>
</measure>

<measure number="2">
  <!-- Continue tie in next measure -->
  <note>
    <pitch><step>D</step><octave>4</octave></pitch>
    <duration>4</duration>
    <type>quarter</type>
    <tie type="stop"/>
    <tie type="start"/>
    <notations>
      <tied type="stop"/>
      <tied type="start"/>
    </notations>
  </note>
  
  <!-- End tie -->
  <note>
    <pitch><step>D</step><octave>4</octave></pitch>
    <duration>4</duration>
    <type>quarter</type>
    <tie type="stop"/>
    <notations>
      <tied type="stop"/>
    </notations>
  </note>
</measure>
```

**Result:** D note tied across measures - sounds for 3 quarter notes total.

#### 7.5 Important Notes

- **Same pitch required**: All tied notes must have identical `<step>`, `<alter>`, and `<octave>`
- **Order matters**: `<tie>` elements appear before `<notations>` section
- **Both required**: Always include both `<tie>` (playback) and `<tied>` (visual)
- **Measure boundaries**: Ties work seamlessly across measure boundaries

---

### 8. Chord Symbols

Chord symbols (like C, Dm7, G7) appear above the staff. Add using `<harmony>` element:

```xml
<harmony>
  <root>
    <root-step>C</root-step>
  </root>
  <kind text="maj7">major-seventh</kind>
</harmony>
<note>
  <!-- note that this chord applies to -->
  <pitch>...</pitch>
  <duration>4</duration>
  <type>quarter</type>
</note>
```

**Important:** 
- `<harmony>` comes **before** the note it applies to
- Add `text="..."` attribute to `<kind>` - this is what MuseScore displays
- After opening in MuseScore, check: **View → Show Chord Symbols** (may be hidden by default)

#### 8.1 Chord Kinds

Common chord types with `text` attribute for MuseScore rendering:

```xml
<kind text="">major</kind>              <!-- C -->
<kind text="m">minor</kind>             <!-- Cm -->
<kind text="aug">augmented</kind>       <!-- Caug -->
<kind text="dim">diminished</kind>      <!-- Cdim -->
<kind text="7">dominant</kind>          <!-- C7 -->
<kind text="maj7">major-seventh</kind>  <!-- Cmaj7 -->
<kind text="m7">minor-seventh</kind>    <!-- Cm7 -->
<kind text="dim7">diminished-seventh</kind>  <!-- Cdim7 -->
<kind text="m7b5">half-diminished</kind>     <!-- Cø7 -->
<kind text="maj9">major-ninth</kind>    <!-- Cmaj9 -->
<kind text="9">dominant-ninth</kind>    <!-- C9 -->
<kind text="sus4">suspended-fourth</kind> <!-- Csus4 -->
<kind text="sus2">suspended-second</kind> <!-- Csus2 -->
```

**Note:** The `text=""` attribute is what displays in MuseScore. The element value is semantic only.

#### 8.2 Root Alterations

```xml
<root>
  <root-step>F</root-step>
  <root-alter>1</root-alter>  <!-- F# -->
</root>
<kind>minor</kind>  <!-- F#m -->
```

Or with flats:

```xml
<root>
  <root-step>B</root-step>
  <root-alter>-1</root-alter>  <!-- Bb -->
</root>
<kind>major-seventh</kind>  <!-- Bbmaj7 -->
```

#### 8.3 Bass Notes (Slash Chords)

For chords like C/E or Dm7/G:

```xml
<harmony>
  <root>
    <root-step>C</root-step>
  </root>
  <kind>major</kind>
  <bass>
    <bass-step>E</bass-step>
    <bass-alter>0</bass-alter>  <!-- optional -->
  </bass>
</harmony>
```

#### 8.4 Complete Example with Chords

```xml
<measure number="1">
  <attributes>...</attributes>
  
  <!-- Cmaj7 chord -->
  <harmony>
    <root>
      <root-step>C</root-step>
    </root>
    <kind text="maj7">major-seventh</kind>
  </harmony>
  <note>
    <pitch>
      <step>C</step>
      <octave>4</octave>
    </pitch>
    <duration>4</duration>
    <type>quarter</type>
  </note>
  
  <!-- Dm7 chord -->
  <harmony>
    <root>
      <root-step>D</root-step>
    </root>
    <kind text="m7">minor-seventh</kind>
  </harmony>
  <note>
    <pitch>
      <step>D</step>
      <octave>4</octave>
    </pitch>
    <duration>4</duration>
    <type>quarter</type>
  </note>
  
  <!-- G7 chord -->
  <harmony>
    <root>
      <root-step>G</root-step>
    </root>
    <kind text="7">dominant</kind>
  </harmony>
  <note>
    <pitch>
      <step>G</step>
      <octave>4</octave>
    </pitch>
    <duration>4</duration>
    <type>quarter</type>
  </note>
  
  <!-- C chord -->
  <harmony>
    <root>
      <root-step>C</root-step>
    </root>
    <kind text="">major</kind>
  </harmony>
  <note>
    <pitch>
      <step>C</step>
      <octave>4</octave>
    </pitch>
    <duration>4</duration>
    <type>quarter</type>
  </note>
</measure>
```

**Result:** A II-V-I progression (Cmaj7 - Dm7 - G7 - C) with chord symbols displayed above the staff.

#### 8.5 Positioning Chords

By default, chord symbols appear above the first note. For chords that span multiple notes or rests:

- Place `<harmony>` before the first note of the chord's duration
- MuseScore automatically positions it above the staff
- No additional positioning needed

---

### 9. Barlines

Barlines separate measures and can indicate different musical structures. By default, MuseScore adds regular barlines between measures automatically, but you can specify different types.

#### 9.1 Basic Barline Structure

```xml
<measure number="1">
  <attributes>...</attributes>
  <note>...</note>
  <barline location="right">
    <bar-style>light-heavy</bar-style>
  </barline>
</measure>
```

**Location options:**
- `left` - At the beginning of the measure
- `right` - At the end of the measure (most common)
- `middle` - Mid-measure (rare, for special notation)

#### 9.2 Barline Types

Common barline styles:

```xml
<!-- Regular (default - not needed to specify) -->
<bar-style>regular</bar-style>

<!-- Double barline (section ending) -->
<bar-style>light-light</bar-style>

<!-- Final barline (end of piece) -->
<bar-style>light-heavy</bar-style>

<!-- Repeat start -->
<bar-style>heavy-light</bar-style>

<!-- Repeat end -->
<bar-style>light-heavy</bar-style>

<!-- Dashed (for continuations) -->
<bar-style>dashed</bar-style>

<!-- Heavy (for major sections) -->
<bar-style>heavy</bar-style>

<!-- None (invisible) -->
<bar-style>none</bar-style>
```

#### 9.3 Repeat Barlines

For repeat signs, add `<repeat>` element:

```xml
<!-- Start repeat (beginning of repeated section) -->
<barline location="left">
  <bar-style>heavy-light</bar-style>
  <repeat direction="forward"/>
</barline>

<!-- End repeat (end of repeated section) -->
<barline location="right">
  <bar-style>light-heavy</bar-style>
  <repeat direction="backward"/>
</barline>
```

**Repeat with times:**
```xml
<barline location="right">
  <bar-style>light-heavy</bar-style>
  <repeat direction="backward" times="2"/>  <!-- play 2 times total -->
</barline>
```

#### 9.4 Complete Examples

**Example 1: Final barline at end of piece**
```xml
<measure number="8">
  <attributes>...</attributes>
  <note>...</note>
  <note>...</note>
  <barline location="right">
    <bar-style>light-heavy</bar-style>
  </barline>
</measure>
```

**Example 2: Section with double barline**
```xml
<measure number="16">
  <attributes>...</attributes>
  <note>...</note>
  <barline location="right">
    <bar-style>light-light</bar-style>
  </barline>
</measure>
```

**Example 3: Simple repeat**
```xml
<!-- Measure 1: start repeat -->
<measure number="1">
  <barline location="left">
    <bar-style>heavy-light</bar-style>
    <repeat direction="forward"/>
  </barline>
  <attributes>...</attributes>
  <note>...</note>
</measure>

<!-- Measure 2 -->
<measure number="2">
  <note>...</note>
</measure>

<!-- Measure 3 -->
<measure number="3">
  <note>...</note>
</measure>

<!-- Measure 4: end repeat -->
<measure number="4">
  <note>...</note>
  <barline location="right">
    <bar-style>light-heavy</bar-style>
    <repeat direction="backward"/>
  </barline>
</measure>
```

**Result:** Measures 1-4 with repeat signs, telling performer to play twice.

#### 9.5 Common Patterns

```
Pattern                 Usage
---------------------------------------------
(none)                  Regular measure - auto-generated
light-light             Section boundary, key/time change
light-heavy             Final barline (end of piece)
heavy-light + forward   Start of repeat section
light-heavy + backward  End of repeat section
dashed                  Measure continuation (rare)
```

#### 9.6 Important Notes

- **Default behavior**: MuseScore adds regular barlines automatically - you don't need to specify them
- **Only specify when different**: Only add `<barline>` when you need something other than regular
- **Right location most common**: `location="right"` is most frequently used
- **Final measure**: Always use `light-heavy` on the last measure of a piece

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

## Text Attachments

To attach text above or below a note or rest, use the `<direction>` element. It must appear **before** the note/rest it applies to.

### Basic Structure

```xml
<direction placement="above">
  <direction-type>
    <words>dolce</words>
  </direction-type>
</direction>
<note>
  <!-- the note this text applies to -->
</note>
```

### Required Elements

- `<direction>` - Container for the text attachment
  - `placement` attribute: `"above"` or `"below"` (position relative to staff)
- `<direction-type>` - Specifies the type of direction
- `<words>` - The actual text to display

### Positioning

**placement attribute:**
- `"above"` - Text appears above the staff
- `"below"` - Text appears below the staff

### Examples

#### Text Above a Note
```xml
<direction placement="above">
  <direction-type>
    <words>molto espressivo</words>
  </direction-type>
</direction>
<note>
  <pitch>
    <step>C</step>
    <octave>5</octave>
  </pitch>
  <duration>4</duration>
  <type>quarter</type>
</note>
```

#### Text Below a Rest
```xml
<direction placement="below">
  <direction-type>
    <words>tacet</words>
  </direction-type>
</direction>
<note>
  <rest/>
  <duration>8</duration>
  <type>half</type>
</note>
```

#### Multiple Text Directions
```xml
<direction placement="above">
  <direction-type>
    <words>p</words>  <!-- piano dynamic -->
  </direction-type>
</direction>
<direction placement="above">
  <direction-type>
    <words>dolce</words>
  </direction-type>
</direction>
<note>
  <pitch>
    <step>E</step>
    <octave>4</octave>
  </pitch>
  <duration>2</duration>
  <type>quarter</type>
</note>
```

### Advanced: Text Formatting

You can add formatting attributes to `<words>`:

```xml
<words font-weight="bold" font-size="12">forte</words>
<words font-style="italic">cantabile</words>
```

**Common attributes:**
- `font-weight`: `"normal"` or `"bold"`
- `font-style`: `"normal"` or `"italic"`
- `font-size`: point size (number)
- `font-family`: font name (e.g., "Times New Roman")

### Order Matters

`<direction>` elements must come **before** the note they apply to, within the measure:

```xml
<measure number="1">
  <attributes>...</attributes>
  
  <!-- Direction comes FIRST -->
  <direction placement="above">
    <direction-type>
      <words>legato</words>
    </direction-type>
  </direction>
  
  <!-- Then the note -->
  <note>...</note>
</measure>
```

---

## Rhythmic and Slash Notation

Rhythmic notation and slash notation are common in lead sheets, jazz charts, and rhythm sections where you want to indicate rhythm patterns or repetitions without specifying exact pitches.

### Slash Notation (Rhythm Slashes)

Slash notation displays rhythm strokes on the staff without specific pitches. Each slash represents a beat or rhythmic duration.

#### Basic Slash Note

```xml
<note>
  <unpitched>
    <display-step>B</display-step>
    <display-octave>4</display-octave>
  </unpitched>
  <duration>1</duration>
  <type>quarter</type>
  <notehead>slash</notehead>
</note>
```

**Key elements:**
- `<unpitched>` - Indicates no specific pitch (replaces `<pitch>`)
- `<display-step>` and `<display-octave>` - Where to draw the slash on the staff (typically middle of staff)
- `<notehead>slash</notehead>` - Makes the note appear as a slash

#### Example: Four Slash Beats in 4/4

```xml
<measure number="1">
  <attributes>
    <divisions>1</divisions>
    <time>
      <beats>4</beats>
      <beat-type>4</beat-type>
    </time>
  </attributes>
  <note>
    <unpitched>
      <display-step>B</display-step>
      <display-octave>4</display-octave>
    </unpitched>
    <duration>1</duration>
    <type>quarter</type>
    <notehead>slash</notehead>
  </note>
  <note>
    <unpitched>
      <display-step>B</display-step>
      <display-octave>4</display-octave>
    </unpitched>
    <duration>1</duration>
    <type>quarter</type>
    <notehead>slash</notehead>
  </note>
  <note>
    <unpitched>
      <display-step>B</display-step>
      <display-octave>4</display-octave>
    </unpitched>
    <duration>1</duration>
    <type>quarter</type>
    <notehead>slash</notehead>
  </note>
  <note>
    <unpitched>
      <display-step>B</display-step>
      <display-octave>4</display-octave>
    </unpitched>
    <duration>1</duration>
    <type>quarter</type>
    <notehead>slash</notehead>
  </note>
</measure>
```

### Measure Repeat Signs

Measure repeat signs indicate that a measure (or measures) should be repeated without rewriting the notation.

#### Single Measure Repeat

Use the `<measure-style>` element within `<attributes>`:

```xml
<measure number="2">
  <attributes>
    <measure-style>
      <measure-repeat type="start">1</measure-repeat>
    </measure-style>
  </attributes>
</measure>
```

**Key elements:**
- `<measure-style>` - Container for measure-level notation styles
- `<measure-repeat>` - Indicates a measure repeat
  - `type` attribute: `"start"` begins the repeat
  - Text content: number of measures to repeat (usually `1` for single measure)

#### Example: Measure with Repeat Sign

```xml
<!-- Measure 1: Original pattern -->
<measure number="1">
  <attributes>
    <divisions>2</divisions>
    <time>
      <beats>4</beats>
      <beat-type>4</beat-type>
    </time>
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
      <step>E</step>
      <octave>4</octave>
    </pitch>
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
  <note>
    <pitch>
      <step>C</step>
      <octave>5</octave>
    </pitch>
    <duration>2</duration>
    <type>quarter</type>
  </note>
</measure>

<!-- Measure 2: Repeat previous measure -->
<measure number="2">
  <attributes>
    <measure-style>
      <measure-repeat type="start">1</measure-repeat>
    </measure-style>
  </attributes>
</measure>
```

**Result:** Measure 2 displays a repeat symbol (%) indicating to repeat measure 1.

#### Multi-Measure Repeat

For repeating multiple measures, use a larger number:

```xml
<measure number="3">
  <attributes>
    <measure-style>
      <measure-repeat type="start">2</measure-repeat>
    </measure-style>
  </attributes>
</measure>
```

This indicates to repeat the previous 2 measures.

### Combining Slash Notation with Chord Symbols

A common use case in lead sheets is slash notation with chord symbols:

```xml
<measure number="1">
  <attributes>
    <divisions>1</divisions>
    <time>
      <beats>4</beats>
      <beat-type>4</beat-type>
    </time>
  </attributes>
  
  <!-- Chord symbol -->
  <harmony>
    <root>
      <root-step>C</root-step>
    </root>
    <kind>major</kind>
  </harmony>
  
  <!-- Four slash beats -->
  <note>
    <unpitched>
      <display-step>B</display-step>
      <display-octave>4</display-octave>
    </unpitched>
    <duration>1</duration>
    <type>quarter</type>
    <notehead>slash</notehead>
  </note>
  <note>
    <unpitched>
      <display-step>B</display-step>
      <display-octave>4</display-octave>
    </unpitched>
    <duration>1</duration>
    <type>quarter</type>
    <notehead>slash</notehead>
  </note>
  <note>
    <unpitched>
      <display-step>B</display-step>
      <display-octave>4</display-octave>
    </unpitched>
    <duration>1</duration>
    <type>quarter</type>
    <notehead>slash</notehead>
  </note>
  <note>
    <unpitched>
      <display-step>B</display-step>
      <display-octave>4</display-octave>
    </unpitched>
    <duration>1</duration>
    <type>quarter</type>
    <notehead>slash</notehead>
  </note>
</measure>
```

**Result:** A measure showing "C" chord symbol with four slash beats underneath.

### Notes on Implementation

1. **Slash positioning:** Use `B4` (treble clef) or `D3` (bass clef) for centered slashes on the staff
2. **Measure repeats:** Only require the `<measure-style>` element; no notes needed
3. **Empty measures:** When using measure repeats, the measure can be empty (only attributes)
4. **Duration still matters:** Even with slashes, duration values must still add up correctly to fill the measure

---

## Summary: What You Need to Implement

1. **Header generation** - part-list with IDs and names
2. **Attributes** - divisions, key (fifths), time, clef
3. **Note conversion** - pitch (step/alter/octave), duration, type
4. **Rest conversion** - same as notes but with `<rest/>`
5. **Tie support** - add `<tie>` and `<tied>` elements
6. **Dotted notes** - add `<dot/>` element
7. **Chord symbols** (optional) - add `<harmony>` before notes with root, kind, and optional bass
8. **Barlines** (optional) - add `<barline>` with style for final bars, repeats, and sections
9. **Text attachments** (optional) - add `<direction>` with `<words>` for text above/below notes
10. **Slash notation** (optional) - use `<unpitched>` with `<notehead>slash</notehead>` for rhythm slashes
11. **Measure repeats** (optional) - use `<measure-style>` with `<measure-repeat>` for repeat signs

**That's it.** MuseScore renders everything else beautifully.
