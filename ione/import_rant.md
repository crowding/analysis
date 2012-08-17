=MATLAB doesn't let you call your data by name and can't read ASCII
data out of a wet paper bag.

I'm working with someone, and they asked. In the interests of maximum
interoperability, I gave them a .csv data file.

It looked something like this:

Simple enough, right? For example, of you wanted to do a scatterplot of "threshold" versus

X = csv(
with(X, plot(threshold, ))

Now the CSV data file is 

A bit later I got back from them a graph, with some MATLAB code that went like this:

```matlab

```

Well, this is fairly typical for random matlab code. Give people
matrices and they use explicit numeric indices for everything. You
have to open up the CSV file and compare its columns against the data
to see what variable they think they're plotting (and they weren't,
acctually, plotting what they thought.)

One of the very useful features of R is that you can assign names
almost everywhere you would use a number. So, you never have to worry
about whether column 4 is "target_spacing" or something else.

For example, `read.csv` reads you a rectangular grid of data like you
expect, but it also reads the header and assigns columnnames and also
reads the header row ans assigns it to 'rownames'. So in order to get
the target spacing on the fourth row, you just write
`data[4,'target_spacing']`

So let's say you have some nice rectilinear data, like this
cross-tabulation of hair color, and eye color, and sex in a group of
students:

```R
> data(HairEyeColor)
> HairEyeColor
, , Sex = Male

       Eye
Hair    Brown Blue Hazel Green
  Black    32   11    10     3
  Brown    53   50    25    15
  Red      10   10     7     7
  Blond     3   30     5     8

, , Sex = Female

       Eye
Hair    Brown Blue Hazel Green
  Black    36    9     5     2
  Brown    66   34    29    14
  Red      16    7     7     7
  Blond     4   64     5     8

> 
```

This is just a 3-D array, like Matlab's 3-D arrays (Interestingly,
Matlab only added multi-D arrays after someone got fed up with the
lack of them and went off to write an array package for
Python. Numpy. And as an aside, Numpy and R have consistent rules for
indexing in N-D (where N can be 1, 2, 3, or more), while MATLAB
forgets about 1 dimensional arrays entirely and as for consistency,
[utterly screwed it up](/2009/08/01/for-your-thoughts-a-question-in-three-parts/). 

Ahem. As I was saying, unlike an array in Matlab, arrays in R can have
nice, human-interpretable identifiers attached to attached to their
rows, columns, and slices. You can see them in the printout above, or
get and set them explicitly with `dimnames`:

```R
> dimnames(HairEyeColor)
$Hair
[1] "Black" "Brown" "Red"   "Blond"

$Eye
[1] "Brown" "Blue"  "Hazel" "Green"

$Sex
[1] "Male"   "Female"
```

This allows you to access elements by name, not number. So if you want
to slice the count of blond, brown-eyed people in this sample, you can
just say:

```
> HairEyeColor['Blond', 'Brown',]
  Male Female 
     3      4 
```

See, that's the same as writing `HairEyeColor[4, 1,]`, only you can
actually see what it's trying to accomplish.

Now, I wish that you would be able to go a step further and write
`HairEyeColor[Eye='Brown',Hair='Blue',]', and not worry about which
order the dimensions come in, but R's not perfect. Just
useful. Actually, you can do that sort of thing with PANDAS, a Ptyhon
library billing itself as "(R's data.frame on
steroids)[http://www.youtube.com/watch?v=w26x-z-BdWQ]" Meanwhile, if
you pay an additional tithe to the Mathworks, you can get the
Statistics toolbox, which includes a "dataset" class that is more or
less R's data.frame on blood thinners.

Anyway, if you ever ask me to remember that "Female" is 1 (in this
dataset) and "Hazel" is 3, well, look, remembering arbitrary
correspondences between names and numbers is something humans are just
really bad at and computers are very good at, OK? If you're writing
analysis scripts and you fond yourself flipping back to the speadcheet
to count columns... just don't. Why would you do a job the computer
should be doing for you?

Okay. Before I had the first gin and tonic and decided to cover a
topic or two on the syllabus of Stuff That's In Every Useful
Programming Language Except MATLAB 101, I had this script someone sent
me, that read in some data from a CSV file I'd sent them. And they
were using numeric indices into the data because they had just loaded
in the data as an array without the column names. But you ought to be
able to load each column of data as a separate field of a struct, and
refer to them by name that way, you know, and that'd be doing pretty
good for MATLAB. So I was planning to tweak this code and send it
bhack with a note about 'here's a nice way to do it better' (this
person teaches a course on MATLAB for scientists, you see, so I want
to slightly reduce the fucking brain damage that gets propagated out
into the academic world.)

All you'd have to do is, instead of reading a CSV as a matrix, use the
function that reads from a CSV file and uses the column headings to
assign the fields in a struct. You know, that function. The one that
does the single bleeding obvious thing to to with CSV files, the way
that `read.csv` does it for you automatically in R. I mean for all I
rant about it, people get work done with MATLAB. It's just impossible
that Matlab can't read a CSV file usefully. Right?

Well, let's try it.

The first thing I find is `csvread`. Aside from being deprecated
according to the documentation, there's another problem in that if
only reads numeric data. Some of them have data like a human
observer's initials, or categorical data that's better expressed to
humans lith labels like "left" or "right" rather than trying to
remember which one of those correspond to zero and 1. (R has "factors"
to handle categorically enumerable data, while MATLAB has bupkis,
unless you shell ou for the Stats toolbox, which has
factors-on-beta-blockers.) So, `csvread` can't cut it, because it only
handles numeric data. Same problem with `dimread`.

Next up we have `xlsread`. That's what my collaborator used to start
out with. Maybe it has an option to get the column names. Well, it
won't even read the file on my Macbook. Nor the Linux cluster we have
in our lab. Ah, see, `xlsread` only reads a CSV file if it can farm
its work out via a fucking COM call to an installed copy of Microsoft
Excel, and it only knows how to do that on Windows.

There's `csv2struct` in the File Exchenge, but it is just a wrapper of
`xlsread` so nope.

CSV, man, one of the world's most ubiquitous, plain-text,
human-readable file formats.

What next? There's `importdata` which wants to DWIM the reading of
tabular data. Except it doesn't handle the quoting conventions in CSV
files. It detects that there's a header row but it doesn't actually
give me the field names--why? Some experimentation reveals that it's
just really unreliable at moxing numeric with string data. Here's how
'importdata' mangles a perfectly straightforward file:

```matlab
>> type testfile.txt

Height,Width,Depth,Weight,Name,Age,Speed
95.01,76.21,61.54,40.57,Charlie,20.28,1.53
23.11,45.65,79.19,93.55,Echo,19.87,74.68
60.68,1.85,92.18,91.69,Delta,60.38,44.51
48.60,82.14,73.82,41.03,Alpha,27.22,93.18
89.13,44.47,17.63,89.36,Romeo,19.88,46.60
>> [a delim nhreaderlines] = importdata('testfile.txt')
a = 
        data: [5x2 double]
    textdata: {6x7 cell}
delim =
,
nhreaderlines =
     1
>> a.textdata
ans = 
  Columns 1 through 6
    'Height'    'Width'    'Depth'    'Weight'    'Name'       'Age'
    '95.01'     '76.21'    '61.54'    '40.57'     'Charlie'    ''   
    '23.11'     '45.65'    '79.19'    '93.55'     'Echo'       ''   
    '60.68'     '1.85'     '92.18'    '91.69'     'Delta'      ''   
    '48.60'     '82.14'    '73.82'    '41.03'     'Alpha'      ''   
    '89.13'     '44.47'    '17.63'    '89.36'     'Romeo'      ''   
  Column 7
    'Speed'
    ''     
    ''     
    ''     
    ''     
    ''     
>> a.data
ans =
   20.2800    1.5300
   19.8700   74.6800
   60.3800   44.5100
   27.2200   93.1800
   19.8800   46.6000
```

So, there's a header row but it doesn't give back column names, the text data is full of perfectly reasonable numbers but 

because the string data is put in double
quotes like a proper CSV file and importdata can't handle quotes.

There's the "Data Import Wizard" but it's just a wrapper for
`importdata` so no.

The documentation for "importing data" is no help, it says to build
something out of `textscan` that's specific to this exact file.


Well, I try
to hack it:



The function that used `xlsread`

Of course they were actually using the wrong I figured I
would fix the code, politely show them show them how to, and remind
them that keeping track of the correspondences between arbitrary names
and integers is something computers are really good at and humans are
really bad at, so use the facilities (structs0 that MATLAB provides
for that purpose.

Finally I end up at the "Import Data Wizard," `uiimport.` This purports
to be able to generate MATLAB code that will be able to replicate my
choices. It recognizes the data Except the options for "create structure field names" are all
grayed out. Who the hell knows why. Maybe because I'm not on Windows.

