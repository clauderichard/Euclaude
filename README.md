# Euclaude
Euclaude = Euclid + Claude.  Haskell program for manipulating a triangle and displaying its special centers, circles, etc.

# Installing and running
You will need to install Haskell on your system before you can run Euclaude.
When you have Haskell (specifically ghc) installed, open a command line from inside the project file
(in the directory where the .hs files are), and run the following command:
%> ghc main
Then you can run the newly created main.exe.

When the program is running, you can grab each of the triangle's three corners with your mouse and drag it around and see how the
rest of the geometrical features

# Switching between demos
If you open the file Demos.hs you will see that mainDemo is assigned the value demoCentroid.
(The centroid is just the intersection of the three medians of the triangle.
Median means a line from a corner to the midpoint of the opposite side.)
You can change this to another uncommented demo found in that file, and then recompile main.exe to get that demo.
