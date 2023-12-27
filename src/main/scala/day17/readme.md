# Solving this problem

This code ends up solving this simulation by using some magic numbers. Where did I get these numbers? here's the process.

Hypothesis: This system repeats at a particular point. we need to find that point

1. Write out the column data for a large enough simulation to a file 20220 was plenty large enough, but even 2022 has the looping point, if you look hard enough
2. Look for a repeated entries for several rows -- maybe 10.
3. Once you find a pair that repeats at a recular cacdense, find the number of columns between each iteration -- also just look at its surrounding, is it mostly the same?
4. Noce you find the cycle length, start looking for the start of the cycle -- it's not at the first line, but somewhere past that. Look for where the pattern starts -- in particular what shape is really critical for this process to work, and look for where that piece fits in.
5. Do some math to figure out how to calculate an arbitrary length -- basically, it's a number of full cycles, plus some emulated steps to cover whatever gap is left

## But if we need to re-calculate

We'd probably want to set up some code to stop the simulation at points, and look for a repeated sequence of 10 rows (in this case, 10 consecutative integers) and then check when it repeats. There might be a better way, but I'm nsure what it is right now. 

Alternative: we might also want to keep a history of what shape landed / was placed at what column, and the corresponding jet stream index, and look for repeats in that, which should be more indicative and easier to programmatically find.