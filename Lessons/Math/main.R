require(pacman)
p_load_gh("galacticpolymath/GPpub")

# STEP 1: Compile alignment data ------------------------------------------
# compile the alignment matrix into a tidy tibble & output a JSON

# Import the lesson alignment matrix from the standards subfolder
f<-"standards/Lesson 1_alignmentMatrix.xlsx"
# aggregate alignment matrix notes and codes, merge with the alignments master
# document from our standardX package
compiled<-compileAlignment(f)


# STEP 2: Output GP Learning Epaulette ------------------------------------
# output subject percentage breakdown that will go at the top of the 
# GP Sensible Lesson Plan
learningEpaulette(compiled,targetSubj="math")


# STEP 3: Output GP Learning Chart ----------------------------------------
# output the custom windrose chart that will go in the standards section of 
# the GP Sensible Lesson Plan
learningChart(compiled,"math","Females Singing to Be Heard: Challenging assumptions with data visualization",fileName="FemalesSing_LearningChart")


# STEP 4: Output lesson chunking visuals ----------------------------------



