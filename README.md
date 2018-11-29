# Project: OCR (Optical Character Recognition) 

![image](figs/intro.png)

### [Full Project Description](doc/project4_desc.md)

Term: Fall 2018

+ **Group 4**
+ **Team members**
	+ Tao Han
	+ Zhirong Li
	+ Atishay Sehgal
	+ Lujia Wang
	+ Jiaqian Yu

+ **Project summary**: In this project, we created an OCR post-processing procedure to enhance Tesseract OCR output in two steps. We split the text files by group and for each group, as a first step, classify each word using Cross-validated Support Vector Machines as erroneous and correct. After detecting the erroneous words, we perform error correction using probability scoring on them and finally evaluate our model performance using precision and recall. 
	
+ **Contribution statement**: ([default](doc/a_note_on_contributions.md))
	+ Tao Han:
	+ Zhirong Li: Worked on performance measure in word level and character level.
	+ Atishay Sehgal: Worked on Error Detection - Contributed to Feature & Label Extraction, implemented cross-validated 			       SVM. Implemented improvements in computational speed of SVM. Provided ideas for error correction 			  speed improvements.
	+ Lujia Wang: Worked on Error Detection - Worked on feature construction, training data set generation and SVM training. Assisted Jiaqian handling some computation on Error Correction. Helped with the presentation slides and discussed with teammates.
	+ Jiaqian Yu: Worked on Error Correction based on C3 - Contributed to all the processes: find proposed candidates, gnerate confusion matrix, calcualte the probability and find out the best "correction". Helped with the PPT and discussed with other teammates.

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
