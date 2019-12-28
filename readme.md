# OANC to VRT

This repository contains an R script for converting the Open American National Corpus (available <a href="http://www.anc.org/data/oanc/" target="_blank">here</a>) to VRT format for use in the Corpus Workbench (CWB, <a href="cwb.sourceforge.net/" target="_blank">cwb.sourceforge.net/</a>).

When using the R script, R's working directory should be set to the head directory OANC-GRaF/. The output of the script a vrt file called oanc.vrt, which should be postprcessed as follows:

````
# replace:
<s>.*\n by <s>\n
</s>.*\n by </s.>\n
<\t by \\<\t
>\t by \\>\t
& by \\&
````

The data can then be encoded to CWB format using the following commands (replace path/to etc. by the paths to your CWB data directory and the location of the VRT file, and change the registry path if your CWB registry is located elsewhere):

`````
cwb-encode -s -c utf8 -x -v -d path/to/cwb-data-directory/oanc -f path/to/vrtfile/oanc.vrt -R /usr/local/share/cwb/registry/oanc -P lemma -P pos -P anchor1 -P anchor2 -S text:0+id+modality+genre+file -S s

cwb-makeall -r /usr/local/share/cwb/registry/ oanc

`````

