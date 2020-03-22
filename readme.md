# OANC to VRT

This repository contains an R script for converting the Open American National Corpus (available <a href="http://www.anc.org/data/oanc/" target="_blank">here</a>) to VRT format for use in the Corpus Workbench (CWB, <a href="cwb.sourceforge.net/" target="_blank">cwb.sourceforge.net/</a>).

**Update March 2020**: I've made separate scripts for the spoken and written parts to retain the turn information in the spoken part.


## new scripts: OANC_to_VRT_spoken.R and OANC_to_VRT_written.R

These scripts convert the spoken and written part of OANC to VRT separately. Lemma and POS annotations as well as sentence annotations and (some) metadata are retained. For the spoken part, the turn annotation is retained as well. As the Hepple POS files are not complete in the current GrAF distribution of OANC, they are taken from the first (non-GrAF) release of OANC, also available at the link mentioned above. This means that to use the script, you need both versions, GrAF and the old non-GrAF XML version. You might have to change the directory paths in the script to make it work.

The data can then be encoded to CWB format using the following commands (replace path/to etc. by the paths to your CWB data directory and the location of the VRT file, and change the registry path if your CWB registry is located elsewhere):

````
# spoken:
cwb-encode -s -c utf8 -x -v -d /corpora/data/oancspoken -f "/Volumes/TOSHIBA EXT/Corpora/OANC-GrAF/oanc_spoken.vrt" -R /usr/local/share/cwb/registry/oancspoken -P lemma -P pos -P anchor1 -P anchor2 -S corpus -S text:0+id+modality+genre+file -S s -S turn:0+id+sex+age

cwb-makeall -r /usr/local/share/cwb/registry/ oancspoken


# written:
cwb-encode -s -c utf8 -x -v -d /corpora/data/oancwritten -f "/Volumes/TOSHIBA EXT/Corpora/OANC-GrAF/oanc_written.vrt" -R /usr/local/share/cwb/registry/oancwritten -P lemma -P pos -P anchor1 -P anchor2 -S corpus -S text:0+id+modality+genre+file -S s

cwb-makeall -r /usr/local/share/cwb/registry/ oancwritten

````



## old script: OANC_to_VRT.R

This script converts the entire GrAF version of OANC to VRT, dicarding all annotations except lemma, POS, and (some) metadata. It also discards all texts for which Hepple POS files are missing in the current GrAF version. Some postprocessing is necessary (see below), which is not the case for the new scripts, which already include these postprocessing steps.

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

