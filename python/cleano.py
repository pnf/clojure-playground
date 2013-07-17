# Loops over all of the gutenberg text files in /var/bigdisk/gutenberg/gt_text
# Extracting unique texts, and copies them to ./gt_raw
# Removes header and footer information - leaving just the text, ready for
# statistical processing
#
# Usage: Be sure to change these paths to point to the relevant directories on your system

import string
import os
import gc
import shutil

# Empty the output directory
outputdir = "/Users/pnf/dev/learn-clojure/tmp"
inputdir = "/Users/pnf/dev/learn-clojure/gut"

# The logic for keeping a file based on its name is put
# into a name to improve readability

# fname = Name of file without path or file extension
def keep_file(fname):
    # filter out readme, info, notes, etc
    if (fname.lower().find("readme") != -1):
        return False
    if  (fname.find(".zip.info") != -1):
        return False
    if (fname.find("pnote") != -1 ):
        return False
    # Filter out the Human Genome
    if (len(fname)==4):
        try:
            n = int(fname)
            if (n>=2201 and n<=2224):
                print "*** Genome skipped:",n
                return False    # Human Genome
        except ValueError:
            n=0  # dummy line

    # Looks good => keep this file
    return True

# Recursively walk the entire directory tree finding all .txt files which
# are not in old sub-directories. readme.txt files are also skipped.

for f in os.listdir(outputdir):
    fpath = os.path.join(outputdir, f)
    try:
        if (os.path.isfile(fpath)):
            os.unlink(fpath)
    except Exception, e:
        print e

for dirname, dirnames, filenames in os.walk(inputdir):
    if (dirname.find('old') == -1  and
        dirname.find('-h') == -1 ) :
        # some files are duplicates, remove these and only copy a single copy
        # The -8 suffix takes priority (8 bit ISO-8859-1) over the
        # files with no suffix or -1 suffix (simple ASCII)
        # also remove auxiliaries: Names contain pnote or .zip.info
        flist = []
        flist_toremove = []
        for fname in filenames:
            fbase, fext = os.path.splitext(fname)
            if ( fext == '.txt'):
                if (keep_file(fbase)):
                    flist.append(fname)
                    if (fname.endswith("-8.txt") ):
                        # -8 takes priority => remove any duplicates
                        flist_toremove.append( fname[: (len(fname)-6)] + ".txt" )
                        flist_toremove.append( fname[: (len(fname)-6)] + "-0.txt" )

        flist_to_proc = [i for i in flist if i not in flist_toremove]

        # flist_to_proc now contains the files to copy
        # loop over them, copying line-by-line
        # Check for header/footer markers - file is skipped if header marker is missing
        for f in flist_to_proc:
            infile = os.path.join(dirname, f)
            outfile = os.path.join(outputdir, f)

            bCopying = False
            # print "Working on", infile, outfile

            extra = ""

            for line in open(infile):
                if (not bCopying):
                    if (line.startswith("Language:") or line.startswith("Author:")):
                        extra = extra + line
                    if (line.startswith("*** START OF ") or
                        line.startswith("*END*THE SMALL PRINT")):
                        fout = open(outfile, "w")
                        fout.write(extra)
                        print "Copying: " + f
                        bCopying = True

                elif (bCopying):
                    if (line.startswith("*** END OF THIS PROJECT GUTENBERG EBOOK")):
                        fout.close()
                        bCopying = False
                    else:
                        fout.write(line)
