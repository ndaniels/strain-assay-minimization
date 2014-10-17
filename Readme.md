#min-barcodes
This is a Haskell program for determining the minimal set of assays needed
to distinguish all samples from a set of molecular barcode samples.

###Compilation
You must have the GHC Haskell compiler installed

 - Change directory to the directory containing the source code, and run:

        cabal install


###Usage
There is one command-line argument: the sample file (see File Format below).

`./min-barcodes input.csv`

###File format
The input file *must* be a CSV (comma-separated value) file as follows:

 - No header
 - One row per sample
 - First field is the name of the sample
 - Next 24 fields are each a single character, representing the 24-assay
   molecular barcode.
   
If different-length barcodes are needed, for now, change line 36 of Barcode.hs
(the one that reads `| V.length v == 25 = Barcode <$>`)
so the harcoded 25 is another number (1 more than the number of barcodes).
Then, recompile.
In the future, this will be parameterized.

###Assumptions
 - The barcodes are all biallelic, except for 'X' and 'N' entries
 - Duplicate samples (complete duplicates) will be ignored.
 - Assays are numbered starting from 1 (1-indexed) in both input and output.
 - Samples with >1 N, >5 X, or >5 (combined X and N) are removed.
   This assumption exists on line 53 of Barcode.hs, which reads
   `where check b = ns <= 1 && xs + ns <= 5`
   If you need to change this, of course you will have to recompile.
   
###Analyzing existing assay sets
If you wish to use an existing assay set in order to see if it uniquely
identifies all samples in a data set, you must do this interactively in the
ghci interpreter.

From the command line in the source code directory, run:
        ghc --make -O3 Main.hs -o min-barcodes
        ghci

Simply put the following in (substitute your list of assay numbers):

        :load Barcode
        samples <- parseBarcode "2013-test.csv"
        let samples' = uniquify $ V.toList $ V.filter validBarcode samples
        let ids = map (\x -> x - 1) [1, 9, 10, 11, 12, 14, 15, 18]
        putStrLn $ unlines $ reportDuplicates samples' ids

Note that the (\x -> x - 1) is a lambda function that subtracts one from every
index. This is because internally, indices are 0-indexed, but for input and
output, indices are 1-indexed.