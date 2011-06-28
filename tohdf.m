function tohdf(infile, outfile)

s = load(infile);
hdf5write(outfile, '/', s);

end
