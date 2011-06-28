function tom6(infile, outfile)

s = load(infile);
save(outfile, '-struct', 's', '-v6');

end
