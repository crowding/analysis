function mat2ascii(infile, outfile)

s = load(infile);
save(outfile, '-struct', 's', '-v6', '-ascii');

end
