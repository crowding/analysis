function toascii(infile, outfile)

s = load(infile);
save(outfile, '-struct', 's', '-ascii');

end
